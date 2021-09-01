//! Sign & magnitude integer representation

use std::convert::TryInto;
use std::fmt;

use crate::const_eval::{errors::ErrorKind, ConstError};

/// Constant Integer representation
///
/// # Note
///
/// `PartialEq` and `Eq` only test for bitwise equality of the structure,
/// and not sign + magnitude equality.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstInt {
    /// invariant: must be representable with the given `sign` and `magnitude`
    magnitude: u64,
    sign: Sign,
    width: Width,
}

impl ConstInt {
    /// Constructs a new integer constant from an unsigned value
    ///
    /// ## Parameters
    /// - `value`: The unsigned value of the corresponding integer constant
    /// - `allow_64bit_values`: If 64-bit values can be constructed from applying
    ///   operations to this integer constant
    pub fn from_unsigned(value: u64, allow_64bit_ops: bool) -> Result<Self, ConstError> {
        let width = if allow_64bit_ops {
            // Allow 64-bit operations, any value is allowed
            Width::As64
        } else {
            // Apply only 32-bit operations
            if value > u64::from(u32::MAX) {
                // Already overflowing
                return Err(ConstError::without_span(ErrorKind::IntOverflow));
            }

            Width::As32
        };

        Ok(Self {
            sign: Sign::Positive,
            magnitude: value,
            width,
        })
    }

    /// Constructs a new integer constant from a signed floating point value
    ///
    /// ## Parameters
    /// - `value`: The signed floating point value of the corresponding integer constant
    /// - `allow_64bit_values`: If 64-bit values can be constructed from applying
    ///   operations to this integer constant
    pub fn from_signed_real(value: f64, allow_64bit_ops: bool) -> Result<Self, ConstError> {
        let width = if allow_64bit_ops {
            // Allow 64-bit operations, check against u64 bounds
            if (value.is_sign_negative() && value < i64::MIN as f64)
                || (value.is_sign_positive() && value > u64::MAX as f64)
            {
                // Already overflowing
                return Err(ConstError::without_span(ErrorKind::IntOverflow));
            }

            Width::As64
        } else {
            // Apply only 32-bit operations
            if (value.is_sign_negative() && value < f64::from(i32::MIN))
                || (value.is_sign_positive() && value > f64::from(u32::MAX))
            {
                // Already overflowing
                return Err(ConstError::without_span(ErrorKind::IntOverflow));
            }

            Width::As32
        };

        let (magnitude, sign) = {
            let magnitude = value.abs().trunc() as u64;

            if magnitude == 0 {
                // Always +0
                (0, Sign::Positive)
            } else {
                let sign = if value.is_sign_negative() {
                    Sign::Negative
                } else {
                    Sign::Positive
                };

                (magnitude, sign)
            }
        };

        Ok(Self {
            magnitude,
            sign,
            width,
        })
    }

    /// Converts the `ConstInt` into the corresponding `u32` value.
    ///
    /// ## Returns
    /// Returns `Some(u32)` if the `ConstInt` is a positive integer (including zero)
    /// and is actually representable as a u32, or `None` otherwise.
    pub fn into_u32(self) -> Option<u32> {
        match self.sign {
            Sign::Positive => self.magnitude.try_into().ok(),
            Sign::Negative => None,
        }
    }

    /// Converts the `ConstInt` into the corresponding `u64` value.
    ///
    /// ## Returns
    /// Returns `Some(u64)` if the `ConstInt` is a positive integer (including zero),
    /// or `None` otherwise.
    pub fn into_u64(self) -> Option<u64> {
        match self.sign {
            Sign::Positive => Some(self.magnitude),
            Sign::Negative => None,
        }
    }

    /// Converts the `ConstInt` into the corresponding `f64` value.
    pub fn into_f64(self) -> f64 {
        match self.sign {
            Sign::Positive => self.magnitude as f64,
            Sign::Negative => -(self.magnitude as f64),
        }
    }

    /// Returns `true` if the integer has a negative sign, or
    /// `false` if the integer has a positive sign or is zero.
    pub fn is_negative(self) -> bool {
        matches!(self.sign, Sign::Negative)
    }

    /// Returns `true` if the integer has a positive sign or is zero, or
    /// `false` if the integer has a negative sign.
    #[allow(dead_code)] // symmetric with `is_negative`
    pub fn is_positive(self) -> bool {
        matches!(self.sign, Sign::Positive)
    }

    /// Checked integer addition.
    /// Computes `self + rhs`, returning `Err(ConstError::IntOverflow)` if overflow occurred.
    pub fn checked_add(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        // Potential signs:
        // lhs rhs    magnitude op
        // +   +   => +
        // -   +   => -
        // +   -   => -
        // -   -   => +
        let (value, new_sign) = if self.sign == rhs.sign {
            // Sign is the same as the original
            (self.magnitude.checked_add(rhs.magnitude), self.sign)
        } else {
            // Sign can change
            let (magnitude, sign) = {
                let (magnitude, wrapped) = self.magnitude.overflowing_sub(rhs.magnitude);

                if wrapped {
                    // Undo two's compliment
                    // Flip the sign
                    (!magnitude + 1, self.sign.negate())
                } else {
                    (magnitude, self.sign)
                }
            };

            (Some(magnitude), sign)
        };

        Self::check_overflow(value, new_sign, effective_width)
    }

    /// Checked integer subtraction.
    /// Computes `self - rhs`, returning `Err(ConstError::IntOverflow)` if overflow occurred.
    pub fn checked_sub(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        // Reuse addition code
        self.checked_add(rhs.unchecked_negate())
    }

    /// Checked integer multiplication.
    /// Computes `self * rhs`, returning `Err(ConstError::IntOverflow)` if overflow occurred.
    pub fn checked_mul(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        // Potential signs:
        // lhs rhs    final sign
        // +   +   => +
        // -   +   => -
        // +   -   => -
        // -   -   => +

        // Magnitude is unaffected by the initial signs
        let value = self.magnitude.checked_mul(rhs.magnitude);

        let new_sign = if self.sign == rhs.sign {
            // Will always be positive
            Sign::Positive
        } else {
            // Will always be negative
            Sign::Negative
        };

        Self::check_overflow(value, new_sign, effective_width)
    }

    /// Checked integer division.
    /// Computes `self div rhs`, returning `Err(ConstError::DivByZero)` if `rhs == 0`.
    pub fn checked_div(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        // Potential signs:
        // lhs rhs    final sign
        // +   +   => +
        // -   +   => -
        // +   -   => -
        // -   -   => +

        // Magnitude is unaffected by the initial signs
        let value = self
            .magnitude
            .checked_div(rhs.magnitude)
            .ok_or_else(|| ConstError::without_span(ErrorKind::DivByZero))?;

        let new_sign = if self.sign == rhs.sign {
            // Will always be positive
            Sign::Positive
        } else {
            // Will always be negative
            Sign::Negative
        };

        Self::check_overflow(Some(value), new_sign, effective_width)
    }

    /// Checked integer modulus (modulus with floored division).
    /// Computes `self mod rhs`, returning `Err(ConstError::DivByZero)` if `rhs == 0`.
    pub fn checked_mod(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        // This is close to a rem_euclid, but the resultant takes the sign of the rhs
        let remainder = self
            .magnitude
            .checked_rem(rhs.magnitude)
            .ok_or_else(|| ConstError::without_span(ErrorKind::DivByZero))?;

        // Adjust into the correct signage
        let (modulus, new_sign) = if remainder == 0 {
            // Always +0
            (0, Sign::Positive)
        } else {
            // Adjust into the right quadrant
            let modulus = if self.sign == rhs.sign {
                // Stays the same
                remainder
            } else {
                // Opposite way!
                rhs.magnitude - remainder
            };

            // Takes the sign of the 2nd operand
            (modulus, rhs.sign)
        };

        Self::check_overflow(Some(modulus), new_sign, effective_width)
    }

    /// Checked integer remainder.
    /// Computes `self rem rhs`, returning `Err(ConstError::DivByZero)` if `rhs == 0`.
    pub fn checked_rem(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        let remainder = self
            .magnitude
            .checked_rem(rhs.magnitude)
            .ok_or_else(|| ConstError::without_span(ErrorKind::DivByZero))?;

        // Adjust into the correct signage
        let (remainder, new_sign) = if remainder == 0 {
            // Always +0
            (0, Sign::Positive)
        } else {
            // Takes the sign of the 1st operand
            (remainder, self.sign)
        };

        Self::check_overflow(Some(remainder), new_sign, effective_width)
    }

    /// Checked integer exponentiation.
    /// Computes `self ** rhs`,
    /// returning `Err(ConstError::NegativeIntExp)` if `rhs` is negative.
    /// or returning `Err(ConstError::IntOverflow)` if overflow occurred.
    pub fn checked_pow(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        if rhs.is_negative() {
            return Err(ConstError::without_span(ErrorKind::NegativeIntExp));
        }

        let exp = rhs
            .magnitude
            .try_into()
            .map_err(|_| ConstError::without_span(ErrorKind::IntOverflow))?;

        let value = self
            .magnitude
            .checked_pow(exp)
            .ok_or_else(|| ConstError::without_span(ErrorKind::IntOverflow))?;

        // Adopts the sign of the base
        let new_sign = self.sign;

        Self::check_overflow(Some(value), new_sign, effective_width)
    }

    /// Performs the bitwise "and" operation.
    ///
    /// Implicitly transforms the integer into the unsigned variant
    pub fn and(self, rhs: ConstInt) -> ConstInt {
        // Apply the bit-and operation
        let bits = self.into_bits() & rhs.into_bits();

        // Truncate to the appropriate width
        let bits = match self.width {
            Width::As32 => bits & 0xFFFF_FFFF,
            Width::As64 => bits,
        };

        // Always an unsigned integer, and always representable
        ConstInt {
            magnitude: bits,
            sign: Sign::Positive,
            width: self.width,
        }
    }

    /// Performs the bitwise "or" operation.
    ///
    /// Implicitly transforms the integer into the unsigned variant
    pub fn or(self, rhs: ConstInt) -> ConstInt {
        // Apply the bit-or operation
        let bits = self.into_bits() | rhs.into_bits();

        // Truncate to the appropriate width
        let bits = match self.width {
            Width::As32 => bits & 0xFFFF_FFFF,
            Width::As64 => bits,
        };

        // Always an unsigned integer, and always representable
        ConstInt {
            magnitude: bits,
            sign: Sign::Positive,
            width: self.width,
        }
    }

    /// Performs the bitwise "xor" operation.
    ///
    /// Implicitly transforms the integer into the unsigned variant
    pub fn xor(self, rhs: ConstInt) -> ConstInt {
        // Apply the bit-xor operation
        let bits = self.into_bits() ^ rhs.into_bits();

        // Truncate to the appropriate width
        let bits = match self.width {
            Width::As32 => bits & 0xFFFF_FFFF,
            Width::As64 => bits,
        };

        // Always an unsigned integer, and always representable
        ConstInt {
            magnitude: bits,
            sign: Sign::Positive,
            width: self.width,
        }
    }

    /// Performs the bitwise negation operation.
    ///
    /// Implicitly transforms the integer into the unsigned variant.
    pub fn not(self) -> ConstInt {
        // Apply the not operation
        let bits = match self.width {
            Width::As32 => !self.into_bits() & 0xFFFF_FFFF,
            Width::As64 => !self.into_bits(),
        };

        // Always an unsigned integer, and always representable
        ConstInt {
            magnitude: bits,
            sign: Sign::Positive,
            width: self.width,
        }
    }

    /// Checked integer shift left.
    /// Computes `self shl rhs`,
    /// returning `Err(ConstError::NegativeIntShift)` if `rhs` is negative,
    /// or `Err(ConstError::IntOverflow)` if overflow occurred.
    ///
    /// Implicitly transforms the integer into the unsigned variant.
    pub fn checked_shl(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        if rhs.is_negative() {
            return Err(ConstError::without_span(ErrorKind::NegativeIntShift));
        }

        let shift_amount: u32 = {
            let shift_amount = rhs
                .into_u64()
                .ok_or_else(|| ConstError::without_span(ErrorKind::IntOverflow))?;

            // Mask the shift amount depending on the effective integer width
            if effective_width == Width::As64 {
                (shift_amount % 64) as u32
            } else {
                (shift_amount % 32) as u32
            }
        };

        // Since bitshifts are width-sensitive, mask appropriately
        let bits = match self.width {
            Width::As32 => self.into_bits() & 0xFFFF_FFFF,
            Width::As64 => self.into_bits(),
        };

        let bits = bits
            .checked_shl(shift_amount)
            .ok_or_else(|| ConstError::without_span(ErrorKind::IntOverflow))?;

        // Always an unsigned integer
        let new_sign = Sign::Positive;

        Self::check_overflow(Some(bits), new_sign, effective_width)
    }

    /// Checked integer shift right.
    /// Computes `self shr rhs`,
    /// returning `Err(ConstError::NegativeIntShift)` if `rhs` is negative,
    /// or `Err(ConstError::IntOverflow)` if overflow occurred.
    ///
    /// Implicitly transforms the integer into the unsigned variant.
    pub fn checked_shr(self, rhs: ConstInt) -> Result<ConstInt, ConstError> {
        let effective_width = Self::effective_width(self.width, rhs.width);

        if rhs.is_negative() {
            return Err(ConstError::without_span(ErrorKind::NegativeIntShift));
        }

        let shift_amount: u32 = {
            let shift_amount = rhs
                .into_u64()
                .ok_or_else(|| ConstError::without_span(ErrorKind::IntOverflow))?;

            // Mask the shift amount depending on the effective integer width
            if effective_width == Width::As64 {
                (shift_amount % 64) as u32
            } else {
                (shift_amount % 32) as u32
            }
        };

        // Since bitshifts are width-sensitive, mask appropriately
        let bits = match self.width {
            Width::As32 => self.into_bits() & 0xFFFF_FFFF,
            Width::As64 => self.into_bits(),
        };

        // Even though overflow would not occur since we currently mask the shift amount,
        // still do a `checked_shr` since this masking behaviour is very odd and may
        // be disabled by a config option.
        let bits = bits
            .checked_shr(shift_amount)
            .ok_or_else(|| ConstError::without_span(ErrorKind::IntOverflow))?;

        // Always an unsigned integer
        let new_sign = Sign::Positive;

        Self::check_overflow(Some(bits), new_sign, effective_width)
    }

    /// Negates the sign of the integer.
    /// Does nothing for a magnitude of 0.
    pub fn negate(self) -> Result<ConstInt, ConstError> {
        Self::check_overflow(Some(self.magnitude), self.sign.negate(), self.width)
    }

    /// Negates the sign of the integer.
    /// Does nothing for a magnitude of 0.
    ///
    /// Note: This can break the invariant that for a width of `As32` and sign of `Negative`,
    /// magnitude must be in the range `[0, 0x7FFFFFFF]`.
    /// The only allowed usage of this function is for negating the `rhs` in `checked_sub`
    fn unchecked_negate(mut self) -> Self {
        if self.magnitude > 0 {
            self.sign = self.sign.negate()
        }

        self
    }

    /// Converts `self` into the corresponding bitwise representation
    /// (not taking into account the width)
    fn into_bits(self) -> u64 {
        match self.sign {
            Sign::Positive => self.magnitude,
            Sign::Negative => !self.magnitude + 1,
        }
    }

    fn effective_width(lhs: Width, rhs: Width) -> Width {
        // Potential widths
        // lhs rhs    effective width
        // 32  32  => 32
        // 64  32  => 32
        // 32  64  => 32
        // 64  64  => 64

        if lhs == Width::As64 && lhs == rhs {
            Width::As64
        } else {
            Width::As32
        }
    }

    fn check_overflow(
        value: Option<u64>,
        new_sign: Sign,
        effective_width: Width,
    ) -> Result<ConstInt, ConstError> {
        let (magnitude, sign) = {
            let effective_magnitude =
                value.ok_or_else(|| ConstError::without_span(ErrorKind::IntOverflow))?;

            if effective_magnitude == 0 {
                // `0` is always a "positive" number
                (0, Sign::Positive)
            } else {
                // Keep the same magnitude
                (effective_magnitude, new_sign)
            }
        };

        // Check for overflow
        let overflowed = match (effective_width, sign) {
            // [0, 0xFFFF_FFFF]
            (Width::As32, Sign::Positive) => magnitude > u64::from(u32::MAX),
            // [0, 0x8000_0000]
            (Width::As32, Sign::Negative) => magnitude > u64::from(i32::MIN.unsigned_abs()),
            // [0, 0xFFFFFFFF_FFFFFFFF] or all values of u64
            (Width::As64, Sign::Positive) => false,
            // [0, 0x80000000_00000000]
            (Width::As64, Sign::Negative) => magnitude > i64::MIN.unsigned_abs(),
        };

        if !overflowed {
            Ok(Self {
                magnitude,
                sign,
                width: effective_width,
            })
        } else {
            Err(ConstError::without_span(ErrorKind::IntOverflow))
        }
    }
}

impl fmt::Display for ConstInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let magnitude = if f.alternate() {
            // Show the width
            let width = match self.width {
                Width::As32 => "i32",
                Width::As64 => "i64",
            };

            format!("{}{}", self.magnitude, width)
        } else {
            // Just the magnitude
            format!("{}", self.magnitude)
        };

        f.pad_integral(matches!(self.sign, Sign::Positive), "", &magnitude)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Width {
    As32,
    As64,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Sign {
    Positive,
    Negative,
}

impl Sign {
    fn negate(self) -> Self {
        match self {
            Sign::Positive => Sign::Negative,
            Sign::Negative => Sign::Positive,
        }
    }
}
