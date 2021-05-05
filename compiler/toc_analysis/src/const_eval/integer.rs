//! Sign & magnitude integer representation

use std::convert::TryInto;
use std::fmt;

use crate::const_eval::ConstError;

/// Constant Integer representation
#[derive(Debug, Clone, Copy)]
pub struct ConstInt {
    /// invariant: must be representable with the given `sign` and `magnitude`
    magnitude: u64,
    sign: IntSign,
    width: IntWidth,
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
            IntWidth::As64
        } else {
            // Apply only 32-bit operations
            if value > u32::MAX as u64 {
                // Already overflowing
                return Err(ConstError::IntOverflow);
            }

            IntWidth::As32
        };

        Ok(Self {
            sign: IntSign::Positive,
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
                return Err(ConstError::IntOverflow);
            }

            IntWidth::As64
        } else {
            // Apply only 32-bit operations
            if (value.is_sign_negative() && value < i32::MIN as f64)
                || (value.is_sign_positive() && value > u32::MAX as f64)
            {
                // Already overflowing
                return Err(ConstError::IntOverflow);
            }

            IntWidth::As32
        };

        let (magnitude, sign) = {
            let magnitude = value.abs().trunc() as u64;

            if magnitude == 0 {
                // Always +0
                (0, IntSign::Positive)
            } else {
                let sign = if value.is_sign_negative() {
                    IntSign::Negative
                } else {
                    IntSign::Positive
                };

                (magnitude, sign)
            }
        };

        Ok(Self {
            sign,
            magnitude,
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
            IntSign::Positive => self.magnitude.try_into().ok(),
            IntSign::Negative => None,
        }
    }

    /// Converts the `ConstInt` into the corresponding `u64` value.
    ///
    /// ## Returns
    /// Returns `Some(u64)` if the `ConstInt` is a positive integer (including zero),
    /// or `None` otherwise.
    pub fn into_u64(self) -> Option<u64> {
        match self.sign {
            IntSign::Positive => Some(self.magnitude),
            IntSign::Negative => None,
        }
    }

    /// Converts the `ConstInt` into the corresponding `f64` value.
    pub fn into_f64(self) -> f64 {
        match self.sign {
            IntSign::Positive => self.magnitude as f64,
            IntSign::Negative => -(self.magnitude as f64),
        }
    }

    /// Returns `true` if the integer has a negative sign, or
    /// `false` if the integer has a positive sign or is zero.
    pub fn is_negative(self) -> bool {
        matches!(self.sign, IntSign::Negative)
    }

    /// Returns `true` if the integer has a positive sign or is zero, or
    /// `false` if the integer has a negative sign.
    pub fn is_positive(self) -> bool {
        matches!(self.sign, IntSign::Positive)
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
            IntSign::Positive
        } else {
            // Will always be negative
            IntSign::Negative
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
            .ok_or(ConstError::DivByZero)?;

        let new_sign = if self.sign == rhs.sign {
            // Will always be positive
            IntSign::Positive
        } else {
            // Will always be negative
            IntSign::Negative
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
            .ok_or_else(|| ConstError::DivByZero)?;

        // Adjust into the correct signage
        let (modulus, new_sign) = if remainder == 0 {
            // Always +0
            (0, IntSign::Positive)
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
            .ok_or_else(|| ConstError::DivByZero)?;

        // Adjust into the correct signage
        let (remainder, new_sign) = if remainder == 0 {
            // Always +0
            (0, IntSign::Positive)
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
            return Err(ConstError::NegativeIntExp);
        }

        let exp = rhs
            .magnitude
            .try_into()
            .map_err(|_| ConstError::IntOverflow)?;

        let value = self
            .magnitude
            .checked_pow(exp)
            .ok_or_else(|| ConstError::IntOverflow)?;

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
            IntWidth::As32 => bits & 0xFFFF_FFFF,
            IntWidth::As64 => bits,
        };

        // Always an unsigned integer, and always representable
        ConstInt {
            magnitude: bits,
            sign: IntSign::Positive,
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
            IntWidth::As32 => bits & 0xFFFF_FFFF,
            IntWidth::As64 => bits,
        };

        // Always an unsigned integer, and always representable
        ConstInt {
            magnitude: bits,
            sign: IntSign::Positive,
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
            IntWidth::As32 => bits & 0xFFFF_FFFF,
            IntWidth::As64 => bits,
        };

        // Always an unsigned integer, and always representable
        ConstInt {
            magnitude: bits,
            sign: IntSign::Positive,
            width: self.width,
        }
    }

    /// Performs the bitwise negation operation.
    ///
    /// Implicitly transforms the integer into the unsigned variant.
    pub fn not(self) -> ConstInt {
        // Apply the not operation
        let bits = match self.width {
            IntWidth::As32 => !self.into_bits() & 0xFFFF_FFFF,
            IntWidth::As64 => !self.into_bits(),
        };

        // Always an unsigned integer, and always representable
        ConstInt {
            magnitude: bits,
            sign: IntSign::Positive,
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
            return Err(ConstError::NegativeIntShift);
        }

        let shift_amount: u32 = {
            let shift_amount = rhs.into_u64().ok_or_else(|| ConstError::IntOverflow)?;

            // Mask the shift amount depending on the effective integer width
            if effective_width == IntWidth::As64 {
                (shift_amount % 64) as u32
            } else {
                (shift_amount % 32) as u32
            }
        };

        // Since bitshifts are width-sensitive, mask appropriately
        let bits = match self.width {
            IntWidth::As32 => self.into_bits() & 0xFFFF_FFFF,
            IntWidth::As64 => self.into_bits(),
        };

        let bits = bits
            .checked_shl(shift_amount)
            .ok_or_else(|| ConstError::IntOverflow)?;

        // Always an unsigned integer
        let new_sign = IntSign::Positive;

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
            return Err(ConstError::NegativeIntShift);
        }

        let shift_amount: u32 = {
            let shift_amount = rhs.into_u64().ok_or_else(|| ConstError::IntOverflow)?;

            // Mask the shift amount depending on the effective integer width
            if effective_width == IntWidth::As64 {
                (shift_amount % 64) as u32
            } else {
                (shift_amount % 32) as u32
            }
        };

        // Since bitshifts are width-sensitive, mask appropriately
        let bits = match self.width {
            IntWidth::As32 => self.into_bits() & 0xFFFF_FFFF,
            IntWidth::As64 => self.into_bits(),
        };

        // Even though overflow would not occur since we currently mask the shift amount,
        // still do a `checked_shr` since this masking behaviour is very odd and may
        // be disabled by a config option.
        let bits = bits
            .checked_shr(shift_amount)
            .ok_or_else(|| ConstError::IntOverflow)?;

        // Always an unsigned integer
        let new_sign = IntSign::Positive;

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
            IntSign::Positive => self.magnitude,
            IntSign::Negative => !self.magnitude + 1,
        }
    }

    fn effective_width(lhs: IntWidth, rhs: IntWidth) -> IntWidth {
        // Potential widths
        // lhs rhs    effective width
        // 32  32  => 32
        // 64  32  => 32
        // 32  64  => 32
        // 64  64  => 64

        if lhs == IntWidth::As64 && lhs == rhs {
            IntWidth::As64
        } else {
            IntWidth::As32
        }
    }

    fn check_overflow(
        value: Option<u64>,
        new_sign: IntSign,
        effective_width: IntWidth,
    ) -> Result<ConstInt, ConstError> {
        let (magnitude, sign) = {
            let effective_magnitude = value.ok_or(ConstError::IntOverflow)?;

            if effective_magnitude == 0 {
                // `0` is always a "positive" number
                (0, IntSign::Positive)
            } else {
                // Keep the same magnitude
                (effective_magnitude, new_sign)
            }
        };

        // Check for overflow
        let overflowed = match effective_width {
            // [0, 0xFFFF_FFFF]
            IntWidth::As32 if sign.is_positive() => magnitude > u32::MAX as u64,
            // [0, 0x8000_0000]
            IntWidth::As32 if sign.is_negative() => magnitude > i32::MIN.unsigned_abs() as u64,
            // [0, 0xFFFFFFFF_FFFFFFFF] or all values of u64
            IntWidth::As64 if sign.is_positive() => false,
            // [0, 0x80000000_00000000]
            IntWidth::As64 if sign.is_negative() => magnitude > i64::MIN.unsigned_abs() as u64,
            // All cases already covered
            _ => unreachable!(),
        };

        if !overflowed {
            Ok(Self {
                magnitude,
                sign,
                width: effective_width,
            })
        } else {
            Err(ConstError::IntOverflow)
        }
    }
}

impl fmt::Display for ConstInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let magnitude = if f.alternate() {
            // Show the width
            let width = match self.width {
                IntWidth::As32 => "i32",
                IntWidth::As64 => "i64",
            };

            format!("{}{}", self.magnitude, width)
        } else {
            // Just the magnitude
            format!("{}", self.magnitude)
        };

        f.pad_integral(matches!(self.sign, IntSign::Positive), "", &magnitude)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IntWidth {
    As32,
    As64,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IntSign {
    Positive,
    Negative,
}

impl IntSign {
    fn negate(self) -> Self {
        match self {
            IntSign::Positive => IntSign::Negative,
            IntSign::Negative => IntSign::Positive,
        }
    }

    fn is_positive(self) -> bool {
        matches!(self, IntSign::Positive)
    }

    fn is_negative(self) -> bool {
        matches!(self, IntSign::Negative)
    }
}
