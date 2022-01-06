//! Instructions used for code generation

/// An offset that is modified for relocations
// Encoded as (previous_reloc: u32, section_offset:u32)
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RelocatableOffset(pub usize);

/// A branch target that will be patched by a later instruction
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodeOffset(pub usize);

/// A reference to the temporaries area in a function
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TemporarySlot(pub usize);

/// Descriptor used for keeping state in `for` loops
///
/// Only used for storage size computation
#[repr(C)]
pub struct ForDescriptor {
    counter: i32,
    end: i32,
    step: i32,
    save_sp: u32,
}

macro_rules! define_encodings {
    (
        $(#[$enum_attr:meta])*
        $vis:vis enum $enum_name:ident {
            $( $(#[$var_attr:meta])* $var_name:ident($($var_ty:ty),* $(,)?) = $var_code:literal),+
            $(,)?
        }
    ) => {
        $(#[$enum_attr])*
        #[repr(u8)]
        $vis enum $enum_name {
            $( $(#[$var_attr])* $var_name ($($var_ty),* ) ),+
        }

        impl $enum_name {
            #[allow(dead_code)] // not encoding anything right now
            $vis fn encoding_kind(&self) -> u8 {
                match self {
                    $( Self::$var_name(..) => $var_code,)+
                }
            }
        }
    };
}

define_encodings! {
    /// Opcodes and their immediate encodings
    #[derive(Debug, Clone, Copy)]
    #[allow(clippy::upper_case_acronyms)] // Matching names from `turing-deep-doc`
    #[allow(dead_code)] // We aren't using all of the invariants right now
    pub enum Opcode {

        /// ## ABORT (abortKind:u32)
        /// Aborts the program.
        /// The message to abort with is dependent on [`AbortKind`]
        ///
        /// ### Stack Effect
        /// ( -- )
        ///
        ABORT (AbortSource) = 0x00,

        /// ## ABORTCOND (abortKind:u32)
        /// (Aborts the program if `do_abort` is true.
        ///
        /// ### Stack Effect
        /// ( do_abort:bool -- )
        ///
        ABORTCOND (AbortSource) = 0x01,

        /// ## ABSINT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ABSINT () = 0x02,

        /// ## ABSREAL ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ABSREAL () = 0x03,

        /// ## ADDINT ()
        /// Adds `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:i32 rhs:i32 -- res:i32 )`
        ///
        ADDINT () = 0x04,

        /// ## ADDINTNAT ()
        /// Adds `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:i32 rhs:u32 -- res:i32 )`
        ///
        ADDINTNAT () = 0x05,

        /// ## ADDNAT ()
        /// Adds `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:u32 -- res:u32 )`
        ///
        ADDNAT () = 0x06,

        /// ## ADDNATINT ()
        /// Adds `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:i32 -- res:u32 )`
        ///
        ADDNATINT () = 0x07,

        /// ## ADDREAL ()
        /// Adds `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:f64 rhs:f64 -- res:f64 )`
        ///
        ADDREAL () = 0x08,

        /// ## ADDSET (setSize:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ADDSET (u32) = 0x09,

        /// ## ALLOCFLEXARRAY ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ALLOCFLEXARRAY () = 0x0A,

        /// ## ALLOCGLOB ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ALLOCGLOB () = 0x0B,

        /// ## ALLOCGLOBARRAY ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ALLOCGLOBARRAY () = 0x0C,

        /// ## ALLOCLOC ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ALLOCLOC () = 0x0D,

        /// ## ALLOCLOCARRAY ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ALLOCLOCARRAY () = 0x0E,

        /// ## AND ()
        /// Performs a bitwise `and` of `lhs` and `rhs`, producing `res`
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:u32 -- res:u32 )`
        ///
        AND () = 0x0F,

        /// ## ARRAYUPPER (dim:u16)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ARRAYUPPER (u16) = 0x10,

        /// ## ASNADDR ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- )`
        ///
        ASNADDR () = 0x11,

        /// ## ASNINT ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        /// Validates that `rhs` is within the range of an `int`.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:i32 -- )`
        ///
        ASNINT () = 0x13,

        /// ## ASNINT1 ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        /// Validates that `rhs` is within the range of an `int1`.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:i8 -- )`
        ///
        ASNINT1 () = 0x15,

        /// ## ASNINT2 ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        /// Validates that `rhs` is within the range of an `int2`.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:i16 -- )`
        ///
        ASNINT2 () = 0x17,

        /// ## ASNINT4 ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:i32 -- )`
        ///
        ASNINT4 () = 0x19,

        /// ## ASNNAT ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        /// Validates that `rhs` is within the range of a `nat`.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:u32 -- )`
        ///
        ASNNAT () = 0x1B,

        /// ## ASNNAT1 ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        /// Validates that `rhs` is within the range of a `nat1`.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:u8 -- )`
        ///
        ASNNAT1 () = 0x1D,

        /// ## ASNNAT2 ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        /// Validates that `rhs` is within the range of a `nat2`.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:u16 -- )`
        ///
        ASNNAT2 () = 0x1F,

        /// ## ASNNAT4 ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:u32 -- )`
        ///
        ASNNAT4 () = 0x21,

        /// ## ASNPTR ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        ///
        /// `rhs` is the address to update the pointer to, and `lhs` is
        /// a pointer to a pointer descriptor.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- )`
        ///
        ASNPTR () = 0x25,

        /// ## ASNREAL ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:f64 -- )`
        ///
        ASNREAL () = 0x27,

        /// ## ASNREAL4 ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        /// Automatically casts `rhs` into the appropriate `real4` type.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:f64 -- )`
        ///
        ASNREAL4 () = 0x29,

        /// ## ASNREAL8 ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:f64 -- )`
        ///
        ASNREAL8 () = 0x2B,

        /// ## ASNADDRINV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        ///
        /// ### Stack Effect
        /// `( rhs:addrint lhs:addrint -- )`
        ///
        ASNADDRINV () = 0x12,

        /// ## ASNINTINV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        /// Validates that `rhs` is within the range of an `int`.
        ///
        /// ### Stack Effect
        /// `( rhs:i32 lhs:addrint -- )`
        ///
        ASNINTINV () = 0x14,

        /// ## ASNINT1INV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        /// Validates that `rhs` is within the range of an `int1`.
        ///
        /// ### Stack Effect
        /// `( rhs:i8 lhs:addrint -- )`
        ///
        ASNINT1INV () = 0x16,

        /// ## ASNINT2INV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        /// Validates that `rhs` is within the range of an `int2`.
        ///
        /// ### Stack Effect
        /// `( rhs:i16 lhs:addrint -- )`
        ///
        ASNINT2INV () = 0x18,

        /// ## ASNINT4INV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        ///
        /// ### Stack Effect
        /// `( rhs:i32 lhs:addrint -- )`
        ///
        ASNINT4INV () = 0x1A,

        /// ## ASNNATINV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        /// Validates that `rhs` is within the range of a `nat`.
        ///
        /// ### Stack Effect
        /// `( rhs:u32 lhs:addrint -- )`
        ///
        ASNNATINV () = 0x1C,

        /// ## ASNNAT1INV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        /// Validates that `rhs` is within the range of a `nat1`.
        ///
        /// ### Stack Effect
        /// `( rhs:u8 lhs:addrint -- )`
        ///
        ASNNAT1INV () = 0x1E,

        /// ## ASNNAT2INV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        /// Validates that `rhs` is within the range of a `nat2`.
        ///
        /// ### Stack Effect
        /// `( rhs:u16 lhs:addrint -- )`
        ///
        ASNNAT2INV () = 0x20,

        /// ## ASNNAT4INV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        ///
        /// ### Stack Effect
        /// `( rhs:u32 lhs:addrint -- )`
        ///
        ASNNAT4INV () = 0x22,

        /// ## ASNPTRINV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        ///
        /// `rhs` is the address to update the pointer to, and `lhs` is
        /// a pointer to a pointer descriptor.
        ///
        /// ### Stack Effect
        /// `( rhs:addrint lhs:addrint -- )`
        ///
        ASNPTRINV () = 0x26,

        /// ## ASNREALINV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        ///
        /// ### Stack Effect
        /// `( rhs:f64 lhs:addrint -- )`
        ///
        ASNREALINV () = 0x28,

        /// ## ASNREAL4INV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        /// Automatically casts `rhs` into the appropriate `real4` type.
        ///
        /// ### Stack Effect
        /// `( rhs:f64 lhs:addrint -- )`
        ///
        ASNREAL4INV () = 0x2A,

        /// ## ASNREAL8INV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        ///
        /// ### Stack Effect
        /// `( rhs:f64 lhs:addrint -- )`
        ///
        ASNREAL8INV () = 0x2C,

        /// ## ASNNONSCALAR (bytes:u32)
        /// Assigns `rhs` into a precomputed `lhs` destination.
        /// This is equivalent to a `memcpy(rhs, lhs, bytes)`.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- )`
        ///
        ASNNONSCALAR (u32) = 0x23,

        /// ## ASNNONSCALARINV (bytes:u32)
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        /// This is equivalent to a `memcpy(rhs, lhs, bytes)`.
        ///
        /// ### Stack Effect
        /// `( rhs:addrint lhs:addrint -- )`
        ///
        ASNNONSCALARINV (u32) = 0x24,

        /// ## ASNSTR ()
        /// Assigns `rhs` into a precomputed `lhs` destination.
        /// Verifies that `lhs` will fit into the required `size` bytes.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint size:u32 -- )`
        ///
        ASNSTR () = 0x2D,

        /// ## ASNSTRINV ()
        /// Assigns `rhs` into a **post**computed `lhs` destination.
        /// Verifies that `lhs` will fit into the required `size` bytes.
        ///
        /// ### Stack Effect
        /// `( rhs:addrint lhs:addrint size:u32 -- )`
        ///
        ASNSTRINV () = 0x2E,

        /// ## BEGINHANDLER (skip:offset, handlerArea:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        BEGINHANDLER (u32, u32) = 0x2F,

        /// ## BITSASSIGN (size:offset, mask:u32, shift:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        BITSASSIGN (u32, u32, u32) = 0x30,

        /// ## BITSEXTRACT (mask:u32, shift:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        BITSEXTRACT (u32, u32) = 0x31,

        /// ## CALL (stkOff:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        CALL (u32) = 0x32,

        /// ## CALLEXTERNAL (externIdx:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        CALLEXTERNAL (u32) = 0x33,

        /// ## CALLIMPLEMENTBY (patchLink:offset, procAddr:addrint)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        CALLIMPLEMENTBY (RelocatableOffset) = 0x34,

        /// ## CASE (descriptor:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        CASE (CodeOffset) = 0x35,

        /// ## CAT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        CAT () = 0x36,

        /// ## CHARSUBSTR1 (kind:u8)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        CHARSUBSTR1 (u8) = 0x37,

        /// ## CHARSUBSTR2 (kind:u8)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        CHARSUBSTR2 (u8) = 0x38,

        /// ## CHARTOCSTR ()
        /// Converts the given `char` into a cstr by copying it
        /// into `storage`.
        ///
        /// ### Stack Effect
        /// `( storage:addrint char:u8 -- storage:addrint )`
        ///
        CHARTOCSTR () = 0x39,

        /// ## CHARTOSTR ()
        /// Converts the given `char` into a string by copying it
        /// as a null-terminated string in `storage`.
        /// Performs the appropriate uninitialized check for `char`.
        ///
        /// ### Stack Effect
        /// `( char:u8 storage:addrint -- storage:addrint )`
        ///
        CHARTOSTR () = 0x3A,

        /// ## CHARTOSTRLEFT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        CHARTOSTRLEFT () = 0x3B,

        /// ## CHKCHRSTRSIZE (reqLen:offset)
        /// Asserts that the `length` of a char(N) is equal to `reqLen`.
        ///
        /// ### Stack Effect
        /// `( length:u32 -- )`
        ///
        CHKCHRSTRSIZE (u32) = 0x3C,

        /// ## CHKCSTRRANGE (reqLen:offset)
        /// Asserts that `chars` has `reqLen` number of chars.
        ///
        /// ### Stack Effect
        /// `( chars:addrint -- chars:addrint )`
        ///
        CHKCSTRRANGE (u32) = 0x3D,

        /// ## CHKRANGE (stackOff:offset, lower:i32, upper:i32, checkKind:u8)
        /// Asserts that `value` at `stackOff` is in the range `[lower, upper]`.
        /// If not, an abort corresponding to `checkKind` is performed.
        ///
        /// ### Stack Effect
        /// `( value:i32 -- value:i32 )`
        ///
        /// The stack effect is relative to `stackOff`.
        ///
        CHKRANGE (u32, i32, i32, CheckKind) = 0x3E,

        /// ## CHKSTRRANGE (maxLen:u16)
        /// Asserts that `value` is at most `maxLen` chars long.
        ///
        /// ### Stack Effect
        /// `( value:addrint -- value:addrint )`
        ///
        CHKSTRRANGE (u16) = 0x3F,

        /// ## CHKSTRSIZE (reqLen:offset)
        /// Asserts that `value` is exactly `reqLen` chars long.
        ///
        /// ### Stack Effect
        /// `( value:addrint -- value:addrint )`
        ///
        CHKSTRSIZE (u32) = 0x40,

        /// ## CLOSE ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        CLOSE () = 0x41,

        /// ## COPYARRAYDESC ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        COPYARRAYDESC () = 0x42,

        /// ## CSTRTOCHAR ()
        /// Converts `chars` into a single `char`.
        /// Asserts that `chars` is of length 1 (by checking `len`).
        ///
        /// ### Stack Effect
        /// `( chars:addrint len:u32 -- char:u8 )`
        ///
        CSTRTOCHAR () = 0x43,

        /// ## CSTRTOSTR ()
        /// Converts the given `chars` into a string by copying it
        /// as a null-terminated string in `storage`.
        /// Performs the appropriate uninitialized check for `char`.
        ///
        /// ### Stack Effect
        /// `( chars:addrint len:u32 storage:addrint -- storage:addrint )`
        ///
        CSTRTOSTR () = 0x44,

        /// ## CSTRTOSTRLEFT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        CSTRTOSTRLEFT () = 0x45,

        /// ## DEALLOCFLEXARRAY ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        DEALLOCFLEXARRAY () = 0x46,

        /// ## DECSP (amount:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        DECSP (u32) = 0x47,

        /// ## DIVINT ()
        /// Divides `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:i32 rhs:i32 -- res:i32 )`
        ///
        DIVINT () = 0x48,

        /// ## DIVNAT ()
        /// Divides `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:u32 -- res:u32 )`
        ///
        DIVNAT () = 0x49,

        /// ## DIVREAL ()
        /// Divides `lhs` and `rhs`, producing `res`.
        /// This performs integer division.
        /// For the floating point division version, use [`Opcode::REALDIVIDE`]
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:f64 rhs:f64 -- res:i32 )`
        ///
        DIVREAL () = 0x4A,

        /// ## EMPTY ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        EMPTY () = 0x4B,

        /// ## ENDFOR (jumpTo:offset)
        /// Completes one iteration of a for-loop.
        /// The branch is taken only if the for-loop counter has not surpassed the
        /// end value.
        ///
        /// `forDescriptor` is used to keep track of the for-loop state.
        ///
        /// ### Stack Effect
        /// `( forDescriptor:addrint -- )`
        ///
        ENDFOR (CodeOffset) = 0x4C,

        /// ## EOF ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        EOF () = 0x4D,

        /// ## EQADDR ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs = rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- compare:bool )`
        ///
        EQADDR () = 0x4E,

        /// ## EQCHARN (len:u32)
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs = rhs`.
        /// The char sequences at `lhs` and `rhs` must be of length `len` (though
        /// it is not validated).
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- compare:bool )`
        ///
        EQCHARN (u32) = 0x4F,

        /// ## EQINT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs = rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:i32 rhs:i32 -- compare:bool )`
        ///
        EQINT () = 0x50,

        /// ## EQINTNAT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs = rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:u32 rhs:i32 -- compare:bool )`
        ///
        EQINTNAT () = 0x51,

        /// ## EQNAT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs = rhs`.
        ///
        /// ### Usage Note
        ///
        /// This opcode isn't used as it isn't implemented in TProlog, and is only here
        /// for the sake of completeness.
        ///
        /// ### Stack Effect
        /// `( lhs:u32 rhs:u32 -- compare:bool )`
        ///
        EQNAT () = 0x52,

        /// ## EQREAL ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs = rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:f64 rhs:f64 -- compare:bool )`
        ///
        EQREAL () = 0x53,

        /// ## EQSTR ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs = rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- compare:bool )`
        ///
        EQSTR () = 0x55,

        /// ## EQSET (setSize:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        EQSET (u32) = 0x92,

        /// ## EXPINTINT ()
        /// Computes `lhs` to the exponent `rhs`, producing res``
        ///
        /// ### Stack Effect
        /// `( lhs:i32 rhs:i32 -- res:i32 )`
        ///
        EXPINTINT () = 0x56,

        /// ## EXPREALINT ()
        /// Computes `lhs` to the exponent `rhs`, producing res``
        ///
        /// ### Stack Effect
        /// `( lhs:f64 rhs:i32 -- res:f64 )`
        ///
        EXPREALINT () = 0x57,

        /// ## EXPREALREAL ()
        /// Computes `lhs` to the exponent `rhs`, producing res``
        ///
        /// ### Stack Effect
        /// `( lhs:f64 rhs:f64 -- res:f64 )`
        ///
        EXPREALREAL () = 0x58,

        /// ## FETCHADDR ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Performs uninitialized value checking.
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:addrint )`
        ///
        FETCHADDR () = 0x59,

        /// ## FETCHBOOL ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Performs uninitialized value checking.
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:bool )`
        ///
        FETCHBOOL () = 0x5A,

        /// ## FETCHINT ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Performs uninitialized value checking.
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:i32 )`
        ///
        FETCHINT () = 0x5B,

        /// ## FETCHINT1 ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Automatically casts `value` from `int1` into `int4`
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:i8 )`
        ///
        FETCHINT1 () = 0x5C,

        /// ## FETCHINT2 ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Automatically casts `value` from `int2` into `int4`
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:i16 )`
        ///
        FETCHINT2 () = 0x5D,

        /// ## FETCHINT4 ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:i32 )`
        ///
        FETCHINT4 () = 0x5E,

        /// ## FETCHNAT ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Performs uninitialized value checking.
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:u32 )`
        ///
        FETCHNAT () = 0x5F,

        /// ## FETCHNAT1 ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Automatically casts `value` from `nat1` into `nat4`
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:u8 )`
        ///
        FETCHNAT1 () = 0x60,

        /// ## FETCHNAT2 ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Automatically casts `value` from `nat2` into `nat4`
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:u16 )`
        ///
        FETCHNAT2 () = 0x61,

        /// ## FETCHNAT4 ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:u32 )`
        ///
        FETCHNAT4 () = 0x62,

        /// ## FETCHPTR ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Performs uninitialized value checking and pointer generation checking.
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:addrint )`
        ///
        FETCHPTR () = 0x63,

        /// ## FETCHREAL ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Performs uninitialized value checking.
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:f64 )`
        ///
        FETCHREAL () = 0x64,

        /// ## FETCHREAL4 ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// Automatically casts `value` from `real4` into `real8`
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:f64 )`
        ///
        FETCHREAL4 () = 0x65,

        /// ## FETCHREAL8 ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:f64 )`
        ///
        FETCHREAL8 () = 0x66,

        /// ## FETCHSET (setSize:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        FETCHSET (u32) = 0x67,

        /// ## FETCHSTR ()
        /// Fetches `value` from `addr`, and pushes it onto the data stack.
        /// `value` points to the string's storage.
        ///
        /// ### Stack Effect
        /// `( addr:addrint -- value:addrint )`
        ///
        FETCHSTR () = 0x68,

        /// ## FIELD (fieldOff:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        FIELD (u32) = 0x69,

        /// ## FOR (skipBlock:offset)
        /// Initializes the state of the for-loop, storing state at `forDescriptor`.
        ///
        /// ### Stack Effect
        /// `( start:i32 end:i32 step:i32 forDescriptor:addrint -- )`
        ///
        FOR (CodeOffset) = 0x6A,

        /// ## FORK (patchLink:offset, processName:addrint, paramBlockSize:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        FORK (RelocatableOffset, u32) = 0x6B,

        /// ## FREE (monitorField:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        FREE (u32) = 0x6C,

        /// ## FREECLASS (monitorField:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        FREECLASS (u32) = 0x6D,

        /// ## FREEU ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        FREEU () = 0x6E,

        /// ## GECLASS ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs >= rhs`
        /// (i.e. if `lhs` is an ancestor class, or the same class as `rhs`).
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- compare:bool )`
        ///
        GECLASS () = 0x70,

        /// ## GECHARN (len:u32)
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs >= rhs`.
        /// The char sequences at `lhs` and `rhs` must be of length `len` (though
        /// it is not validated).
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- compare:bool )`
        ///
        GECHARN (u32) = 0x6F,

        /// ## GEINT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs >= rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:i32 rhs:i32 -- compare:bool )`
        ///
        GEINT () = 0x71,

        /// ## GEINTNAT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs >= rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:i32 rhs:u32 -- compare:bool )`
        ///
        GEINTNAT () = 0x72,

        /// ## GENAT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs >= rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:u32 rhs:u32 -- compare:bool )`
        ///
        GENAT () = 0x73,

        /// ## GENATINT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs >= rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:u32 rhs:i32 -- compare:bool )`
        ///
        GENATINT () = 0x74,

        /// ## GEREAL ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs >= rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:f64 rhs:f64 -- compare:bool )`
        ///
        GEREAL () = 0x75,

        /// ## GESTR ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs >= rhs`
        /// (i.e. if `lhs` is lexicographically after `rhs`).
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- compare:bool )`
        ///
        GESTR () = 0x77,

        /// ## GESET (setSize:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        GESET (u32) = 0x76,

        /// ## GET (getKind:u8, ...)
        /// Gets a value from the provided `stream`.
        ///
        /// The actual number of stack arguments and operands is based on `getKind`,
        /// as different get items have different uses for the arguments.
        /// See [`GetKind`] for more information on the specific stack arguments and
        /// instruction operands.
        ///
        /// ### Stack Effect
        /// `( ... stream:addrint -- )`
        ///
        GET (GetKind) = 0x78,

        /// ## GETPRIORITY ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        GETPRIORITY () = 0x79,

        /// ## GTCLASS ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        GTCLASS () = 0x7A,

        /// ## IF (jumpTo:offset)
        /// Follows the branch if `condition` is false.
        ///
        /// `jumpTo` is computed relative to the address *at* the operand.
        ///
        /// ### Stack Effect
        /// `( condition:bool -- ??? )`
        ///
        IF (CodeOffset) = 0x7B,

        /// ## IN (setSize:u32, lo:i32, hi:i32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        IN (u32, i32, i32) = 0x7C,

        /// ## INCLINENO ()
        /// Increments the line number in the location information by 1.
        ///
        /// ### Stack Effect
        /// `( -- )`
        ///
        INCLINENO () = 0x7D,

        /// ## INCSP (amount:offset)
        /// Adjusts the data stack pointer by `amount` bytes.
        /// This is the same thing as popping off many items from the data stack.
        ///
        /// ### Stack Effect
        /// `( ...items:bytes(amount) -- )`
        ///
        INCSP (u32) = 0x7E,

        /// ## INFIXAND (jumpTo:offset)
        /// Follows the branch if `condition` is false.
        /// This is used to implement short-circuiting `and`, but is otherwise
        /// equivalent to a chain of `IF`.
        ///
        /// `jumpTo` is computed relative to the address *at* the operand.
        ///
        /// ### Stack Effect
        ///
        /// `( condition:bool -- )`
        ///
        INFIXAND (CodeOffset) = 0x7F,

        /// ## INFIXOR (jumpTo:offset)
        /// Follows the branch if `condition` is true.
        /// This is used to implement short-circuiting `or`, but is otherwise
        /// equivalent to `NOT` `IF`.
        ///
        /// `jumpTo` is computed relative to the address *at* the operand.
        ///
        /// ### Stack Effect
        ///
        /// `( condition:bool -- )`
        ///
        INFIXOR (CodeOffset) = 0x80,

        /// ## INITARRAYDESC ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        INITARRAYDESC () = 0x81,

        /// ## INITCONDITION (patchLink:offset, name:addrint)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        INITCONDITION (RelocatableOffset) = 0x82,

        /// ## INITMONITOR (patchLink:offset, name:addrint)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        INITMONITOR (RelocatableOffset) = 0x83,

        /// ## INITUNIT (patchLink:offset, markerAddr:addrint, initSkip:offset, markerVal:u8)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        INITUNIT (RelocatableOffset, u32, u8) = 0x84,

        /// ## INTREAL ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        INTREAL () = 0x85,

        /// ## INTREALLEFT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        INTREALLEFT () = 0x86,

        /// ## INTSTR ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        INTSTR () = 0x87,

        /// ## JSR (jumpTo:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        JSR (CodeOffset) = 0x88,

        /// ## JUMP (jumpTo:offset)
        /// Jumps the program counter forwards relative to the current instruction.
        ///
        /// `jumpTo` is computed relative to the address *at* the operand.
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        JUMP (CodeOffset) = 0x89,

        /// ## JUMPB (jumpTo:offset)
        /// Jumps the program counter backwards relative to the current instruction.
        ///
        /// `jumpTo` is computed relative to the address *at* the operand.
        ///
        /// ### Stack Effect
        /// `( -- )`
        ///
        JUMPB (CodeOffset) = 0x8A,

        /// ## LECLASS ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs <= rhs`
        /// (i.e. if `lhs` is a descendant class, or the same class as `rhs`).
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- compare:bool )`
        ///
        LECLASS () = 0x8C,

        /// ## LECHARN (len:u32)
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs <= rhs`.
        /// The char sequences at `lhs` and `rhs` must be of length `len` (though
        /// it is not validated).
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- compare:bool )`
        ///
        LECHARN (u32) = 0x8B,

        /// ## LEINT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs <= rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:i32 rhs:i32 -- compare:bool )`
        ///
        LEINT () = 0x8D,

        /// ## LEINTNAT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs <= rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:i32 rhs:u32 -- compare:bool )`
        ///
        LEINTNAT () = 0x8E,

        /// ## LENAT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs <= rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:u32 rhs:u32 -- compare:bool )`
        ///
        LENAT () = 0x8F,

        /// ## LENATINT ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs <= rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:u32 rhs:i32 -- compare:bool )`
        ///
        LENATINT () = 0x90,

        /// ## LEREAL ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs <= rhs`.
        ///
        /// ### Stack Effect
        /// `( lhs:f64 rhs:f64 -- compare:bool )`
        ///
        LEREAL () = 0x91,

        /// ## LESTR ()
        /// Compares `lhs` with `rhs`, producing a `compare` of true if `lhs <= rhs`
        /// (i.e. if `lhs` is lexicographically before `rhs`).
        ///
        /// ### Stack Effect
        /// `( lhs:addrint rhs:addrint -- compare:bool )`
        ///
        LESTR () = 0x93,

        /// ## LESET (setSize:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        LESET (u32) = 0x92,

        /// ## LOCATEARG (argOff:offset)
        /// Produces an address `argAt` that points to the stack argument
        /// at `argOff` from the stack pointer.
        ///
        /// ### Stack Effect
        /// `( -- argAt:addrint )`
        ///
        LOCATEARG (u32) = 0x94,

        /// ## LOCATECLASS (classOff:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        LOCATECLASS (u32) = 0x95,

        /// ## LOCATELOCAL (localOff:offset)
        /// Produces an address `localAt` that points to the local variable
        /// at `localOff` in the locals space.
        ///
        /// ### Stack Effect
        /// `( -- localAt:addrint )`
        ///
        LOCATELOCAL (u32) = 0x96,

        /// ## LOCATEPARM (paramOff:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        LOCATEPARM (u32) = 0x97,

        /// ## LOCATETEMP (localsArea:offset, temporary:offset)
        /// Produces an address `tempAt` that points to the temporary at
        /// `temporary` in the temporary space, just after the locals space.
        ///
        /// ### Stack Effect
        /// `( -- tempAt:addrint )`
        ///
        LOCATETEMP (TemporarySlot) = 0x98,

        /// ## LTCLASS ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        LTCLASS () = 0x99,

        /// ## MAXINT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        MAXINT () = 0x9A,

        /// ## MAXNAT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        MAXNAT () = 0x9B,

        /// ## MAXREAL ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        MAXREAL () = 0x9C,

        /// ## MININT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        MININT () = 0x9D,

        /// ## MINNAT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        MINNAT () = 0x9E,

        /// ## MINREAL ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        MINREAL () = 0x9F,

        /// ## MODINT ()
        /// Performs modulus of `lhs` and `rhs`, producing `res`.
        /// This performs the floored division variant of computing the modulus.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:i32 rhs:i32 -- res:i32 )`
        ///
        MODINT () = 0xA0,

        /// ## MODNAT ()
        /// Performs modulus of `lhs` and `rhs`, producing `res`.
        /// This performs the floored division variant of computing the modulus.
        ///
        /// This is used as the encoding for both `mod` and `rem` on `nat`s,
        /// as they only differer when negative numbers come along.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:u32 -- res:u32 )`
        ///
        MODNAT () = 0xA1,

        /// ## MODREAL ()
        /// Performs modulus of `lhs` and `rhs`, producing `res`.
        /// This performs the floored division variant of computing the modulus.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:f64 rhs:f64 -- res:f64 )`
        ///
        MODREAL () = 0xA2,

        /// ## MONITORENTER ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        MONITORENTER () = 0xA3,

        /// ## MONITOREXIT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        MONITOREXIT () = 0xA4,

        /// ## MULINT ()
        /// Multiplies `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:i32 rhs:i32 -- res:i32 )`
        ///
        MULINT () = 0xA5,

        /// ## MULNAT ()
        /// Multiplies `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:u32 -- res:u32 )`
        ///
        MULNAT () = 0xA6,

        /// ## MULREAL ()
        /// Multiplies `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:f64 rhs:f64 -- res:f64 )`
        ///
        MULREAL () = 0xA7,

        /// ## MULSET (setSize:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        MULSET (u32) = 0xA8,

        /// ## NATREAL ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        NATREAL () = 0xA9,

        /// ## NATREALLEFT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        NATREALLEFT () = 0xAA,

        /// ## NATSTR ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        NATSTR () = 0xAB,

        /// ## NEGINT ()
        /// Flips the sign of `rhs`, producing `res`
        ///
        /// ### Stack Effect
        ///
        /// `( rhs:i32 -- res:i32 )`
        ///
        NEGINT () = 0xAC,

        /// ## NEGREAL ()
        /// Flips the sign of `rhs`, producing `res`
        ///
        /// ### Stack Effect
        ///
        /// `( rhs:i32 -- res:i32 )`
        ///
        NEGREAL () = 0xAD,

        /// ## NEW ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        NEW () = 0xAE,

        /// ## NEWARRAY ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        NEWARRAY () = 0xAF,

        /// ## NEWCLASS ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        NEWCLASS () = 0xB0,

        /// ## NEWU ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        NEWU () = 0xB1,

        /// ## NOT ()
        /// Performs a logical `not` on `rhs`, producing `res`.
        /// This only toggles `res` between 0 and 1, so a `XOR` with `0xFFFFFFFF`
        /// is used to emulate a bitwise not.
        ///
        /// #### Stack Effect
        ///
        /// `( rhs:bool -- res:bool )`
        ///
        NOT () = 0xB2,

        /// ## NUMARRAYELEMENTS ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        NUMARRAYELEMENTS () = 0xB3,

        /// ## OBJCLASS ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        OBJCLASS () = 0xB4,

        /// ## OPEN (openKind:u8, openMode:u8)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        OPEN (u8, u8) = 0xB5,

        /// ## OR ()
        /// Performs a bitwise `or` of `lhs` and `rhs`, producing `res`
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:u32 -- res:u32 )`
        ///
        OR () = 0xB6,

        /// ## ORD ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ORD () = 0xB7,

        /// ## PAUSE ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        PAUSE () = 0xB8,

        /// ## PRED ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        PRED () = 0xB9,

        /// ## PROC (localSize:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        PROC (u32) = 0xBA,

        /// ## PUSHADDR (imm:addrint)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        PUSHADDR (u32) = 0xBB,

        /// ## PUSHADDR1 (patchLink:offset, imm:addrint)
        /// Pushes the `imm` address (after relocation fixup) onto the data stack.
        ///
        /// ### Stack Effect
        /// `( -- imm:addrint )`
        ///
        PUSHADDR1 (RelocatableOffset) = 0xBC,

        /// ## PUSHCOPY ()
        /// Pushes a copy of `value` onto the data stack.
        ///
        /// ### Stack Effect
        /// `( value:u32 -- value:u32 copy:u32 )`
        ///
        PUSHCOPY () = 0xBD,

        /// ## PUSHINT (imm:u32)
        /// Pushes `imm` onto the data stack.
        ///
        /// ### Stack Effect
        /// `( -- imm:i32 )`
        ///
        PUSHINT (u32) = 0xBE,

        /// ## PUSHINT1 (imm:u8)
        /// Pushes `imm` onto the data stack.
        ///
        /// ### Stack Effect
        /// `( -- imm:u8 )`
        ///
        PUSHINT1 (u8) = 0xBF,

        /// ## PUSHINT2 (imm:u16)
        /// Pushes `imm` onto the data stack.
        ///
        /// ### Stack Effect
        /// `( -- imm:u16 )`
        ///
        PUSHINT2 (u16) = 0xC0,

        /// ## PUSHREAL (imm:f64)
        /// Pushes `imm` onto the data stack.
        ///
        /// ### Stack Effect
        /// `( -- imm:f64 )`
        ///
        PUSHREAL (f64) = 0xC1,

        /// ## PUSHVAL0 ()
        /// Pushes a literal 0 as `imm` onto the data stack.
        ///
        /// ### Stack Effect
        /// `( -- imm:u8 )`
        ///
        PUSHVAL0 () = 0xC2,

        /// ## PUSHVAL1 ()
        /// Pushes a literal 1 as `imm` onto the data stack.
        ///
        /// ### Stack Effect
        /// `( -- imm:u8 )`
        ///
        PUSHVAL1 () = 0xC3,

        /// ## PUT (putKind:u8)
        /// Puts the given value to the provided `stream`.
        ///
        /// The actual number of stack arguments is based on `putKind`,
        /// as different put items have different uses for the arguments.
        /// See [`PutKind`] for more information on the specific stack arguments.
        ///
        /// ### Stack Effect
        /// `( ... stream:addrint -- )`
        ///
        PUT (PutKind) = 0xC4,

        /// ## QUIT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        QUIT () = 0xC5,

        /// ## READ ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        READ () = 0xC6,

        /// ## REALDIVIDE ()
        /// Divides `lhs` and `rhs`, producing `res`.
        /// This performs floating point division.
        /// For the integer division version, use [`Opcode::DIVREAL`]
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:f64 rhs:f64 -- res:f64 )`
        ///
        REALDIVIDE () = 0xC7,

        /// ## REMINT ()
        /// Finds the euclidean remainder of `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:i32 rhs:i32 -- res:i32 )`
        ///
        REMINT () = 0xC8,

        /// ## REMREAL ()
        /// Finds the euclidean remainder of `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:f64 rhs:f64 -- res:f64 )`
        ///
        REMREAL () = 0xC9,

        /// ## RESOLVEDEF (descOff:offset)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        RESOLVEDEF (u32) = 0xCA,

        /// ## RESOLVEPTR ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        RESOLVEPTR () = 0xCB,

        /// ## RESTORESP ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        RESTORESP () = 0xCC,

        /// ## RETURN ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        RETURN () = 0xCD,

        /// ## RTS ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        RTS () = 0xCE,

        /// ## SAVESP ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SAVESP () = 0xCF,

        /// ## SEEK ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SEEK () = 0xD0,

        /// ## SEEKSTAR ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SEEKSTAR () = 0xD1,

        /// ## SETALL (setSize:u32, tail:u16)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SETALL (u32, u16) = 0xD2,

        /// ## SETCLR (setSize:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SETCLR (u32) = 0xD3,

        /// ## SETELEMENT (setSize:u32, lower:i32, upper:i32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SETELEMENT (u32, i32, i32) = 0xD4,

        /// ## SETFILENO (fileNo:u16, lineNo:u16)
        /// Sets the file and line number in the location information to `fileNo` and `lineNo`
        /// respectively.
        ///
        /// ### Stack Effect
        /// `( -- )`
        ///
        SETFILENO (u16, u16) = 0xD5,

        /// ## SETLINENO (lineNo:u16)
        /// Sets the line number in the location information to `lineNo`.
        ///
        /// ### Stack Effect
        /// `( -- )`
        ///
        SETLINENO (u16) = 0xD6,

        /// ## SETPRIORITY ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SETPRIORITY () = 0xD7,

        /// ## SETSTDSTREAM (kind:u8)
        /// Assigns `dest` with the stream handle of the corresponding
        /// standard stream. If a standard stream isn't redirected, the handle
        /// stored will be the default stream handle for the respective
        /// standard stream.
        ///
        /// ### Stack Effect
        /// `( dest:addrint -- ??? )`
        ///
        SETSTDSTREAM (StdStream) = 0xD8,

        /// ## SETSTREAM (kind:u8)
        /// Sets up a stream for operations with the given `kind`.
        ///
        /// This initializes the stream info with `streamHandle` (the handle for the stream)
        /// and `status` (a pointer to where to store the status), and copies these two into
        /// `handleVar` and `statusVar` respectively.
        ///
        /// ### Stack Effect
        /// `( streamHandle:i32 status:addrint handleVar:addrint statusVar:addrint -- )`
        ///
        SETSTREAM (StreamKind) = 0xD9,

        /// ## SHL ()
        /// Shifts `lhs` left by `rhs` bits, producing `res`
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:u32 -- res:u32 )`
        ///
        SHL () = 0xDA,

        /// ## SHR ()
        /// Shifts `lhs` right by `rhs` bits, producing `res`
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:u32 -- res:u32 )`
        ///
        SHR () = 0xDB,

        /// ## SIGNAL (kind:u8)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SIGNAL (u8) = 0xDC,

        /// ## STRINT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        STRINT () = 0xDD,

        /// ## STRINTOK ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        STRINTOK () = 0xDE,

        /// ## STRNAT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        STRNAT () = 0xDF,

        /// ## STRNATOK ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        STRNATOK () = 0xE0,

        /// ## STRTOCHAR ()
        /// Converts `chars` into a single `char`.
        /// Asserts that `chars` is of length 1.
        ///
        /// ### Stack Effect
        /// `( chars:addrint -- char:u8 )`
        ///
        STRTOCHAR () = 0xE1,

        /// ## SUBINT ()
        /// Subtracts `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:i32 rhs:i32 -- res:i32 )`
        ///
        SUBINT () = 0xE2,

        /// ## SUBINTNAT ()
        /// Subtracts `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:i32 rhs:u32 -- res:i32 )`
        ///
        SUBINTNAT () = 0xE3,

        /// ## SUBNAT ()
        /// Subtracts `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:u32 -- res:u32 )`
        ///
        SUBNAT () = 0xE4,

        /// ## SUBNATINT ()
        /// Subtracts `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:i32 -- res:u32 )`
        ///
        SUBNATINT () = 0xE5,

        /// ## SUBREAL ()
        /// Subtracts `lhs` and `rhs`, producing `res`.
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:f64 rhs:f64 -- res:f64 )`
        ///
        SUBREAL () = 0xE6,

        /// ## SUBSCRIPT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SUBSCRIPT () = 0xE7,

        /// ## SUBSET (setSize:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SUBSET (u32) = 0xE8,

        /// ## SUBSTR1 (kind:u8)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SUBSTR1 (u8) = 0xE9,

        /// ## SUBSTR2 (kind:u8)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SUBSTR2 (u8) = 0xEA,

        /// ## SUCC ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SUCC () = 0xEB,

        /// ## TAG (tagSize:u8)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        TAG (u8) = 0xEC,

        /// ## TELL ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        TELL () = 0xED,

        /// ## UFIELD (fieldOff:offset, expectMap:offset, tagSize:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        UFIELD (u32, u32, u32) = 0xEE,

        /// ## UNINIT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        UNINIT () = 0xEF,

        /// ## UNINITADDR ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        UNINITADDR () = 0xF0,

        /// ## UNINITBOOLEAN ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        UNINITBOOLEAN () = 0xF1,

        /// ## UNINITINT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        UNINITINT () = 0xF2,

        /// ## UNINITNAT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        UNINITNAT () = 0xF3,

        /// ## UNINITREAL ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        UNINITREAL () = 0xF4,

        /// ## UNINITSTR ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        UNINITSTR () = 0xF5,

        /// ## UNLINKHANDLER ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        UNLINKHANDLER () = 0xF6,

        /// ## VSUBSCRIPT (elemSize:offset, lower:i32, upper:i32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        VSUBSCRIPT (u32, i32, i32) = 0xF7,

        /// ## WAIT (kind:u8)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        WAIT (u8) = 0xF8,

        /// ## WRITE ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        WRITE () = 0xF9,

        /// ## XOR ()
        /// Performs a bitwise `xor` of `lhs` and `rhs`, producing `res`
        ///
        /// ### Stack Effect
        ///
        /// `( lhs:u32 rhs:u32 -- res:u32 )`
        ///
        XOR () = 0xFA,

        /// ## XORSET (setSize:u32)
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        XORSET (u32) = 0xFB,

        /// ## BREAK ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        BREAK () = 0xFC,

        /// ## SYSEXIT ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        SYSEXIT () = 0xFD,

        /// ## ILLEGAL ()
        /// (description)
        ///
        /// ### Stack Effect
        /// `( ??? -- ??? )`
        ///
        ILLEGAL () = 0xFE,

    }
}

impl Opcode {
    pub fn size(&self) -> usize {
        let operand_count = match self {
            Opcode::ABORT(_) => 1,
            Opcode::ABORTCOND(_) => 1,
            Opcode::ABSINT() => 0,
            Opcode::ABSREAL() => 0,
            Opcode::ADDINT() => 0,
            Opcode::ADDINTNAT() => 0,
            Opcode::ADDNAT() => 0,
            Opcode::ADDNATINT() => 0,
            Opcode::ADDREAL() => 0,
            Opcode::ADDSET(_) => 1,
            Opcode::ALLOCFLEXARRAY() => 0,
            Opcode::ALLOCGLOB() => 0,
            Opcode::ALLOCGLOBARRAY() => 0,
            Opcode::ALLOCLOC() => 0,
            Opcode::ALLOCLOCARRAY() => 0,
            Opcode::AND() => 0,
            Opcode::ARRAYUPPER(_) => 1,
            Opcode::ASNADDR() => 0,
            Opcode::ASNINT() => 0,
            Opcode::ASNINT1() => 0,
            Opcode::ASNINT2() => 0,
            Opcode::ASNINT4() => 0,
            Opcode::ASNNAT() => 0,
            Opcode::ASNNAT1() => 0,
            Opcode::ASNNAT2() => 0,
            Opcode::ASNNAT4() => 0,
            Opcode::ASNPTR() => 0,
            Opcode::ASNREAL() => 0,
            Opcode::ASNREAL4() => 0,
            Opcode::ASNREAL8() => 0,
            Opcode::ASNADDRINV() => 0,
            Opcode::ASNINTINV() => 0,
            Opcode::ASNINT1INV() => 0,
            Opcode::ASNINT2INV() => 0,
            Opcode::ASNINT4INV() => 0,
            Opcode::ASNNATINV() => 0,
            Opcode::ASNNAT1INV() => 0,
            Opcode::ASNNAT2INV() => 0,
            Opcode::ASNNAT4INV() => 0,
            Opcode::ASNPTRINV() => 0,
            Opcode::ASNREALINV() => 0,
            Opcode::ASNREAL4INV() => 0,
            Opcode::ASNREAL8INV() => 0,
            Opcode::ASNNONSCALAR(_) => 1,
            Opcode::ASNNONSCALARINV(_) => 1,
            Opcode::ASNSTR() => 0,
            Opcode::ASNSTRINV() => 0,
            Opcode::BEGINHANDLER(_, _) => 2,
            Opcode::BITSASSIGN(_, _, _) => 3,
            Opcode::BITSEXTRACT(_, _) => 2,
            Opcode::CALL(_) => 1,
            Opcode::CALLEXTERNAL(_) => 1,
            Opcode::CALLIMPLEMENTBY(_) => 2,
            Opcode::CASE(_) => 1,
            Opcode::CAT() => 0,
            Opcode::CHARSUBSTR1(_) => 1,
            Opcode::CHARSUBSTR2(_) => 1,
            Opcode::CHARTOCSTR() => 0,
            Opcode::CHARTOSTR() => 0,
            Opcode::CHARTOSTRLEFT() => 0,
            Opcode::CHKCHRSTRSIZE(_) => 1,
            Opcode::CHKCSTRRANGE(_) => 1,
            Opcode::CHKRANGE(_, _, _, _) => 4,
            Opcode::CHKSTRRANGE(_) => 1,
            Opcode::CHKSTRSIZE(_) => 1,
            Opcode::CLOSE() => 0,
            Opcode::COPYARRAYDESC() => 0,
            Opcode::CSTRTOCHAR() => 0,
            Opcode::CSTRTOSTR() => 0,
            Opcode::CSTRTOSTRLEFT() => 0,
            Opcode::DEALLOCFLEXARRAY() => 0,
            Opcode::DECSP(_) => 1,
            Opcode::DIVINT() => 0,
            Opcode::DIVNAT() => 0,
            Opcode::DIVREAL() => 0,
            Opcode::EMPTY() => 0,
            Opcode::ENDFOR(_) => 1,
            Opcode::EOF() => 0,
            Opcode::EQADDR() => 0,
            Opcode::EQCHARN(_) => 1,
            Opcode::EQINT() => 0,
            Opcode::EQINTNAT() => 0,
            Opcode::EQNAT() => 0,
            Opcode::EQREAL() => 0,
            Opcode::EQSTR() => 0,
            Opcode::EQSET(_) => 1,
            Opcode::EXPINTINT() => 0,
            Opcode::EXPREALINT() => 0,
            Opcode::EXPREALREAL() => 0,
            Opcode::FETCHADDR() => 0,
            Opcode::FETCHBOOL() => 0,
            Opcode::FETCHINT() => 0,
            Opcode::FETCHINT1() => 0,
            Opcode::FETCHINT2() => 0,
            Opcode::FETCHINT4() => 0,
            Opcode::FETCHNAT() => 0,
            Opcode::FETCHNAT1() => 0,
            Opcode::FETCHNAT2() => 0,
            Opcode::FETCHNAT4() => 0,
            Opcode::FETCHPTR() => 0,
            Opcode::FETCHREAL() => 0,
            Opcode::FETCHREAL4() => 0,
            Opcode::FETCHREAL8() => 0,
            Opcode::FETCHSET(_) => 1,
            Opcode::FETCHSTR() => 0,
            Opcode::FIELD(_) => 1,
            Opcode::FOR(_) => 1,
            Opcode::FORK(_, _) => 3,
            Opcode::FREE(_) => 1,
            Opcode::FREECLASS(_) => 1,
            Opcode::FREEU() => 0,
            Opcode::GECLASS() => 0,
            Opcode::GECHARN(_) => 1,
            Opcode::GEINT() => 0,
            Opcode::GEINTNAT() => 0,
            Opcode::GENAT() => 0,
            Opcode::GENATINT() => 0,
            Opcode::GEREAL() => 0,
            Opcode::GESTR() => 0,
            Opcode::GESET(_) => 1,
            Opcode::GET(kind) => {
                let extra_operands = match kind {
                    // implicit type size
                    GetKind::Boolean() => 1,
                    GetKind::Char() => 1,
                    GetKind::CharRange(_, _) => 3,
                    // explicit type size
                    GetKind::CharN(_) => 1,
                    GetKind::Enum(_) => 1,
                    GetKind::EnumRange(_, _, _) => 3,
                    GetKind::Int(_) => 1,
                    GetKind::IntRange(_, _, _) => 3,
                    GetKind::Nat(_) => 1,
                    GetKind::Real(_) => 1,
                    GetKind::StringExact(_) => 1,
                    GetKind::StringLine(_) => 1,
                    GetKind::StringToken(_) => 1,
                    // no extra operands
                    GetKind::Skip() => 0,
                };
                extra_operands + 1
            }
            Opcode::GETPRIORITY() => 0,
            Opcode::GTCLASS() => 0,
            Opcode::IF(_) => 1,
            Opcode::IN(_, _, _) => 3,
            Opcode::INCLINENO() => 0,
            Opcode::INCSP(_) => 1,
            Opcode::INFIXAND(_) => 1,
            Opcode::INFIXOR(_) => 1,
            Opcode::INITARRAYDESC() => 0,
            Opcode::INITCONDITION(_) => 2,
            Opcode::INITMONITOR(_) => 2,
            Opcode::INITUNIT(_, _, _) => 4,
            Opcode::INTREAL() => 0,
            Opcode::INTREALLEFT() => 0,
            Opcode::INTSTR() => 0,
            Opcode::JSR(_) => 1,
            Opcode::JUMP(_) => 1,
            Opcode::JUMPB(_) => 1,
            Opcode::LECLASS() => 0,
            Opcode::LECHARN(_) => 1,
            Opcode::LEINT() => 0,
            Opcode::LEINTNAT() => 0,
            Opcode::LENAT() => 0,
            Opcode::LENATINT() => 0,
            Opcode::LEREAL() => 0,
            Opcode::LESTR() => 0,
            Opcode::LESET(_) => 1,
            Opcode::LOCATEARG(_) => 1,
            Opcode::LOCATECLASS(_) => 1,
            Opcode::LOCATELOCAL(_) => 1,
            Opcode::LOCATEPARM(_) => 1,
            Opcode::LOCATETEMP(_) => 2,
            Opcode::LTCLASS() => 0,
            Opcode::MAXINT() => 0,
            Opcode::MAXNAT() => 0,
            Opcode::MAXREAL() => 0,
            Opcode::MININT() => 0,
            Opcode::MINNAT() => 0,
            Opcode::MINREAL() => 0,
            Opcode::MODINT() => 0,
            Opcode::MODNAT() => 0,
            Opcode::MODREAL() => 0,
            Opcode::MONITORENTER() => 0,
            Opcode::MONITOREXIT() => 0,
            Opcode::MULINT() => 0,
            Opcode::MULNAT() => 0,
            Opcode::MULREAL() => 0,
            Opcode::MULSET(_) => 1,
            Opcode::NATREAL() => 0,
            Opcode::NATREALLEFT() => 0,
            Opcode::NATSTR() => 0,
            Opcode::NEGINT() => 0,
            Opcode::NEGREAL() => 0,
            Opcode::NEW() => 0,
            Opcode::NEWARRAY() => 0,
            Opcode::NEWCLASS() => 0,
            Opcode::NEWU() => 0,
            Opcode::NOT() => 0,
            Opcode::NUMARRAYELEMENTS() => 0,
            Opcode::OBJCLASS() => 0,
            Opcode::OPEN(_, _) => 2,
            Opcode::OR() => 0,
            Opcode::ORD() => 0,
            Opcode::PAUSE() => 0,
            Opcode::PRED() => 0,
            Opcode::PROC(_) => 1,
            Opcode::PUSHADDR(_) => 1,
            Opcode::PUSHADDR1(_) => 2,
            Opcode::PUSHCOPY() => 0,
            Opcode::PUSHINT(_) => 1,
            Opcode::PUSHINT1(_) => 1,
            Opcode::PUSHINT2(_) => 1,
            Opcode::PUSHREAL(_) => 1,
            Opcode::PUSHVAL0() => 0,
            Opcode::PUSHVAL1() => 0,
            Opcode::PUT(_) => 1,
            Opcode::QUIT() => 0,
            Opcode::READ() => 0,
            Opcode::REALDIVIDE() => 0,
            Opcode::REMINT() => 0,
            Opcode::REMREAL() => 0,
            Opcode::RESOLVEDEF(_) => 1,
            Opcode::RESOLVEPTR() => 0,
            Opcode::RESTORESP() => 0,
            Opcode::RETURN() => 0,
            Opcode::RTS() => 0,
            Opcode::SAVESP() => 0,
            Opcode::SEEK() => 0,
            Opcode::SEEKSTAR() => 0,
            Opcode::SETALL(_, _) => 2,
            Opcode::SETCLR(_) => 1,
            Opcode::SETELEMENT(_, _, _) => 3,
            Opcode::SETFILENO(_, _) => 2,
            Opcode::SETLINENO(_) => 1,
            Opcode::SETPRIORITY() => 0,
            Opcode::SETSTDSTREAM(_) => 1,
            Opcode::SETSTREAM(_) => 1,
            Opcode::SHL() => 0,
            Opcode::SHR() => 0,
            Opcode::SIGNAL(_) => 1,
            Opcode::STRINT() => 0,
            Opcode::STRINTOK() => 0,
            Opcode::STRNAT() => 0,
            Opcode::STRNATOK() => 0,
            Opcode::STRTOCHAR() => 0,
            Opcode::SUBINT() => 0,
            Opcode::SUBINTNAT() => 0,
            Opcode::SUBNAT() => 0,
            Opcode::SUBNATINT() => 0,
            Opcode::SUBREAL() => 0,
            Opcode::SUBSCRIPT() => 0,
            Opcode::SUBSET(_) => 1,
            Opcode::SUBSTR1(_) => 1,
            Opcode::SUBSTR2(_) => 1,
            Opcode::SUCC() => 0,
            Opcode::TAG(_) => 1,
            Opcode::TELL() => 0,
            Opcode::UFIELD(_, _, _) => 3,
            Opcode::UNINIT() => 0,
            Opcode::UNINITADDR() => 0,
            Opcode::UNINITBOOLEAN() => 0,
            Opcode::UNINITINT() => 0,
            Opcode::UNINITNAT() => 0,
            Opcode::UNINITREAL() => 0,
            Opcode::UNINITSTR() => 0,
            Opcode::UNLINKHANDLER() => 0,
            Opcode::VSUBSCRIPT(_, _, _) => 3,
            Opcode::WAIT(_) => 1,
            Opcode::WRITE() => 0,
            Opcode::XOR() => 0,
            Opcode::XORSET(_) => 1,
            Opcode::BREAK() => 0,
            Opcode::SYSEXIT() => 0,
            Opcode::ILLEGAL() => 0,
        };

        (operand_count + 1) * 4
    }
}

define_encodings! {
    /// Possible sources for an abort
    #[derive(Debug, Clone, Copy)]
    #[allow(dead_code)] // We aren't using all of the invariants right now
    pub enum AbortSource {
        /// From an `assert` statement
        Assert() = 1,
        /// From a `pre` statement
        Pre() = 2,
        /// From a `post` statement
        Post() = 3,
        /// From an `invariant` inside of a for-loop
        InvariantFor() = 4,
        /// From an `invariant` inside of a loop
        InvariantLoop() = 5,
        /// From an `invariant` inside of a module
        InvariantModule() = 6,
        /// From coercing a class pointer
        ClassCoercing() = 7,
        /// From a value not handled by a case arm
        InvalidCaseVariant() = 8,
        /// From a `function` missing a `result` statement
        MissingResult() = 9,
    }
}

define_encodings! {
    /// Valid types for `put`
    #[derive(Debug, Clone, Copy)]
    #[allow(dead_code)] // We aren't using all of the variants right now
    pub enum PutKind {
        /// Boolean Item
        ///
        /// ### Stack Effect
        /// `( value:i32 width:i32 -- )`
        ///
        Boolean() = 0,

        /// Char Item
        ///
        /// ### Stack Effect
        /// `( value:u8 width:i32 -- )`
        ///
        Char() = 1,

        /// CharN Item
        ///
        /// Note: The argument ordering is emitted in the wrong order from
        /// original Turing (`width` and `length` are swapped). This uses
        /// the ordering from the bytecode executor, which is what we're
        /// ultimately targeting.
        ///
        /// ### Stack Effect
        /// `( value:addrint length:u32 width:i32 -- )`
        ///
        CharN() = 2,

        /// Enum Item
        ///
        /// ### Stack Effect
        /// `( value:u32 width:i32 variantNames:addrint -- )`
        ///
        Enum() = 3,

        /// Int Item, only width
        ///
        /// ### Stack Effect
        /// `( value:i32 width:i32 -- )`
        Int() = 4,

        /// Int Item, with fractional width
        ///
        /// ### Stack Effect
        /// `( value:i32 width:i32 fractWidth:i32 -- )`
        IntFract() = 5,

        /// Int Item, with exponent width
        ///
        /// ### Stack Effect
        /// `( value:i32 width:i32 fractWidth:i32 expWidth:i32 -- )`
        IntExp() = 6,

        /// Int Item, only width
        ///
        /// ### Stack Effect
        /// `( value:i32 width:i32 -- )`
        Nat() = 7,

        /// Int Item, with fractional width
        ///
        /// ### Stack Effect
        /// `( value:i32 width:i32 fractWidth:i32 -- )`
        NatFract() = 8,

        /// Int Item, with exponent width
        ///
        /// ### Stack Effect
        /// `( value:i32 width:i32 fractWidth:i32 expWidth:i32 -- )`
        NatExp() = 9,

        /// Real Item, only width
        ///
        /// ### Stack Effect
        /// `( value:i32 width:i32 -- )`
        Real() = 10,

        /// Real Item, with fractional width
        ///
        /// ### Stack Effect
        /// `( value:i32 width:i32 fractWidth:i32 -- )`
        RealFract() = 11,

        /// Real Item, with exponent width
        ///
        /// ### Stack Effect
        /// `( value:i32 width:i32 fractWidth:i32 expWidth:i32 -- )`
        RealExp() = 12,

        /// String Item
        ///
        /// ### Stack Effect
        /// `( value:addrint width:i32 -- )`
        String() = 13,

        /// Skip item, only printing a newline
        ///
        /// ### Stack Effect
        /// `( -- )`
        Skip() = 14,
    }
}

impl PutKind {
    pub fn has_exp_width_opt(self) -> bool {
        matches!(
            self,
            PutKind::IntExp() | PutKind::NatExp() | PutKind::RealExp()
        )
    }

    pub fn has_fract_opt(self) -> bool {
        self.has_exp_width_opt()
            || matches!(
                self,
                PutKind::IntFract() | PutKind::NatFract() | PutKind::RealFract()
            )
    }
}

define_encodings! {
    #[derive(Debug, Clone, Copy)]
    #[allow(dead_code)] // We aren't using all of the variants right now
    pub enum GetKind {
        /// Boolean Item
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32 )`
        ///
        /// `typeSize` is always 1.
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint -- )`
        Boolean() = 0,

        /// Char Item
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32 )`
        ///
        /// `typeSize` is always 1.
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint -- )`
        Char() = 1,

        /// Char Item, with a specific value range
        ///
        /// The character value must be in the range `min .. max` (inclusive).
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32, min:i32, max:i32 )`
        ///
        /// `typeSize` is always 1.
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint -- )`
        CharRange(i32, i32) = 2,

        /// CharN Item
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32 )`
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint getWidth:i32 length:u32 -- )`
        CharN(u32) = 3,

        /// Enum Item
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32 )`
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint variantNames:addrint -- )`
        Enum(u32) = 4,

        /// Enum Item, with a specific value range
        ///
        /// The enum ordinal value must be in the range `min .. max` (inclusive).
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32, min:i32, max:i32 )`
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint variantNames:addrint -- )`
        EnumRange(u32, i32, i32) = 5,

        /// Int Item
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32 )`
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint -- )`
        Int(u32) = 6,

        /// Int Item, with a specific value range
        ///
        /// The value must be in the range `min .. max` (inclusive).
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32, min:i32, max:i32 )`
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint -- )`
        IntRange(u32, i32, i32) = 7,

        /// Nat Item
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32 )`
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint -- )`
        Nat(u32) = 8,

        /// Real Item
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32 )`
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint -- )`
        Real(u32) = 9,

        /// String Item, getting an exact number of characters from the stream
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32 )`
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint getWidth:u32 maxLength:u32 -- )`
        StringExact(u32) = 10,

        /// String Item, getting a full line from the stream
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32 )`
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint maxLength:u32 -- )`
        StringLine(u32) = 11,

        /// String Item, getting a full token from the stream
        ///
        /// ### Extra Operands
        ///
        /// `( typeSize:u32 )`
        ///
        /// ### Stack Effect
        ///
        /// `( storeTo:addrint maxLength:u32 -- )`
        StringToken(u32) = 12,

        /// Skip Item, skipping a full token from the stream
        ///
        /// ### Extra Operands
        ///
        /// `(  )`
        ///
        /// ### Stack Effect
        ///
        /// `( -- )`
        Skip() = 13,
    }
}

define_encodings! {
    /// Standard streams selectable from `SETSTDSTREAM`
    #[derive(Debug, Clone, Copy)]
    pub enum StdStream {
        /// Standard In (default handle = -2)
        Stdin() = 1,
        /// Standard Out (default handle = -1)
        Stdout() = 2,
    }
}

define_encodings! {
    /// Stream operations that can be used from `SETSTREAM`
    #[derive(Debug, Clone, Copy)]
    #[allow(dead_code)] // We aren't using all of the variants right now
    pub enum StreamKind {
        Seek() = 0,
        Get() = 1,
        Put() = 2,
        Read() = 3,
        Write() = 4,
    }
}

define_encodings! {
    #[derive(Debug, Clone, Copy)]
    #[allow(dead_code)] // We aren't using all of the variants right now
    pub enum CheckKind {
        /// Range check as part of an assignemt
        Assign() = 0,
        /// Negative or zero char(*) length
        DynChar() = 1,
        /// Value passed to `chr` is not in `[0,255]`
        Chr() = 2,
        /// After an arithmetic operation
        IntOverflow() = 3,
        /// Range check as part of an assignemt into a range
        RangeAssign() = 4,
        /// Computation of a for-loop step
        LoopStep() = 5,
        /// Asserting that `pred` isn't applied on the first element of a sequence
        Pred() = 6,
        /// Asserting that `succ` isn't applied on the last element of a sequence
        Succ() = 7,
        /// Asserting that a value is a valid tag for a given union
        TagValue() = 8,
        // Range check as part of range parameter passing
        ValueParam() = 9,
    }
}
