//! Owned, NUL-terminated byte/string types for safe MuPDF FFI.
//!
//! [`PdfVec`] and [`PdfString`] are 24 bytes. They store up to 23 payload
//! bytes inline, spilling to the heap for larger payloads. Heap storage uses
//! [`ecow::EcoVec<u8>`] because of its compact size (just two machine words)
//! and clone-on-write semantics, making clones of the reference-counted
//! allocation extremely cheap.
//!
//! A trailing NUL is always maintained for zero-allocation `&CStr` conversions,
//! and niche optimization ensures `Option<PdfString>` requires no extra memory.

pub use string::PdfString;
pub use vector::PdfVec;

use core::mem::{align_of, size_of};

mod repr;

/// Generates `PartialEq` implementations.
macro_rules! impl_partial_eq {
    ($(impl PartialEq<$rhs:ty> for $lhs:ty { |$self_:ident, $other:ident| $body:expr })*) => {$(
        impl PartialEq<$rhs> for $lhs {
            #[inline]
            fn eq(&$self_, $other: &$rhs) -> bool {
                $body
            }
        }
    )*};
}

pub mod string;
pub mod vector;

#[cfg(test)]
mod tests;

use repr::{Repr, INLINE_TOTAL};

// Size guarantees
const _: () = assert!(size_of::<PdfString>() == INLINE_TOTAL);
const _: () = assert!(size_of::<PdfString>() == size_of::<PdfVec>());

// Alignment guarantees
const _: () = assert!(align_of::<PdfString>() == align_of::<Repr>());
const _: () = assert!(align_of::<PdfString>() == align_of::<PdfVec>());

// Niche optimization guarantees
const _: () = assert!(size_of::<Option<PdfString>>() == size_of::<PdfString>());
const _: () = assert!(size_of::<Result<PdfString, ()>>() == size_of::<PdfString>());
const _: () = assert!(size_of::<Option<PdfVec>>() == size_of::<PdfVec>());
const _: () = assert!(size_of::<Result<PdfVec, ()>>() == size_of::<PdfVec>());
