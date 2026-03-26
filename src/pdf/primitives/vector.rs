//! Owned, NUL-terminated byte vector for PDF name and binary data.

use core::cmp::Ordering;
use core::ffi::{c_char, CStr};
use core::fmt;
use core::hash::{Hash, Hasher};
use core::ops::Deref;
use std::borrow::Borrow;

use super::repr::Repr;

/// An owned, 24-byte vector that always maintains a trailing NUL terminator.
///
/// It is built with several memory and performance optimizations:
///
/// * Stores up to 23 payload bytes inline (zero heap allocation), spilling longer
///   payloads to a reference-counted [`ecow::EcoVec<u8>`].
///
/// * Maintains an invisible trailing NUL terminator, allowing instant, allocation-free
///   conversion to `&CStr` via [`as_cstr`](Self::as_cstr).
///
/// * Allows fast cloning, performed either by copying the 24 inline bytes or atomically
///   incrementing a reference count.
///
/// * Enables the Rust compiler to use niche optimization, making the size of `Option<PdfVec>`
///   equal to `PdfVec`.
#[derive(Default, Clone)]
#[repr(transparent)]
pub struct PdfVec(pub(super) Repr);

impl PdfVec {
    /// Creates a `PdfVec` from a raw C string pointer.
    ///
    /// # Safety
    /// `ptr` must be a valid, NUL-terminated pointer.
    #[inline]
    pub unsafe fn from_raw_c_unchecked(ptr: *const c_char) -> Self {
        let c_str = unsafe { CStr::from_ptr(ptr) };
        Self::from(c_str)
    }

    /// Returns `true` if the data is heap-allocated.
    #[inline]
    pub fn is_heap(&self) -> bool {
        self.0.is_heap()
    }

    /// Returns `true` if the data is stored inline.
    #[inline]
    pub fn is_inline(&self) -> bool {
        !self.is_heap()
    }

    /// Returns the payload length in bytes (without the NUL terminator).
    #[inline]
    pub fn len(&self) -> usize {
        self.as_slice().len()
    }

    /// Returns `true` if the payload is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the payload bytes without the trailing NUL.
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        self
    }

    /// Returns the content as a `&CStr` without allocation.
    #[inline]
    pub fn as_cstr(&self) -> &CStr {
        self.0.as_cstr()
    }
}

impl From<&[u8]> for PdfVec {
    #[inline]
    fn from(value: &[u8]) -> Self {
        Self(Repr::from_slice(value))
    }
}

impl From<&Vec<u8>> for PdfVec {
    #[inline]
    fn from(bytes: &Vec<u8>) -> Self {
        Self::from(bytes.as_slice())
    }
}

impl From<&CStr> for PdfVec {
    #[inline]
    fn from(c_str: &CStr) -> Self {
        Self(Repr::from_cstr(c_str))
    }
}

impl Deref for PdfVec {
    type Target = [u8];

    /// Returns the payload bytes without the trailing NUL.
    #[inline]
    fn deref(&self) -> &Self::Target {
        self.0.to_bytes()
    }
}

impl AsRef<[u8]> for PdfVec {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self
    }
}

impl AsRef<CStr> for PdfVec {
    #[inline]
    fn as_ref(&self) -> &CStr {
        self.as_cstr()
    }
}

impl Borrow<[u8]> for PdfVec {
    #[inline]
    fn borrow(&self) -> &[u8] {
        self
    }
}

impl<T: AsRef<[u8]> + ?Sized> PartialEq<T> for PdfVec {
    #[inline]
    fn eq(&self, other: &T) -> bool {
        self.as_slice() == other.as_ref()
    }
}

impl Eq for PdfVec {}

impl_partial_eq! {
    impl PartialEq<PdfVec>     for &PdfVec  { |self, other| self.as_slice() == other.as_slice() }

    impl PartialEq<PdfVec>     for Vec<u8>  { |self, other| self.as_slice() == other.as_slice() }
    impl PartialEq<&PdfVec>    for Vec<u8>  { |self, other| self.as_slice() == other.as_slice() }
    impl PartialEq<PdfVec>     for &Vec<u8> { |self, other| self.as_slice() == other.as_slice() }
    impl PartialEq<Vec<u8>>    for &PdfVec  { |self, other| self.as_slice() == other.as_slice() }

    impl PartialEq<PdfVec>     for [u8]     { |self, other| self   == other.as_slice() }
    impl PartialEq<&PdfVec>    for [u8]     { |self, other| self   == other.as_slice() }
    impl PartialEq<PdfVec>     for &[u8]    { |self, other| *self  == other.as_slice() }
    impl PartialEq<PdfVec>     for &&[u8]   { |self, other| **self == other.as_slice() }
}

impl PartialOrd for PdfVec {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PdfVec {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl Hash for PdfVec {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}

impl fmt::Debug for PdfVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("PdfVec").field(&self.as_slice()).finish()
    }
}
