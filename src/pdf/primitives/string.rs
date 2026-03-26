//! Owned, NUL-terminated, UTF-8 validated string for PDF text values.

use core::ffi::{c_char, CStr};
use core::fmt;
use core::ops::Deref;
use std::borrow::{Borrow, Cow};
use std::ffi::CString;
use std::str::FromStr;

use super::vector::PdfVec;
use crate::Error;

/// An owned, 24-byte, UTF-8 validated string.
///
/// This is a wrapper around [`PdfVec`] that guarantees the payload is valid UTF-8 [String].
#[derive(Debug, Default, Clone, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct PdfString(PdfVec);

impl PdfString {
    /// Creates a `PdfString` from a `PdfVec`.
    ///
    /// # Safety
    /// The caller must ensure that `vec` contains valid UTF-8.
    #[inline]
    pub unsafe fn from_utf8_unchecked(vec: PdfVec) -> Self {
        Self(vec)
    }

    /// Constructs from a `CStr` without checking UTF-8 validity.
    ///
    /// # Safety
    /// The `CStr` must contain valid UTF-8 bytes.
    #[inline]
    pub unsafe fn from_utf8_cstr_unchecked(c_str: &CStr) -> Self {
        Self(PdfVec::from(c_str))
    }

    /// Returns `true` if the data is heap-allocated.
    #[inline]
    pub fn is_heap(&self) -> bool {
        self.0.is_heap()
    }

    /// Returns `true` if the data is stored inline.
    #[inline]
    pub fn is_inline(&self) -> bool {
        self.0.is_inline()
    }

    /// Returns the length in bytes (not chars).
    #[inline]
    pub fn len(&self) -> usize {
        self.as_bytes().len()
    }

    /// Returns `true` if the string is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the raw UTF-8 bytes without the trailing NUL.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_slice()
    }

    /// Returns the content as a `&CStr` without any allocation.
    #[inline]
    pub fn as_cstr(&self) -> &CStr {
        self.0.as_cstr()
    }

    /// Returns a raw pointer to the NUL-terminated C string.
    #[inline]
    pub fn as_ptr(&self) -> *const c_char {
        self.as_cstr().as_ptr()
    }

    /// Returns the content as a `&str`.
    #[inline]
    pub fn as_str(&self) -> &str {
        self
    }

    /// Converts `self` into a [`String`].
    ///
    /// This allocates a new `String` and copies the UTF-8 payload.
    #[inline]
    pub fn into_string(self) -> String {
        self.as_str().to_owned()
    }

    /// Consumes `self` and returns the underlying [`PdfVec`].
    #[inline]
    pub fn into_bytes(self) -> PdfVec {
        self.0
    }
}

impl From<&str> for PdfString {
    #[inline]
    fn from(s: &str) -> Self {
        Self(PdfVec::from(s.as_bytes()))
    }
}

impl From<&String> for PdfString {
    #[inline]
    fn from(s: &String) -> Self {
        Self::from(s.as_str())
    }
}

impl FromStr for PdfString {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from(s))
    }
}

impl From<PdfString> for String {
    #[inline]
    fn from(s: PdfString) -> Self {
        s.into_string()
    }
}

impl TryFrom<&CStr> for PdfString {
    type Error = Error;

    /// Validates UTF-8 then copies the bytes.
    #[inline]
    fn try_from(c_str: &CStr) -> Result<Self, Self::Error> {
        c_str.to_str().map_err(|_| Error::InvalidUtf8)?;
        // SAFETY: to_str() succeeded, so the bytes are valid UTF-8.
        Ok(unsafe { Self::from_utf8_cstr_unchecked(c_str) })
    }
}

impl TryFrom<&CString> for PdfString {
    type Error = Error;
    #[inline]
    fn try_from(s: &CString) -> Result<Self, Self::Error> {
        Self::try_from(s.as_c_str())
    }
}

impl TryFrom<PdfVec> for PdfString {
    type Error = Error;

    /// Creates a `PdfString` from a `PdfVec` with validation that vector contains valid UTF-8.
    #[inline]
    fn try_from(value: PdfVec) -> Result<Self, Self::Error> {
        str::from_utf8(&value).map_err(|_| Error::InvalidUtf8)?;
        Ok(PdfString(value))
    }
}

impl fmt::Display for PdfString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Deref for PdfString {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        // SAFETY: PdfString is always constructed from validated UTF-8.
        unsafe { str::from_utf8_unchecked(self.as_bytes()) }
    }
}

impl AsRef<[u8]> for PdfString {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl AsRef<CStr> for PdfString {
    #[inline]
    fn as_ref(&self) -> &CStr {
        self.as_cstr()
    }
}

impl AsRef<str> for PdfString {
    #[inline]
    fn as_ref(&self) -> &str {
        self
    }
}

impl Borrow<str> for PdfString {
    #[inline]
    fn borrow(&self) -> &str {
        self
    }
}

impl<T: AsRef<str> + ?Sized> PartialEq<T> for PdfString {
    #[inline]
    fn eq(&self, other: &T) -> bool {
        self.as_str() == other.as_ref()
    }
}

impl_partial_eq! {
    impl PartialEq<PdfString>     for &PdfString    { |self, other| self.as_str() == other.as_str() }

    impl PartialEq<PdfString>     for String        { |self, other| self.as_str() == other.as_str() }
    impl PartialEq<&PdfString>    for String        { |self, other| self.as_str() == other.as_str() }
    impl PartialEq<PdfString>     for &String       { |self, other| self.as_str() == other.as_str() }
    impl PartialEq<String>        for &PdfString    { |self, other| self.as_str() == other.as_str() }

    impl PartialEq<PdfString>     for str           { |self, other| self   == other.as_str() }
    impl PartialEq<&PdfString>    for str           { |self, other| self   == other.as_str() }
    impl PartialEq<PdfString>     for &str          { |self, other| *self  == other.as_str() }
    impl PartialEq<PdfString>     for &&str         { |self, other| **self == other.as_str() }

    impl PartialEq<PdfString>     for Cow<'_, str>  { |self, other| self  == other.as_str() }
    impl PartialEq<PdfString>     for &Cow<'_, str> { |self, other| *self == other.as_str() }
    impl PartialEq<Cow<'_, str>>  for &PdfString    { |self, other| self.as_str() == other  }
}
