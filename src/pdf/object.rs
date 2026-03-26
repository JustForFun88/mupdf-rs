use std::borrow::Cow;
use std::convert::TryFrom;
use std::ffi::{CStr, CString};
use std::fmt;
use std::io::{self, BufReader, Read, Write};
use std::slice;
use std::str::FromStr;

use mupdf_sys::*;

use super::primitives::{PdfString, PdfVec};
use crate::pdf::PdfDocument;
use crate::{context, Buffer, Error, Matrix};

pub trait IntoPdfDictKey {
    fn into_pdf_dict_key(self) -> Result<PdfObject, Error>;
}

impl IntoPdfDictKey for &str {
    fn into_pdf_dict_key(self) -> Result<PdfObject, Error> {
        PdfObject::new_name(self)
    }
}

impl IntoPdfDictKey for String {
    fn into_pdf_dict_key(self) -> Result<PdfObject, Error> {
        PdfObject::new_name(&self)
    }
}

impl IntoPdfDictKey for &CStr {
    fn into_pdf_dict_key(self) -> Result<PdfObject, Error> {
        PdfObject::new_name(self)
    }
}

impl IntoPdfDictKey for CString {
    fn into_pdf_dict_key(self) -> Result<PdfObject, Error> {
        PdfObject::new_name(&self)
    }
}

impl IntoPdfDictKey for PdfObject {
    fn into_pdf_dict_key(self) -> Result<PdfObject, Error> {
        Ok(self)
    }
}

impl IntoPdfDictKey for PdfString {
    #[inline]
    fn into_pdf_dict_key(self) -> Result<PdfObject, Error> {
        PdfObject::new_name(self)
    }
}

impl IntoPdfDictKey for &PdfString {
    #[inline]
    fn into_pdf_dict_key(self) -> Result<PdfObject, Error> {
        PdfObject::new_name(self)
    }
}

pub trait TryAsCStr {
    fn try_as_c_str(&self) -> Result<Cow<'_, CStr>, Error>;
}

impl TryAsCStr for str {
    #[inline]
    fn try_as_c_str(&self) -> Result<Cow<'_, CStr>, Error> {
        Ok(Cow::Owned(CString::new(self)?))
    }
}

impl TryAsCStr for String {
    #[inline]
    fn try_as_c_str(&self) -> Result<Cow<'_, CStr>, Error> {
        Ok(Cow::Owned(CString::new(self.as_str())?))
    }
}

impl TryAsCStr for CStr {
    #[inline]
    fn try_as_c_str(&self) -> Result<Cow<'_, CStr>, Error> {
        Ok(Cow::Borrowed(self))
    }
}

impl TryAsCStr for CString {
    #[inline]
    fn try_as_c_str(&self) -> Result<Cow<'_, CStr>, Error> {
        Ok(Cow::Borrowed(self))
    }
}

impl TryAsCStr for PdfString {
    #[inline]
    fn try_as_c_str(&self) -> Result<Cow<'_, CStr>, Error> {
        Ok(Cow::Borrowed(self.as_cstr()))
    }
}

impl<T: TryAsCStr + ?Sized> TryAsCStr for &T {
    #[inline]
    fn try_as_c_str(&self) -> Result<Cow<'_, CStr>, Error> {
        (**self).try_as_c_str()
    }
}

#[derive(Debug)]
pub struct PdfObject {
    pub(crate) inner: *mut pdf_obj,
}

impl PdfObject {
    pub(crate) unsafe fn from_raw(ptr: *mut pdf_obj) -> Self {
        Self { inner: ptr }
    }

    pub(crate) unsafe fn from_raw_keep_ref(ptr: *mut pdf_obj) -> Self {
        pdf_keep_obj(context(), ptr);
        Self { inner: ptr }
    }

    pub fn try_clone(&self) -> Result<Self, Error> {
        unsafe { ffi_try!(mupdf_pdf_clone_obj(context(), self.inner)) }.map(|inner| Self { inner })
    }

    pub fn new_null() -> PdfObject {
        unsafe {
            let inner = mupdf_pdf_new_null();
            PdfObject::from_raw(inner)
        }
    }

    pub fn new_bool(b: bool) -> PdfObject {
        unsafe {
            let inner = mupdf_pdf_new_bool(b);
            PdfObject::from_raw(inner)
        }
    }

    pub fn new_int(i: i32) -> Result<PdfObject, Error> {
        unsafe { ffi_try!(mupdf_pdf_new_int(context(), i)) }
            .map(|inner| unsafe { PdfObject::from_raw(inner) })
    }

    pub fn new_real(f: f32) -> Result<PdfObject, Error> {
        unsafe { ffi_try!(mupdf_pdf_new_real(context(), f)) }
            .map(|inner| unsafe { PdfObject::from_raw(inner) })
    }

    pub fn new_string(s: impl TryAsCStr) -> Result<PdfObject, Error> {
        let c_str = s.try_as_c_str()?;
        unsafe { ffi_try!(mupdf_pdf_new_string(context(), c_str.as_ptr())) }
            .map(|inner| unsafe { PdfObject::from_raw(inner) })
    }

    pub fn new_name(name: impl TryAsCStr) -> Result<PdfObject, Error> {
        let c_name = name.try_as_c_str()?;
        unsafe { ffi_try!(mupdf_pdf_new_name(context(), c_name.as_ptr())) }
            .map(|inner| unsafe { PdfObject::from_raw(inner) })
    }

    pub fn is_indirect(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_indirect(context(), self.inner)) }
    }

    pub fn is_null(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_null(context(), self.inner)) }
    }

    pub fn is_bool(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_bool(context(), self.inner)) }
    }

    pub fn is_int(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_int(context(), self.inner)) }
    }

    pub fn is_real(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_real(context(), self.inner)) }
    }

    pub fn is_number(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_number(context(), self.inner)) }
    }

    pub fn is_string(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_string(context(), self.inner)) }
    }

    pub fn is_name(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_name(context(), self.inner)) }
    }

    pub fn is_array(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_array(context(), self.inner)) }
    }

    pub fn is_dict(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_dict(context(), self.inner)) }
    }

    pub fn is_stream(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_is_stream(context(), self.inner)) }
    }

    pub fn as_bool(&self) -> Result<bool, Error> {
        unsafe { ffi_try!(mupdf_pdf_to_bool(context(), self.inner)) }
    }

    pub fn as_int(&self) -> Result<i32, Error> {
        unsafe { ffi_try!(mupdf_pdf_to_int(context(), self.inner)) }
    }

    pub fn as_float(&self) -> Result<f32, Error> {
        unsafe { ffi_try!(mupdf_pdf_to_float(context(), self.inner)) }
    }

    pub fn as_indirect(&self) -> Result<i32, Error> {
        unsafe { ffi_try!(mupdf_pdf_to_indirect(context(), self.inner)) }
    }

    pub fn as_name(&self) -> Result<PdfVec, Error> {
        let name_ptr = unsafe { ffi_try!(mupdf_pdf_to_name(context(), self.inner)) }?;
        Ok(PdfVec::from(unsafe { CStr::from_ptr(name_ptr) }))
    }

    /// Compare this name object to a string.
    /// Returns `false` if this object is not a name or does not match.
    pub fn name_eq(&self, name: impl AsRef<[u8]>) -> bool {
        self.as_name().is_ok_and(|bytes| bytes == name.as_ref())
    }

    pub fn as_string(&self) -> Result<PdfString, Error> {
        let str_ptr = unsafe { ffi_try!(mupdf_pdf_to_string(context(), self.inner)) }?;
        PdfString::try_from(unsafe { CStr::from_ptr(str_ptr) })
    }

    pub fn as_bytes(&self) -> Result<PdfVec, Error> {
        let mut len = 0;
        let ptr = unsafe { ffi_try!(mupdf_pdf_to_bytes(context(), self.inner, &mut len)) }?;
        Ok(PdfVec::from(unsafe { slice::from_raw_parts(ptr, len) }))
    }

    pub fn resolve(&self) -> Result<Option<Self>, Error> {
        let inner = unsafe { ffi_try!(mupdf_pdf_resolve_indirect(context(), self.inner)) }?;
        if inner.is_null() {
            return Ok(None);
        }
        Ok(Some(Self { inner }))
    }

    pub fn read_stream(&self) -> Result<Vec<u8>, Error> {
        let inner = unsafe { ffi_try!(mupdf_pdf_read_stream(context(), self.inner)) }?;
        let buf = unsafe { Buffer::from_raw(inner) };
        let buf_len = buf.len();
        let mut reader = BufReader::new(buf);
        let mut output = Vec::with_capacity(buf_len);
        reader.read_to_end(&mut output)?;
        Ok(output)
    }

    pub fn read_raw_stream(&self) -> Result<Vec<u8>, Error> {
        let inner = unsafe { ffi_try!(mupdf_pdf_read_raw_stream(context(), self.inner)) }?;
        let buf = unsafe { Buffer::from_raw(inner) };
        let buf_len = buf.len();
        let mut reader = BufReader::new(buf);
        let mut output = Vec::with_capacity(buf_len);
        reader.read_to_end(&mut output)?;
        Ok(output)
    }

    pub fn write_object(&mut self, obj: &PdfObject) -> Result<(), Error> {
        unsafe { ffi_try!(mupdf_pdf_write_object(context(), self.inner, obj.inner)) }
    }

    pub fn write_stream_buffer(&mut self, buf: &Buffer) -> Result<(), Error> {
        unsafe {
            ffi_try!(mupdf_pdf_write_stream_buffer(
                context(),
                self.inner,
                buf.inner,
                0
            ))
        }
    }

    pub fn write_stream_string(&mut self, string: &str) -> Result<(), Error> {
        let buf = Buffer::from_str(string)?;
        self.write_stream_buffer(&buf)
    }

    pub fn write_raw_stream_buffer(&mut self, buf: &Buffer) -> Result<(), Error> {
        unsafe {
            ffi_try!(mupdf_pdf_write_stream_buffer(
                context(),
                self.inner,
                buf.inner,
                1
            ))
        }
    }

    pub fn write_raw_stream_string(&mut self, string: &str) -> Result<(), Error> {
        let buf = Buffer::from_str(string)?;
        self.write_raw_stream_buffer(&buf)
    }

    pub fn get_array(&self, index: i32) -> Result<Option<Self>, Error> {
        let inner = unsafe { ffi_try!(mupdf_pdf_array_get(context(), self.inner, index)) }?;
        if inner.is_null() {
            return Ok(None);
        }
        Ok(Some(Self { inner }))
    }

    pub fn dict_len(&self) -> Result<usize, Error> {
        unsafe { ffi_try!(mupdf_pdf_dict_len(context(), self.inner)) }.map(|size| size as usize)
    }

    pub fn get_dict_val(&self, idx: i32) -> Result<Option<Self>, Error> {
        let inner = unsafe { ffi_try!(mupdf_pdf_dict_get_val(context(), self.inner, idx)) }?;
        if inner.is_null() {
            return Ok(None);
        }
        Ok(Some(Self { inner }))
    }
    pub fn get_dict_key(&self, idx: i32) -> Result<Option<Self>, Error> {
        let inner = unsafe { ffi_try!(mupdf_pdf_dict_get_key(context(), self.inner, idx)) }?;
        if inner.is_null() {
            return Ok(None);
        }
        Ok(Some(Self { inner }))
    }

    pub fn get_dict<K: IntoPdfDictKey>(&self, key: K) -> Result<Option<Self>, Error> {
        let key = key.into_pdf_dict_key()?;
        let inner = unsafe { ffi_try!(mupdf_pdf_dict_get(context(), self.inner, key.inner)) }?;
        if inner.is_null() {
            return Ok(None);
        }
        Ok(Some(Self { inner }))
    }

    /// Get a text string value from a dict entry by key.
    /// Returns `Ok(None)` if the key is not present.
    pub fn get_dict_string<K: IntoPdfDictKey>(&self, key: K) -> Result<Option<PdfString>, Error> {
        let key_obj = key.into_pdf_dict_key()?;
        let ptr = unsafe {
            ffi_try!(mupdf_pdf_dict_get_string(
                context(),
                self.inner,
                key_obj.inner
            ))
        }?;

        if ptr.is_null() {
            return Ok(None);
        }

        match PdfString::try_from(unsafe { CStr::from_ptr(ptr) }) {
            Ok(text) if !text.is_empty() => Ok(Some(text)),
            Ok(_) => Ok(None),
            Err(err) => Err(err),
        }
    }

    pub fn get_dict_inheritable<K: IntoPdfDictKey>(&self, key: K) -> Result<Option<Self>, Error> {
        let key = key.into_pdf_dict_key()?;
        let inner = unsafe {
            ffi_try!(mupdf_pdf_dict_get_inheritable(
                context(),
                self.inner,
                key.inner
            ))
        }?;
        if inner.is_null() {
            return Ok(None);
        }
        Ok(Some(Self { inner }))
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> Result<usize, Error> {
        unsafe { ffi_try!(mupdf_pdf_array_len(context(), self.inner)) }.map(|size| size as usize)
    }

    /// Creates a shallow copy of this array, resolving any indirect reference.
    ///
    /// Wraps `pdf_copy_array`: resolves `self` if indirect, then copies each
    /// element into a new direct array. Returns an error if `self` is not an
    /// array.
    pub fn copy_array(&self) -> Result<Self, Error> {
        unsafe { ffi_try!(mupdf_pdf_copy_array(context(), self.inner)) }
            .map(|inner| unsafe { Self::from_raw(inner) })
    }

    pub fn array_put(&mut self, index: i32, value: Self) -> Result<(), Error> {
        unsafe {
            ffi_try!(mupdf_pdf_array_put(
                context(),
                self.inner,
                index,
                value.inner
            ))
        }
    }

    pub fn array_push(&mut self, value: Self) -> Result<(), Error> {
        unsafe { ffi_try!(mupdf_pdf_array_push(context(), self.inner, value.inner)) }
    }

    pub(crate) fn array_push_ref(&mut self, value: &Self) -> Result<(), Error> {
        // From MUPDF (https://ghostscript.com/~robin/mupdf_explored.pdf, p. 238)
        // The array will take new references to the object passed in - that is, after the call,
        // both the array and the caller will hold references to the object. In cases where the
        // object to be inserted is a ‘borrowed’ reference, this is ideal.
        unsafe { ffi_try!(mupdf_pdf_array_push(context(), self.inner, value.inner)) }
    }

    pub fn array_delete(&mut self, index: i32) -> Result<(), Error> {
        unsafe { ffi_try!(mupdf_pdf_array_delete(context(), self.inner, index)) }
    }

    pub fn dict_put<K: IntoPdfDictKey>(&mut self, key: K, value: Self) -> Result<(), Error> {
        self.dict_put_ref(key, &value)
    }

    pub(crate) fn dict_put_ref<K: IntoPdfDictKey>(
        &mut self,
        key: K,
        value: &Self,
    ) -> Result<(), Error> {
        // The same as array_push_ref, look at
        // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-object.c#L2505
        let key_obj = key.into_pdf_dict_key()?;
        unsafe {
            ffi_try!(mupdf_pdf_dict_put(
                context(),
                self.inner,
                key_obj.inner,
                value.inner
            ))
        }
    }

    pub fn dict_put_name<K>(&mut self, key: K, name: impl TryAsCStr) -> Result<(), Error>
    where
        K: IntoPdfDictKey,
    {
        let key_obj = key.into_pdf_dict_key()?;
        let c_name = name.try_as_c_str()?;
        unsafe {
            ffi_try!(mupdf_pdf_dict_put_name(
                context(),
                self.inner,
                key_obj.inner,
                c_name.as_ptr()
            ))
        }
    }

    pub fn dict_put_int<K: IntoPdfDictKey>(&mut self, key: K, value: i32) -> Result<(), Error> {
        let key_obj = key.into_pdf_dict_key()?;
        unsafe {
            ffi_try!(mupdf_pdf_dict_put_int(
                context(),
                self.inner,
                key_obj.inner,
                value as i64
            ))
        }
    }

    pub fn dict_put_string<K, T>(&mut self, key: K, text: T) -> Result<(), Error>
    where
        K: IntoPdfDictKey,
        T: TryAsCStr,
    {
        let key_obj = key.into_pdf_dict_key()?;
        let c_text = text.try_as_c_str()?;
        unsafe {
            ffi_try!(mupdf_pdf_dict_put_string(
                context(),
                self.inner,
                key_obj.inner,
                c_text.as_ptr()
            ))
        }
    }

    pub fn dict_put_bool<K: IntoPdfDictKey>(&mut self, key: K, value: bool) -> Result<(), Error> {
        let key_obj = key.into_pdf_dict_key()?;
        unsafe {
            ffi_try!(mupdf_pdf_dict_put_bool(
                context(),
                self.inner,
                key_obj.inner,
                value as i32
            ))
        }
    }

    pub fn dict_put_real<K: IntoPdfDictKey>(&mut self, key: K, value: f32) -> Result<(), Error> {
        let key_obj = key.into_pdf_dict_key()?;
        unsafe {
            ffi_try!(mupdf_pdf_dict_put_real(
                context(),
                self.inner,
                key_obj.inner,
                value
            ))
        }
    }

    pub fn dict_put_array<K>(&mut self, key: K, capacity: i32) -> Result<PdfObject, Error>
    where
        K: IntoPdfDictKey,
    {
        let key_obj = key.into_pdf_dict_key()?;
        let inner = unsafe {
            ffi_try!(mupdf_pdf_dict_put_array(
                context(),
                self.inner,
                key_obj.inner,
                capacity
            ))
        }?;
        Ok(Self { inner })
    }

    pub fn dict_put_dict<K>(&mut self, key: K, capacity: i32) -> Result<PdfObject, Error>
    where
        K: IntoPdfDictKey,
    {
        let key_obj = key.into_pdf_dict_key()?;
        let inner = unsafe {
            ffi_try!(mupdf_pdf_dict_put_dict(
                context(),
                self.inner,
                key_obj.inner,
                capacity
            ))
        }?;
        Ok(Self { inner })
    }

    pub fn dict_delete<K: IntoPdfDictKey>(&mut self, key: K) -> Result<(), Error> {
        let key_obj = key.into_pdf_dict_key()?;
        unsafe { ffi_try!(mupdf_pdf_dict_delete(context(), self.inner, key_obj.inner)) }
    }

    fn print(&self, tight: bool, ascii: bool) -> Result<String, Error> {
        let ptr =
            unsafe { ffi_try!(mupdf_pdf_obj_to_string(context(), self.inner, tight, ascii)) }?;
        let c_str = unsafe { CStr::from_ptr(ptr) };
        let s = c_str.to_string_lossy().into_owned();
        unsafe { fz_free(context(), ptr.cast()) };
        Ok(s)
    }

    pub fn document(&self) -> Option<PdfDocument> {
        unsafe {
            let ptr = mupdf_pdf_get_bound_document(context(), self.inner);
            if ptr.is_null() {
                return None;
            }
            Some(PdfDocument::from_raw(ptr))
        }
    }

    pub fn page_ctm(&self) -> Result<Matrix, Error> {
        unsafe { ffi_try!(mupdf_pdf_page_obj_transform(context(), self.inner)) }.map(Into::into)
    }
}

impl Write for PdfObject {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let len = buf.len();
        let mut fz_buf = Buffer::with_capacity(len);
        fz_buf.write(buf)?;
        self.write_stream_buffer(&fz_buf)
            .map_err(io::Error::other)?;
        Ok(len)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl Drop for PdfObject {
    fn drop(&mut self) {
        if !self.inner.is_null() {
            unsafe {
                pdf_drop_obj(context(), self.inner);
            }
        }
    }
}

impl Clone for PdfObject {
    fn clone(&self) -> PdfObject {
        self.try_clone().unwrap()
    }
}

impl fmt::Display for PdfObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self.print(true, false).unwrap();
        f.write_str(&s)
    }
}

impl From<bool> for PdfObject {
    fn from(b: bool) -> PdfObject {
        PdfObject::new_bool(b)
    }
}

impl TryFrom<i32> for PdfObject {
    type Error = Error;

    fn try_from(i: i32) -> Result<PdfObject, Self::Error> {
        PdfObject::new_int(i)
    }
}

impl TryFrom<f32> for PdfObject {
    type Error = Error;

    fn try_from(f: f32) -> Result<PdfObject, Self::Error> {
        PdfObject::new_real(f)
    }
}

impl TryFrom<&str> for PdfObject {
    type Error = Error;

    fn try_from(s: &str) -> Result<PdfObject, Self::Error> {
        PdfObject::new_string(s)
    }
}

impl TryFrom<String> for PdfObject {
    type Error = Error;

    fn try_from(s: String) -> Result<PdfObject, Self::Error> {
        PdfObject::new_string(&s)
    }
}
