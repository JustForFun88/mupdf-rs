use std::{
    convert::TryFrom,
    ffi::{c_int, c_uint, CStr, CString},
};

use mupdf_sys::*;

use crate::pdf::links::{
    parse_link_action_from_annot_dict, set_link_action_on_annot_dict, DestPageResolver, LinkAction,
    SingleResolver,
};
use crate::pdf::{PdfDocument, PdfObject};
use crate::{color::AnnotationColor, pdf::Intent};
use crate::{context, from_enum, Error};
use crate::{pdf::PdfFilterOptions, Point, Rect};

from_enum! { pdf_annot_type => c_uint,
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum PdfAnnotationType {
        Text = PDF_ANNOT_TEXT,
        Link = PDF_ANNOT_LINK,
        FreeText = PDF_ANNOT_FREE_TEXT,
        Line = PDF_ANNOT_LINE,
        Square = PDF_ANNOT_SQUARE,
        Circle = PDF_ANNOT_CIRCLE,
        Polygon = PDF_ANNOT_POLYGON,
        PloyLine = PDF_ANNOT_POLY_LINE,
        Highlight = PDF_ANNOT_HIGHLIGHT,
        Underline = PDF_ANNOT_UNDERLINE,
        Squiggly = PDF_ANNOT_SQUIGGLY,
        StrikeOut = PDF_ANNOT_STRIKE_OUT,
        Redact = PDF_ANNOT_REDACT,
        Stamp = PDF_ANNOT_STAMP,
        Caret = PDF_ANNOT_CARET,
        Ink = PDF_ANNOT_INK,
        Popup = PDF_ANNOT_POPUP,
        FileAttachment = PDF_ANNOT_FILE_ATTACHMENT,
        Sound = PDF_ANNOT_SOUND,
        Movie = PDF_ANNOT_MOVIE,
        RichMedia = PDF_ANNOT_RICH_MEDIA,
        Widget = PDF_ANNOT_WIDGET,
        Screen = PDF_ANNOT_SCREEN,
        PrinterMark = PDF_ANNOT_PRINTER_MARK,
        TrapNet = PDF_ANNOT_TRAP_NET,
        Watermark = PDF_ANNOT_WATERMARK,
        ThreeD = PDF_ANNOT_3D,
        Projection = PDF_ANNOT_PROJECTION,
        Unknown = PDF_ANNOT_UNKNOWN,
    }
}

from_enum! { pdf_line_ending => c_uint,
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum LineEndingStyle {
        None = PDF_ANNOT_LE_NONE,
        Square = PDF_ANNOT_LE_SQUARE,
        Circle = PDF_ANNOT_LE_CIRCLE,
        Diamond = PDF_ANNOT_LE_DIAMOND,
        OpenArrow = PDF_ANNOT_LE_OPEN_ARROW,
        ClosedArrow = PDF_ANNOT_LE_CLOSED_ARROW,
        Butt = PDF_ANNOT_LE_BUTT,
        ROpenArrow = PDF_ANNOT_LE_R_OPEN_ARROW,
        RClosedArrow = PDF_ANNOT_LE_R_CLOSED_ARROW,
        Slash = PDF_ANNOT_LE_SLASH,
    }
}

#[derive(Debug)]
pub struct PdfAnnotation {
    pub(crate) inner: *mut pdf_annot,
}

impl PdfAnnotation {
    pub(crate) unsafe fn from_raw(ptr: *mut pdf_annot) -> Self {
        Self { inner: ptr }
    }

    /// Returns the underlying PDF object dictionary for this annotation.
    pub fn obj(&self) -> Result<PdfObject, Error> {
        let ptr = unsafe { ffi_try!(mupdf_pdf_annot_obj(context(), self.inner)) }?;
        Ok(unsafe { PdfObject::from_raw_keep_ref(ptr) })
    }

    /// Get the [`PdfAnnotationType`] of this annotation
    pub fn r#type(&self) -> Result<PdfAnnotationType, Error> {
        unsafe { ffi_try!(mupdf_pdf_annot_type(context(), self.inner)) }.map(|subtype| {
            PdfAnnotationType::try_from(subtype).unwrap_or(PdfAnnotationType::Unknown)
        })
    }

    /// Check if the annotation is hot (i.e. that the pointing device's cursor is hovering over the
    /// annotation)
    pub fn is_hot(&self) -> bool {
        unsafe { pdf_annot_hot(context(), self.inner) != 0 }
    }

    /// Make this "hot" (see [`Self::is_hot()`])
    pub fn set_hot(&mut self, hot: bool) {
        // Just kinda trusting it would be insane of them to throw here
        unsafe { pdf_set_annot_hot(context(), self.inner, i32::from(hot)) }
    }

    pub fn is_active(&self) -> bool {
        unsafe { pdf_annot_active(context(), self.inner) != 0 }
    }

    pub fn set_line(&mut self, start: Point, end: Point) -> Result<(), Error> {
        unsafe {
            ffi_try!(mupdf_pdf_set_annot_line(
                context(),
                self.inner,
                start.into(),
                end.into()
            ))
        }
    }

    pub fn set_color(&mut self, color: AnnotationColor) -> Result<(), Error> {
        unsafe {
            match color {
                AnnotationColor::Gray(g) => ffi_try!(mupdf_pdf_set_annot_color(
                    context(),
                    self.inner,
                    1,
                    [g].as_ptr()
                )),
                AnnotationColor::Rgb { red, green, blue } => ffi_try!(mupdf_pdf_set_annot_color(
                    context(),
                    self.inner,
                    3,
                    [red, green, blue].as_ptr()
                )),
                AnnotationColor::Cmyk {
                    cyan,
                    magenta,
                    yellow,
                    key,
                } => ffi_try!(mupdf_pdf_set_annot_color(
                    context(),
                    self.inner,
                    4,
                    [cyan, magenta, yellow, key].as_ptr()
                )),
            }
        }
    }

    pub fn set_flags(&mut self, flags: AnnotationFlags) -> Result<(), Error> {
        unsafe {
            ffi_try!(mupdf_pdf_set_annot_flags(
                context(),
                self.inner,
                flags.bits()
            ))
        }
    }

    /// Set the bounding box of the annotation
    pub fn set_rect(&mut self, rect: Rect) -> Result<(), Error> {
        unsafe { ffi_try!(mupdf_pdf_set_annot_rect(context(), self.inner, rect.into())) }
    }

    pub fn author(&self) -> Result<Option<&str>, Error> {
        let ptr = unsafe { ffi_try!(mupdf_pdf_annot_author(context(), self.inner)) }?;
        if ptr.is_null() {
            return Ok(None);
        }
        let c_str = unsafe { CStr::from_ptr(ptr) };
        Ok(Some(c_str.to_str().unwrap()))
    }

    pub fn set_author(&mut self, author: &str) -> Result<(), Error> {
        let c_author = CString::new(author)?;
        unsafe {
            ffi_try!(mupdf_pdf_set_annot_author(
                context(),
                self.inner,
                c_author.as_ptr()
            ))
        }
    }

    pub fn filter(&mut self, mut opt: PdfFilterOptions) -> Result<(), Error> {
        unsafe {
            ffi_try!(mupdf_pdf_filter_annot_contents(
                context(),
                self.inner,
                &raw mut opt.inner
            ))
        }
    }

    pub fn set_popup(&mut self, rect: Rect) -> Result<(), Error> {
        unsafe {
            ffi_try!(mupdf_pdf_set_annot_popup(
                context(),
                self.inner,
                fz_rect::from(rect)
            ))
        }
    }

    pub fn set_active(&mut self, active: bool) -> Result<(), Error> {
        unsafe {
            ffi_try!(mupdf_pdf_set_annot_active(
                context(),
                self.inner,
                c_int::from(active)
            ))
        }
    }

    pub fn set_border_width(&mut self, width: f32) -> Result<(), Error> {
        unsafe {
            ffi_try!(mupdf_pdf_set_annot_border_width(
                context(),
                self.inner,
                width
            ))
        }
    }

    pub fn set_intent(&mut self, intent: Intent) -> Result<(), Error> {
        unsafe {
            ffi_try!(mupdf_pdf_set_annot_intent(
                context(),
                self.inner,
                pdf_intent::from(intent)
            ))
        }
    }

    /// Reads the link action from this annotation's dictionary, preserving
    /// the `/Dest` vs `/A` distinction.
    ///
    /// Returns `Ok(None)` if this is not a Link annotation or has no recognizable action.
    ///
    /// `page_num` is the 0-based page number where this annotation lives, used to
    /// resolve relative `Named` actions (`PrevPage`, `NextPage`). Pass `None` if
    /// the page number is unknown. Absolute named actions (`FirstPage`, `LastPage`)
    /// will still be resolved.
    ///
    /// Unlike [`PdfPage::pdf_links`](crate::pdf::PdfPage::pdf_links), this method:
    /// - Does **not** resolve `named` destinations to page numbers
    /// - Preserves the `Launch(FileSpec::Url)` vs `Uri` distinction
    /// - Preserves whether the original entry was `/Dest` or `/A`
    pub fn get_link_action(
        &self,
        doc: &PdfDocument,
        page_num: Option<i32>,
    ) -> Result<Option<LinkAction>, Error> {
        if self.r#type()? != PdfAnnotationType::Link {
            return Ok(None);
        }
        let obj = self.obj()?;
        parse_link_action_from_annot_dict(&obj, doc, page_num)
    }

    /// Replaces the link action on this annotation.
    ///
    /// For [`LinkAction::Dest`], writes a `/Dest` entry directly and removes `/A`.
    /// For [`LinkAction::Action`], writes an `/A` dictionary and removes `/Dest`.
    ///
    /// `GoTo` destination coordinates are transformed from Fitz space to PDF
    /// user space using the destination page's CTM.
    ///
    /// # Errors
    ///
    /// Returns an error if this annotation is not of type Link.
    pub fn set_link_action(
        &mut self,
        doc: &mut PdfDocument,
        action: &LinkAction,
    ) -> Result<(), Error> {
        let mut resolver =
            SingleResolver::new(|page_obj: &PdfObject| Ok(page_obj.page_ctm()?.invert()));
        self.set_link_action_with_inv_ctm(doc, action, &mut resolver)
    }

    /// Like [`set_link_action`](Self::set_link_action) but with a caller-provided
    /// [`DestPageResolver`] for `GoTo` destination coordinate transformation.
    ///
    /// This mirrors the `resolver` parameter in
    /// [`PdfPage::add_links_with_inv_ctm`](crate::pdf::PdfPage::add_links_with_inv_ctm).
    pub fn set_link_action_with_inv_ctm(
        &mut self,
        doc: &mut PdfDocument,
        action: &LinkAction,
        resolver: &mut impl DestPageResolver,
    ) -> Result<(), Error> {
        if self.r#type()? != PdfAnnotationType::Link {
            return Err(Error::InvalidDestination(
                "set_link_action called on non-Link annotation".into(),
            ));
        }

        let mut obj = self.obj()?;
        // Remove conflicting entry from existing annotation dict
        match action {
            LinkAction::Action(_) => {
                let _ = obj.dict_delete("Dest");
            }
            LinkAction::Dest(_) => {
                let _ = obj.dict_delete("A");
            }
        }
        set_link_action_on_annot_dict(doc, &mut obj, action, resolver)
    }
}

impl Drop for PdfAnnotation {
    fn drop(&mut self) {
        if !self.inner.is_null() {
            unsafe {
                pdf_drop_annot(context(), self.inner);
            }
        }
    }
}

bitflags::bitflags! {
    pub struct AnnotationFlags: i32 {
        const IS_INVISIBLE = PDF_ANNOT_IS_INVISIBLE as _;
        const IS_HIDDEN = PDF_ANNOT_IS_HIDDEN as _;
        const IS_PRINT = PDF_ANNOT_IS_PRINT as _;
        const NO_ZOOM = PDF_ANNOT_IS_NO_ZOOM as _;
        const NO_ROTATE = PDF_ANNOT_IS_NO_ROTATE as _;
        const NO_VIEW = PDF_ANNOT_IS_NO_VIEW as _;
        const IS_READ_ONLY = PDF_ANNOT_IS_READ_ONLY as _;
        const IS_LOCKED = PDF_ANNOT_IS_LOCKED as _;
        const IS_TOGGLE_NO_VIEW = PDF_ANNOT_IS_TOGGLE_NO_VIEW as _;
        const IS_LOCKED_CONTENTS = PDF_ANNOT_IS_LOCKED_CONTENTS as _;
    }
}
