//! PDF formatting helpers for link serialization.
//!
//! This module provides types for formatting Rust values into PDF-compatible strings,
//! matching MuPDF's internal serialization behavior.

use std::ffi::CStr;
use std::fmt::{self, Write};
use std::os::raw::{c_char, c_double};

use crate::{DestinationKind, Rect};

/// Wrapper for formatting f32 values in PDF format.
///
/// Uses MuPDF's `fz_snprintf` with `"%g"` format specifier for exact parity.
/// Converts `f32::NAN` to `null` (PDF null object).
///
/// # MuPDF `NaN` Handling
///
/// MuPDF internally stores destination values as `f32`. The PDF standard permits certain
/// parameters (like zoom or coordinates in `XYZ` destinations) to be `null` or omitted
/// to indicate "unchanged". MuPDF represents these `null`/missing values as `f32::NAN`
/// in memory and reconstructs the `null` object during serialization.
///
/// Reading (see MuPDF source `pdf-link.c:139`):
///
/// ```c
/// destination->x = arg1 ? p.x : NAN;
/// destination->y = arg2 ? p.y : NAN;
/// ```
///
/// Writing (see MuPDF source `pdf-link.c:1365`):
///
/// ```c
/// if (isnan(p.x))
///     pdf_array_push(ctx, dest, PDF_NULL);
/// else
///     pdf_array_push_real(ctx, dest, p.x);
/// ```
///
/// # Behavior Summary (for x / y)
///
/// | PDF Output (`XYZ`) | MuPDF Internal Value | PdfNum         |
/// |--------------------|----------------------|----------------|
/// | `null y z`         | `f32::NAN`           | `null`         |
/// | `(missing) y z`    | `f32::NAN`           | `null`         |
/// | `x y z`            | `x`                  | `x`            |
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct PdfNum(pub f32);

impl fmt::Display for PdfNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_nan() {
            return f.write_str("null");
        }
        write_pdf_float(f, self.0)
    }
}

/// Wrapper for formatting strings as PDF string literals.
///
/// Handles encoding correctly matching PyMuPDF's `get_pdf_str`:
/// - ASCII-only: `(escaped_string)` with special chars escaped
/// - 8-bit chars (128-255): `(string)` with octal escapes `\nnn`
/// - Chars > 255: `<FEFF...>` UTF-16BE hex string with BOM
///
/// A PDF string literal includes delimiters `()` or `<>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct PdfText<'a>(pub &'a str);

impl fmt::Display for PdfText<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = self.0;
        if str.is_empty() {
            return f.write_str("()");
        }

        // UTF-16BE for chars > 255
        if str.chars().any(|c| c as u32 > 255) {
            f.write_str("<FEFF")?;
            for code_unit in str.encode_utf16() {
                write!(f, "{:04X}", code_unit)?;
            }
            return f.write_char('>');
        }

        // ASCII/Latin-1 with escapes
        f.write_char('(')?;
        for c in str.chars() {
            let code = c as u32;
            match code {
                0x80..=0xFF => write!(f, "\\{:03o}", code)?,
                0x20..=0x7E => match c {
                    '(' | ')' | '\\' => {
                        f.write_char('\\')?;
                        f.write_char(c)?;
                    }
                    _ => f.write_char(c)?,
                },
                _ => match c {
                    '\x08' => f.write_str("\\b")?,
                    '\t' => f.write_str("\\t")?,
                    '\n' => f.write_str("\\n")?,
                    '\x0C' => f.write_str("\\f")?,
                    '\r' => f.write_str("\\r")?,
                    _ => write!(f, "\\{:03o}", code)?,
                },
            }
        }
        f.write_char(')')
    }
}

/// Wrapper for formatting MuPDF zoom values to PDF scale factors.
///
/// MuPDF internally stores zoom as a percentage (e.g., `100.0` = 100% zoom),
/// whereas PDF `XYZ` destinations use a scale factor (e.g., `1.0` = 100% zoom).
///
/// # MuPDF Implementation Details
///
/// **Reading** (see MuPDF source `pdf-link.c:141`): Checks if the zoom argument exists.
/// If it is `0` or `null`, it defaults to `100` (100%).
///
/// ```c
/// destination->zoom = arg3 ? (arg3v > 0 ? (arg3v * 100) : 100) : NAN;
/// ```
///
/// **Writing** (see MuPDF source `pdf-link.c:1373`): Writes `null` if the zoom is `NaN`,
/// otherwise divides by 100.
///
/// ```c
/// if (isnan(val.zoom))
///     pdf_array_push(ctx, dest, PDF_NULL);
/// else
///     pdf_array_push_real(ctx, dest, val.zoom / 100);
/// ```
///
/// # Behavior Summary
///
/// | PDF Output (`XYZ`) | MuPDF Internal Value | format_zoom    |
/// |--------------------|----------------------|----------------|
/// | `x y null`         | `100.0`              | `null`         |
/// | `x y 0`            | `100.0`              | `null`         |
/// | `x y 1.0`          | `100.0`              | `null`         |
/// | `x y (missing)`    | `NaN`                | `null`         |
/// | `x y z             | `z`                  | `z`            |
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct PdfZoom(pub f32);

impl fmt::Display for PdfZoom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", PdfNum(self.0 / 100.0))
    }
}

/// Wrapper for formatting a Rect in PDF format.
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct PdfRect<'a>(pub &'a Rect);

impl fmt::Display for PdfRect<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_pdf_float(f, self.0.x0)?;
        f.write_char(' ')?;
        write_pdf_float(f, self.0.y0)?;
        f.write_char(' ')?;
        write_pdf_float(f, self.0.x1)?;
        f.write_char(' ')?;
        write_pdf_float(f, self.0.y1)
    }
}

/// Wrapper for formatting DestinationKind in PDF format.
///
/// Outputs the destination type and parameters (e.g., `/XYZ 0 0 null`).
/// Optional parameters are omitted when possible to preserve missing semantics.
/// Does NOT include page reference - caller must provide that.
///
/// IMPORTANT: Assumes `transform(inv_ctm)` has already been called on the kind.
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub(crate) struct PdfDestKind<'a>(pub &'a DestinationKind);

impl fmt::Display for PdfDestKind<'_> {
    /// IMPORTANT: We assume that transform(inv_ctm) has already been called before encoding,
    /// as is done in the current create_link_annotation code.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self.0 {
            DestinationKind::Fit => f.write_str("/Fit"),
            DestinationKind::FitB => f.write_str("/FitB"),
            DestinationKind::FitH { top } => write!(f, "/FitH {}", PdfOptNum(top)),
            DestinationKind::FitBH { top } => write!(f, "/FitBH {}", PdfOptNum(top)),
            DestinationKind::FitV { left } => write!(f, "/FitV {}", PdfOptNum(left)),
            DestinationKind::FitBV { left } => write!(f, "/FitBV {}", PdfOptNum(left)),
            DestinationKind::XYZ { left, top, zoom } => {
                write!(f, "/XYZ {} {} {}", PdfOptNum(left), PdfOptNum(top), PdfOptZoom(zoom))
            }
            DestinationKind::FitR {
                left,
                bottom,
                right,
                top,
            } => {
                write!(
                    f,
                    "/FitR {} {} {} {}",
                    PdfNum(left),
                    PdfNum(bottom),
                    PdfNum(right),
                    PdfNum(top)
                )
            }
        }
    }
}

/// Wrapper for formatting Option<f32> values in PDF format.
/// `None` is formatted as `null`.
#[derive(Debug, Clone, Copy, PartialEq)]
struct PdfOptNum(Option<f32>);

impl fmt::Display for PdfOptNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(v) if !v.is_nan() => write_pdf_float(f, v),
            _ => f.write_str("null"),
        }
    }
}

/// Wrapper for formatting Option<f32> zoom values in PDF format.
/// Converts from MuPDF percentage to PDF scale factor.
#[derive(Debug, Clone, Copy, PartialEq)]
struct PdfOptZoom(Option<f32>);

impl fmt::Display for PdfOptZoom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(v) if !v.is_nan() => write_pdf_float(f, v / 100.0),
            _ => f.write_str("null"),
        }
    }
}

/// Helper function to write an f32 using MuPDF's fz_snprintf "%g".
///
/// This isolates the unsafe C-interop and ensures exact parity with MuPDF's
/// serialization format without allocating intermediate strings.
#[inline(always)]
fn write_pdf_float(f: &mut fmt::Formatter, value: f32) -> fmt::Result {
    // 64 bytes is more than enough for %g (usually requires ~20 chars max)
    let mut buffer = [0u8; 64];

    unsafe {
        mupdf_sys::fz_snprintf(
            buffer.as_mut_ptr() as *mut c_char,
            buffer.len(),
            c"%g".as_ptr(),
            value as c_double, // C varargs promote float to double
        );

        // Create a CStr from the buffer (finds the null terminator)
        let s = CStr::from_ptr(buffer.as_ptr() as *const c_char);
        f.write_str(s.to_str().unwrap_or("0"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pdf_text_empty() {
        assert_eq!(format!("{}", PdfText("")), "()");
    }

    #[test]
    fn test_pdf_text_ascii() {
        assert_eq!(format!("{}", PdfText("Hello")), "(Hello)");
        assert_eq!(format!("{}", PdfText("Chapter 1")), "(Chapter 1)");
    }

    #[test]
    fn test_pdf_text_special_chars() {
        assert_eq!(format!("{}", PdfText("(test)")), "(\\(test\\))");
        assert_eq!(format!("{}", PdfText("a\\b")), "(a\\\\b)");
        assert_eq!(format!("{}", PdfText("a(b)c\\d")), "(a\\(b\\)c\\\\d)");
    }

    #[test]
    fn test_pdf_text_control_chars() {
        assert_eq!(format!("{}", PdfText("a\tb")), "(a\\tb)");
        assert_eq!(format!("{}", PdfText("a\nb")), "(a\\nb)");
        assert_eq!(format!("{}", PdfText("a\rb")), "(a\\rb)");
    }

    #[test]
    fn test_pdf_text_8bit_chars() {
        // Characters 128-255 use octal escapes
        // e.g., e-acute = U+00E9 (233)
        assert_eq!(format!("{}", PdfText("cafe\u{0301}")), "<FEFF00630061006600650301>");
    }

    #[test]
    fn test_pdf_text_unicode() {
        // Characters > 255 use UTF-16BE with BOM
        assert_eq!(format!("{}", PdfText("\u{4E2D}")), "<FEFF4E2D>");
        assert_eq!(format!("{}", PdfText("\u{4E2D}\u{6587}")), "<FEFF4E2D6587>");
        assert_eq!(
            format!("{}", PdfText("Hello\u{4E2D}\u{6587}")),
            "<FEFF00480065006C006C006F4E2D6587>"
        );
    }

    #[test]
    fn test_pdf_text_surrogate_pairs() {
        // Characters outside BMP use surrogate pairs in UTF-16
        // Musical G clef = U+1D11E = D834 DD1E in UTF-16
        assert_eq!(format!("{}", PdfText("\u{1D11E}")), "<FEFFD834DD1E>");
    }

    #[test]
    fn test_pdf_num_nan() {
        assert_eq!(format!("{}", PdfNum(f32::NAN)), "null");
    }

    #[test]
    fn test_pdf_num_normal() {
        assert_eq!(format!("{}", PdfNum(0.0)), "0");
        assert_eq!(format!("{}", PdfNum(1.0)), "1");
        assert_eq!(format!("{}", PdfNum(100.5)), "100.5");
    }

    #[test]
    fn test_pdf_zoom() {
        // 100% zoom -> 1.0 scale factor
        assert_eq!(format!("{}", PdfZoom(100.0)), "1");
        // 200% zoom -> 2.0 scale factor
        assert_eq!(format!("{}", PdfZoom(200.0)), "2");
        // 50% zoom -> 0.5 scale factor (MuPDF %g format omits leading zero)
        assert_eq!(format!("{}", PdfZoom(50.0)), ".5");
        // NaN zoom -> null
        assert_eq!(format!("{}", PdfZoom(f32::NAN)), "null");
    }
}
