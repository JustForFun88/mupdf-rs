//! Link data types for PDF operations.
//!
//! These types represent link information in a format suitable for
//! extraction and reconstruction during PDF operations.

use std::collections::HashMap;

use crate::pdf::{PdfDocument, PdfObject};
use crate::{DestinationKind, Error, Matrix, Rect};

use super::format::{PdfDestKind, PdfRect, PdfText};

/// Extracted link data from a PDF page.
/// Contains all information needed to reconstruct the link in a destination document.
#[derive(Debug, Clone, PartialEq)]
pub struct LinkData {
    /// Link rectangle in fitz coordinate space
    pub bounds: Rect,
    /// Link destination information
    pub destination: LinkDestination,
}

impl LinkData {
    /// Creates a new link annotation from extracted link data using text-based approach.
    /// This matches PyMuPDF's strategy and avoids stack overflow issues.
    ///
    /// # Arguments
    ///
    /// * `doc` - Destination PDF document
    /// * `dest_page` - Destination page object (for P reference)
    /// * `inv_ctm` - Inverse CTM to transform from fitz to PDF coordinates
    /// * `page_mapping` - Mapping from source to destination page indices
    /// * `page_xref_cache` - Cache for page xrefs to avoid repeated lookups
    ///
    /// # Returns
    ///
    /// The created link annotation as a PdfObject.
    pub fn create_annotation(
        &self,
        doc: &mut PdfDocument,
        dest_page: &PdfObject,
        inv_ctm: &Matrix,
        page_mapping: &HashMap<i32, i32>,
        page_xref_cache: &mut HashMap<i32, i32>,
    ) -> Result<PdfObject, Error> {
        // Transform rect from fitz coordinates to PDF coordinates using inverse CTM
        // This matches PyMuPDF: r = lnk["from"] * ctm where ctm = ~page.transformation_matrix
        let rect = self.bounds.transform(inv_ctm);

        // Create PDF object string based on link type
        let mut buf = String::with_capacity(256);

        match &self.destination {
            LinkDestination::Internal { page, kind } => {
                // Map source page to destination page
                let dest_page_idx = page_mapping.get(page).copied().unwrap_or(*page);

                // Get page xref (with caching)
                let page_xref = if let Some(&xref) = page_xref_cache.get(&dest_page_idx) {
                    xref
                } else {
                    let page_obj = doc.find_page(dest_page_idx)?;
                    let xref = page_obj.as_indirect()?;
                    page_xref_cache.insert(dest_page_idx, xref);
                    xref
                };

                // Transform destination coordinates by inverse CTM, matching PyMuPDF:
                // p = lnk["to"] * ctm  (utils.py:2034)
                let transformed_kind = kind.transform(inv_ctm);

                write_goto_link(&mut buf, page_xref, &transformed_kind, &rect)
                    .expect("write to String cannot fail");
            }
            LinkDestination::Named(name) => {
                // Named destinations use Action/GoTo structure with string destination.
                // Matches PyMuPDF's annot_skel["named"] template.
                write_named_link(&mut buf, name, &rect).expect("write to String cannot fail");
            }
            LinkDestination::Uri(uri) => {
                write_uri_link(&mut buf, uri, &rect).expect("write to String cannot fail");
            }
            LinkDestination::Launch(file) => {
                write_launch_link(&mut buf, file, &rect).expect("write to String cannot fail");
            }
            LinkDestination::Remote {
                is_url,
                file,
                page,
                kind,
                named_dest,
            } => {
                write_gotor_link(
                    &mut buf,
                    *is_url,
                    *page,
                    kind,
                    file,
                    named_dest.as_deref(),
                    &rect,
                )
                .expect("write to String cannot fail");
            }
        }

        // Parse string into PDF object (single operation - minimal stack usage!)
        let mut annot = doc.new_object_from_str(&buf)?;

        // Set P (page reference) - required field not in template
        annot.dict_put("P", dest_page.clone())?;

        Ok(annot)
    }
}

/// Link destination types extracted from PDF annotations.
#[derive(Debug, Clone)]
pub enum LinkDestination {
    /// Internal page destination with page index and destination kind
    Internal { page: i32, kind: DestinationKind },
    /// Named destination (string reference resolved at view time)
    Named(String),
    /// External URI link
    Uri(String),
    /// Launch external file
    Launch(String),
    /// Go to remote document with page number or named destination.
    /// When `page < 0`, the link uses a named destination specified in `named_dest`.
    Remote {
        is_url: bool,
        file: String,
        /// Page number (0-based), or -1 for named destination
        page: i32,
        /// Destination kind (XYZ, Fit, etc.) - used when page >= 0
        kind: DestinationKind,
        /// Named destination string - used when page < 0
        named_dest: Option<String>,
    },
}

impl PartialEq for LinkDestination {
    fn eq(&self, other: &Self) -> bool {
        let eq_f32 = |a: f32, b: f32| -> bool { (a.is_nan() && b.is_nan()) || a == b };

        let eq_opt_f32 = |a: Option<f32>, b: Option<f32>| -> bool {
            match (a, b) {
                (None, None) => true,
                (Some(va), Some(vb)) => eq_f32(va, vb),
                _ => false,
            }
        };

        let eq_kind = |k1: &DestinationKind, k2: &DestinationKind| -> bool {
            match (k1, k2) {
                (DestinationKind::Fit, DestinationKind::Fit) => true,
                (DestinationKind::FitB, DestinationKind::FitB) => true,
                (DestinationKind::FitH { top: t1 }, DestinationKind::FitH { top: t2 }) => {
                    eq_opt_f32(*t1, *t2)
                }
                (DestinationKind::FitBH { top: t1 }, DestinationKind::FitBH { top: t2 }) => {
                    eq_opt_f32(*t1, *t2)
                }
                (DestinationKind::FitV { left: l1 }, DestinationKind::FitV { left: l2 }) => {
                    eq_opt_f32(*l1, *l2)
                }
                (DestinationKind::FitBV { left: l1 }, DestinationKind::FitBV { left: l2 }) => {
                    eq_opt_f32(*l1, *l2)
                }
                (
                    DestinationKind::XYZ {
                        left: l1,
                        top: t1,
                        zoom: z1,
                    },
                    DestinationKind::XYZ {
                        left: l2,
                        top: t2,
                        zoom: z2,
                    },
                ) => eq_opt_f32(*l1, *l2) && eq_opt_f32(*t1, *t2) && eq_opt_f32(*z1, *z2),
                (
                    DestinationKind::FitR {
                        left: l1,
                        bottom: b1,
                        right: r1,
                        top: t1,
                    },
                    DestinationKind::FitR {
                        left: l2,
                        bottom: b2,
                        right: r2,
                        top: t2,
                    },
                ) => eq_f32(*l1, *l2) && eq_f32(*b1, *b2) && eq_f32(*r1, *r2) && eq_f32(*t1, *t2),
                _ => false,
            }
        };

        match (self, other) {
            (Self::Internal { page: p1, kind: k1 }, Self::Internal { page: p2, kind: k2 }) => {
                p1 == p2 && eq_kind(k1, k2)
            }
            (Self::Named(n1), Self::Named(n2)) => n1 == n2,
            (Self::Uri(u1), Self::Uri(u2)) => u1 == u2,
            (Self::Launch(l1), Self::Launch(l2)) => l1 == l2,
            (
                Self::Remote {
                    is_url: u1,
                    file: f1,
                    page: p1,
                    kind: k1,
                    named_dest: nd1,
                },
                Self::Remote {
                    is_url: u2,
                    file: f2,
                    page: p2,
                    kind: k2,
                    named_dest: nd2,
                },
            ) => u1 == u2 && f1 == f2 && p1 == p2 && nd1 == nd2 && eq_kind(k1, k2),
            _ => false,
        }
    }
}

// ==========================================================================
// Helper Functions for PDF link string generation
// ==========================================================================

/// Writes GoTo link annotation (internal link).
/// Matches PyMuPDF's annot_skel["goto1"] template.
fn write_goto_link(
    w: &mut impl std::fmt::Write,
    page_xref: i32,
    kind: &DestinationKind,
    rect: &Rect,
) -> std::fmt::Result {
    write!(
        w,
        "<</A<</S/GoTo/D[{} 0 R{}]>>/Rect[{}]/BS<</W 0>>/Subtype/Link>>",
        page_xref,
        PdfDestKind(kind),
        PdfRect(rect)
    )
}

/// Writes named destination link annotation.
/// Matches PyMuPDF's annot_skel["goto2"] template.
fn write_named_link(w: &mut impl std::fmt::Write, name: &str, rect: &Rect) -> std::fmt::Result {
    write!(
        w,
        "<</A<</S/GoTo/D{}>>/Rect[{}]/BS<</W 0>>/Subtype/Link>>",
        PdfText(name),
        PdfRect(rect)
    )
}

/// Writes URI link annotation.
/// Matches PyMuPDF's annot_skel["uri"] template.
fn write_uri_link(w: &mut impl std::fmt::Write, uri: &str, rect: &Rect) -> std::fmt::Result {
    write!(
        w,
        "<</A<</S/URI/URI{}>>/Rect[{}]/BS<</W 0>>/Subtype/Link>>",
        PdfText(uri),
        PdfRect(rect)
    )
}

/// Writes Launch link annotation.
/// Matches PyMuPDF's annot_skel["launch"] template.
fn write_launch_link(w: &mut impl std::fmt::Write, file: &str, rect: &Rect) -> std::fmt::Result {
    write!(
        w,
        "<</A<</S/Launch/F<</F{0}/UF{0}/Type/Filespec>>>>/Rect[{1}]/BS<</W 0>>/Subtype/Link>>",
        PdfText(file),
        PdfRect(rect)
    )
}

/// Writes GoToR (remote) link annotation.
/// Matches PyMuPDF's annot_skel["gotor1"] and ["gotor2"] templates.
///
/// # Arguments
/// * `page` - Page number (0-based), or -1 for named destination
/// * `kind` - Destination kind (used when page >= 0)
/// * `file` - Remote file path
/// * `named_dest` - Named destination string (used when page < 0)
/// * `rect` - Link rectangle
fn write_gotor_link(
    w: &mut impl std::fmt::Write,
    is_url: bool,
    page: i32,
    kind: &DestinationKind,
    file: &str,
    named_dest: Option<&str>,
    rect: &Rect,
) -> std::fmt::Result {
    // 1. Header
    w.write_str("<</A<</S/GoToR")?;

    // 2. Destination (/D): Page index vs Named destination
    if page < 0 {
        if let Some(name) = named_dest {
            write!(w, "/D{}", PdfText(name))?;
        } else {
            // Fallback: no name provided, go to first page
            w.write_str("/D[0 /Fit]")?;
        }
    } else {
        // Explicit page number and view kind
        write!(w, "/D[{} {}]", page, PdfDestKind(kind))?;
    }

    // 3. File Specification (/F): URL vs Local File.
    // Format file specification based on type:
    // - URL: <</FS/URL/F(url)>>
    // - File: <</F(file)/UF(file)/Type/Filespec>>
    let file_str = PdfText(file);
    if is_url {
        // Simple URL file spec
        write!(w, "/F<</FS/URL/F{}>>", file_str)?;
    } else {
        // Full PDF file spec dictionary for local files
        // We reuse file_str for both /F (legacy) and /UF (unicode) keys
        write!(w, "/F<</F{0}/UF{0}/Type/Filespec>>", file_str)?;
    }

    // 4. Footer (Rect, Borders, Subtype)
    write!(w, ">>/Rect[{}]/BS<</W 0>>/Subtype/Link>>", PdfRect(rect))
}
