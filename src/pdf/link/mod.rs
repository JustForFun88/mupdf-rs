//! PDF link extraction, creation, and reconstruction.
//!
//! This module provides functionality for working with PDF link annotations:
//!
//! - **Extraction**: Extract link data from PDF pages using MuPDF's Page::links() API
//! - **Creation**: Create link annotations using text-based PDF object creation
//! - **Reconstruction**: Reconstruct links after page copying/merging operations
//!
//! The approach matches PyMuPDF's do_links() implementation:
//! - Uses the same underlying C API (fz_load_links) for extraction
//! - Uses inverse CTM transformation for coordinate conversion
//! - Uses text-based annotation creation to minimize stack usage
//!
//! # Example
//!
//! ```ignore
//! use mupdf::pdf::{PdfDocument, PdfPage};
//! use mupdf::pdf::link::{extract_links_from_page, LinkDestination};
//!
//! let doc = PdfDocument::open("document.pdf")?;
//! let page = PdfPage::try_from(doc.load_page(0)?)?;
//!
//! let links = extract_links_from_page(&page, &doc)?;
//! for link in links {
//!     match &link.destination {
//!         LinkDestination::Uri(uri) => println!("External link: {}", uri),
//!         LinkDestination::Internal { page, .. } => println!("Internal link to page {}", page),
//!         _ => {}
//!     }
//! }
//! ```

mod data;
mod extraction;
mod format;
mod reconstruction;

pub use data::{LinkData, LinkDestination};
pub use extraction::{extract_links_from_page, is_external_link, is_valid_pdf_path, parse_external_link};
pub use format::{PdfNum, PdfRect, PdfText, PdfZoom};
pub use reconstruction::{reconstruct_links, PageMapping};
