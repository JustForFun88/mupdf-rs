//! Link reconstruction for PDF merge operations.
//!
//! Reconstructs link annotations from extracted link data after pages are copied.
//! Uses MuPDF's Page::links() API for extraction and text-based creation for output.

use std::collections::HashMap;

use crate::pdf::PdfDocument;
use crate::Error;

use super::data::LinkDestination;
use super::extraction::extract_links_from_page;

/// Type alias for page mapping (source page index -> destination page index)
pub type PageMapping = HashMap<i32, i32>;

/// Reconstructs link annotations from source pages into destination pages.
/// This is Phase 2 of the link handling process, called after all pages are copied.
///
/// Uses MuPDF's Page::links() API (same as PyMuPDF's fz_load_links) for extraction,
/// ensuring consistent behavior with PyMuPDF's do_links() function.
///
/// # Arguments
///
/// * `dest` - Destination PDF document
/// * `src_doc` - Source PDF document
/// * `pages_to_insert` - Source page indices being inserted
/// * `first_dest_page` - Starting destination page index
/// * `page_mapping` - Mapping from source to destination page indices
///
/// # Returns
///
/// Returns `Ok(())` on success, or an `Error` on failure.
pub fn reconstruct_links(
    dest: &mut PdfDocument,
    src_doc: &PdfDocument,
    pages_to_insert: &[i32],
    first_dest_page: i32,
    page_mapping: &PageMapping,
) -> Result<(), Error> {
    const BATCH_SIZE: usize = 100; // Process 100 pages at a time

    for batch_start in (0..pages_to_insert.len()).step_by(BATCH_SIZE) {
        let batch_end = (batch_start + BATCH_SIZE).min(pages_to_insert.len());

        reconstruct_links_batch(
            dest,
            src_doc,
            &pages_to_insert[batch_start..batch_end],
            first_dest_page + batch_start as i32,
            page_mapping,
        )?;
    }

    Ok(())
}

/// Reconstructs links for a batch of pages.
fn reconstruct_links_batch(
    dest: &mut PdfDocument,
    src_doc: &PdfDocument,
    pages_batch: &[i32],
    first_dest_page: i32,
    page_mapping: &PageMapping,
) -> Result<(), Error> {
    for (insert_order, &src_page_no) in pages_batch.iter().enumerate() {
        let dest_page_no = first_dest_page + insert_order as i32;

        // Load source page as PdfPage to use links() API
        let src_page = src_doc.load_page(src_page_no)?;
        let src_pdf_page = crate::pdf::PdfPage::try_from(src_page)?;

        // Extract links using MuPDF's links() API (same as PyMuPDF's fz_load_links)
        // Named destinations are resolved to concrete page references during extraction
        let links = extract_links_from_page(&src_pdf_page, src_doc)?;

        if links.is_empty() {
            continue;
        }

        // Get the page transformation matrix and compute its inverse.
        // This matches PyMuPDF's: ctm = ~page_src.transformation_matrix (utils.py:2119)
        // The inverse transforms from fitz coordinate space to PDF coordinates.
        let ctm = src_pdf_page.ctm()?;
        let inv_ctm = ctm.invert();

        // Get destination page object
        let mut dest_page = dest.find_page(dest_page_no)?;

        // Get or create Annots array on destination page
        let mut annots = match dest_page.get_dict("Annots")? {
            Some(a) => a,
            None => dest.new_array()?,
        };

        // Reconstruct each link using text-based approach
        // Use a page xref cache to avoid repeated find_page calls
        let mut page_xref_cache = HashMap::new();

        for link in &links {
            // Skip internal links that point to pages not in the copied range
            // This matches PyMuPDF's: if l["kind"] == LINK_GOTO and (l["page"] not in pno_src): continue
            if let LinkDestination::Internal {
                page: src_target_page,
                ..
            } = &link.destination
            {
                if !page_mapping.contains_key(src_target_page) {
                    continue; // Target page not in copied pages
                }
            }

            // Create new link annotation from string (minimal stack usage!)
            let new_link = link.create_annotation(
                dest,
                &dest_page,
                &inv_ctm,
                page_mapping,
                &mut page_xref_cache,
            )?;

            // Add as indirect object
            let new_link_indirect = dest.add_object(&new_link)?;

            // Add to annots array
            annots.array_push(new_link_indirect)?;
        }

        // Set Annots array back on page
        if annots.len()? > 0 {
            dest_page.dict_put("Annots", annots)?;
        }
    }

    Ok(())
}
