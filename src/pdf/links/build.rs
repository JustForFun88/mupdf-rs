use super::{DestPageResolver, LinkAction, PdfAction, PdfLink};
use crate::pdf::{PdfDocument, PdfObject};
use crate::{Error, Matrix};

/// Builds a PDF link annotation dictionary from a [`PdfLink`] (see [PDF 32000-1:2008, 12.5.6.5]
/// for link annotations, [12.6.4] for action types).
///
/// This is the Rust analogue of MuPDF's [`pdf_set_link_uri`] -> [`pdf_new_action_from_link`]
/// flow. While MuPDF serializes actions to a URI string and then reconstructs the PDF
/// dictionary from that string, this function builds the action dictionary directly from the
/// structured [`PdfAction`] type.
///
/// # Action dictionary shapes
///
/// | Variant                   | `S` (type) | `D` entry (`URI` for `Uri`) | `F` entry     |
/// |---------------------------|------------|-----------------------------|---------------|
/// | `GoTo(Page { .. })`       | `GoTo`     | `[page_ref, /Kind, ...]`    | -             |
/// | `GoTo(Named(..))`         | `GoTo`     | `(name)`                    | -             |
/// | `Uri(..)`                 | `URI`      | `(uri)`                     | -             |
/// | `GoToR { .. }` (explicit) | `GoToR`    | `[page_int, /Kind, ...]`    | filespec dict |
/// | `GoToR { .. }` (named)    | `GoToR`    | `(name)`                    | filespec dict |
/// | `Launch(..)`              | `Launch`   | -                           | filespec dict |
///
/// where:
///
/// - `page_ref` is an indirect reference to the destination page object (local document)
/// - `page_int` is the zero-based page number as an integer (remote document)
/// - `/Kind` is the PDF destination type name (e.g. `/Fit`, `/XYZ`) followed by its parameters
///   (see [`crate::DestinationKind::encode_into`])
/// - `filespec dict` is a file specification dictionary built by [`super::FileSpec::encode_into`]
///
/// For `GoTo(Page { .. })`, destination coordinates are transformed from MuPDF page space back
/// to PDF default user space using the `resolver` (see [`crate::DestinationKind::transform`]).
/// For `GoToR`, coordinates are used as-is (already in PDF default user space).
///
/// # MuPDF source mapping
///
/// | Variant                   | MuPDF function(s)                                                          |
/// |---------------------------|----------------------------------------------------------------------------|
/// | `GoTo(Page { .. })`       | [`pdf_new_action_from_link`] (`#` branch) + [`pdf_new_dest_from_link`]     |
/// | `GoTo(Named(..))`         | [`pdf_new_action_from_link`] (`#` branch) + [`pdf_new_dest_from_link`]     |
/// | `Uri(..)`                 | [`pdf_new_action_from_link`] ([`fz_is_external_link`] branch)              |
/// | `GoToR { .. }`            | [`pdf_new_action_from_link`] (`file:` branch) + [`pdf_new_dest_from_link`] |
/// | `Launch(..)`              | (no direct MuPDF equivalent, see [PDF 32000-1:2008, 12.6.4.5])             |
/// | File spec                 | [`pdf_add_filespec`] / [`pdf_add_url_filespec`]                            |
///
/// [PDF 32000-1:2008, 12.5.6.5]: https://opensource.adobe.com/dc-acrobat-sdk-docs/pdfstandards/PDF32000_2008.pdf#G11.1951136
/// [12.6.4]: https://opensource.adobe.com/dc-acrobat-sdk-docs/pdfstandards/PDF32000_2008.pdf#G11.1697199
/// [PDF 32000-1:2008, 12.6.4.5]: https://opensource.adobe.com/dc-acrobat-sdk-docs/pdfstandards/PDF32000_2008.pdf#G11.1952224
/// [`pdf_set_link_uri`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L614
/// [`pdf_new_action_from_link`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1177
/// [`pdf_new_dest_from_link`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1286
/// [`fz_is_external_link`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/fitz/link.c#L68
/// [`pdf_add_filespec`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1223
/// [`pdf_add_url_filespec`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1268
pub(crate) fn build_link_annotation(
    doc: &mut PdfDocument,
    page_obj: &PdfObject,
    link: &PdfLink,
    annot_inv_ctm: &Option<Matrix>,
    resolver: &mut impl DestPageResolver,
) -> Result<PdfObject, Error> {
    let rect = annot_inv_ctm
        .as_ref()
        .map(|inv_ctm| link.bounds.transform(inv_ctm))
        .unwrap_or(link.bounds);

    let mut annot = doc.new_dict_with_capacity(5)?;
    annot.dict_put("Subtype", PdfObject::new_name("Link")?)?;

    let mut rect_array = doc.new_array_with_capacity(4)?;
    rect.encode_into(&mut rect_array)?;
    annot.dict_put("Rect", rect_array)?;

    let mut border_style = doc.new_dict_with_capacity(1)?;
    border_style.dict_put("W", PdfObject::new_int(0)?)?;
    annot.dict_put("BS", border_style)?;

    set_link_action_on_annot_dict(doc, &mut annot, &link.action, resolver)?;

    annot.dict_put_ref("P", page_obj)?;

    Ok(annot)
}

/// Dispatches a [`LinkAction`] to the appropriate builder function.
///
/// - [`LinkAction::Dest`] -> [`set_dest_on_annot_dict`] (writes `/Dest`)
/// - [`LinkAction::Action`] -> [`set_action_on_annot_dict`] (writes `/A`)
///
/// Note: Callers are responsible for removing conflicting entries (`/A` or `/Dest`)
/// when updating existing annotations.
pub(crate) fn set_link_action_on_annot_dict(
    doc: &mut PdfDocument,
    annot: &mut PdfObject,
    action: &LinkAction,
    resolver: &mut impl DestPageResolver,
) -> Result<(), Error> {
    match action {
        LinkAction::Action(pdf_action) => {
            set_action_on_annot_dict(doc, annot, pdf_action, resolver)
        }
        LinkAction::Dest(dest) => {
            let dest_obj = dest.encode_local(doc, resolver)?;
            annot.dict_put("Dest", dest_obj)
        }
    }
}

/// Builds and sets the `/A` action entry on an annotation dictionary from a [`PdfAction`].
///
/// For `GoTo(Page { .. })` destinations, coordinates are transformed from Fitz space
/// to PDF user space using the `resolver`. For `GoToR`, coordinates are used as-is.
///
/// Note: Callers are responsible for removing conflicting `/Dest` entries before
/// calling this function when updating existing annotations.
fn set_action_on_annot_dict(
    doc: &mut PdfDocument,
    annot: &mut PdfObject,
    action: &PdfAction,
    resolver: &mut impl DestPageResolver,
) -> Result<(), Error> {
    match action {
        PdfAction::GoTo(dest) => {
            let dest_obj = dest.encode_local(doc, resolver)?;
            // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1191
            let mut action = doc.new_dict_with_capacity(2)?;
            action.dict_put("S", PdfObject::new_name("GoTo")?)?;
            action.dict_put("D", dest_obj)?;
            annot.dict_put("A", action)
        }
        PdfAction::Uri(uri) => {
            // MuPDF reads a URI action and stores the URI string as-is, since URI entries are ASCII strings
            // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L545
            // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1205
            let mut action = doc.new_dict_with_capacity(2)?;
            action.dict_put("S", PdfObject::new_name("URI")?)?;
            action.dict_put("URI", PdfObject::new_string(uri)?)?;
            annot.dict_put("A", action)
        }
        PdfAction::GoToR { file, dest } => {
            // MuPDF: GoToR action uses destination + filespec
            // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1197
            let mut action = doc.new_dict_with_capacity(3)?;
            action.dict_put("S", PdfObject::new_name("GoToR")?)?;
            let dest_obj = dest.encode_remote(doc)?;
            action.dict_put("D", dest_obj)?;

            // Same as MuPDF `pdf_add_filespec_from_link` function
            // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1152
            let file_spec = file.encode_into(doc)?;

            action.dict_put("F", file_spec)?;
            annot.dict_put("A", action)
        }
        PdfAction::Launch(file) => {
            // Same as MuPDF `pdf_add_filespec_from_link` function
            // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1152
            let file_spec = file.encode_into(doc)?;
            // No direct MuPDF code, see PDF 32000-1:2008, section 12.6.4.5, Table 203.
            // https://opensource.adobe.com/dc-acrobat-sdk-docs/pdfstandards/PDF32000_2008.pdf#G11.1952224
            let mut action = doc.new_dict_with_capacity(2)?;
            action.dict_put("S", PdfObject::new_name("Launch")?)?;
            action.dict_put("F", file_spec)?;
            annot.dict_put("A", action)
        }
    }
}
