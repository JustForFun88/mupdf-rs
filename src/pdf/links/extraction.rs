//! Link extraction - Phase 1 of the link handling process.
//!
//! Extracts link data from source pages using MuPDF's fz_load_links() API.
//!
//! Named destination links are resolved to concrete page destinations using
//! MuPDF's Document::resolve_link() API. If resolution fails, the link is dropped.
//!
//! URI parsing uses a lightweight heuristic for scheme detection (loosely based
//! on RFC 3986) and the percent-encoding crate for proper UTF-8 decoding.

use percent_encoding::percent_decode_str;

use super::{FileSpec, PdfAction, PdfDestination};
use crate::DestinationKind;

/// Parses a [MuPDF-compatible] link URI string into a structured [`PdfAction`] based on the Adobe
/// specification ["Parameters for Opening PDF Files"](https://web.archive.org/web/20170921000830/http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/pdf_open_parameters.pdf)
/// from the Adobe Acrobat SDK, version 8.1.
///
/// This is the inverse of [`PdfAction`]'s [`fmt::Display`](std::fmt::Display) implementation.
/// It takes the URI string that MuPDF produces when reading link annotations (via
/// [`pdf_parse_link_action`]) and reconstructs the corresponding [`PdfAction`] variant.
///
/// # Input -> Output mapping
///
/// | Input pattern                                    | Output action                    |
/// |--------------------------------------------------|----------------------------------|
/// | `#page=<N><dest_params>`                         | `GoTo(Page { page: N-1, kind })` |
/// | `#nameddest=<encoded>`                           | `GoTo(Named(decoded_name))`      |
/// | `#<raw_name (encoded)>`                          | `GoTo(Named(decoded_name))`      |
/// | `file://<path>.pdf#<params>`                     | `GoToR { file: Path(..), dest }` |
/// | `file:<path>.pdf#<params>`                       | `GoToR { file: Path(..), dest }` |
/// | `<scheme>://<host>/<..>.pdf#<params>` (external) | `GoToR { file: Url(..), dest }`  |
/// | `<path>.pdf#<params>` (no scheme)                | `GoToR { file: Path(..), dest }` |
/// | `file:<path>` (non-PDF)                          | `Launch(Path(..))`               |
/// | `<local_path>` (non-external, non-PDF)           | `Launch(Path(..))`               |
/// | `<scheme>://<..>` (non-PDF, external)            | `Uri(uri)`                       |
///
/// where:
///
/// - `<N>` is a **1-based** page number (converted to 0-based)
/// - `<dest_params>` are `&view=` / `&zoom=` / `&viewrect=` parameters, parsed into a [`DestinationKind`]
///   (see [`pdf_new_explicit_dest_from_uri`] and "Parameters for Opening PDF Files")
/// - `<params>` in the `GoToR` rows are parsed as:
///   - explicit destination parameters
///   - a named destination
///   - or [`PdfDestination::default`]
///
/// File paths are percent-decoded and normalized (`.`/`..` resolved), matching MuPDF's
/// [`parse_file_uri_path`]. Named destinations are percent-decoded matching MuPDF's
/// [`fz_decode_uri_component`].
///
/// # Disambiguation
///
/// MuPDF flattens all PDF action types into a single URI string (via [`pdf_parse_link_action`]),
/// so this function uses heuristics to reconstruct the original action type:
///
/// - Fragment-only (`#...`) -> `GoTo`
/// - `.pdf` suffix in path -> `GoToR`
/// - `file:` scheme or non-external path -> `Launch`
/// - External URI (scheme length > 2, matching MuPDF's [`fz_is_external_link`]) -> `Uri`
///
/// A `Launch` with URL-based `FileSpec` (`FS`=`URL`) is indistinguishable from a `URI` action
/// in the flattened string, so all external non-PDF URIs map to [`PdfAction::Uri`].
///
/// # MuPDF source mapping
///
/// | Step                     | MuPDF function(s)                                                         |
/// |--------------------------|---------------------------------------------------------------------------|
/// | Overall reconstruct      | [`pdf_new_action_from_link`]                                              |
/// | `file:` detection        | [`is_file_uri`]                                                           |
/// | External link detection  | [`fz_is_external_link`]                                                   |
/// | Explicit dest parsing    | [`pdf_new_explicit_dest_from_uri`]                                        |
/// | Named dest parsing       | [`parse_uri_named_dest`]                                                  |
/// | File path decoding       | [`parse_file_uri_path`] -> [`fz_decode_uri_component`] + [`fz_cleanname`] |
/// | Named dest decoding      | [`fz_decode_uri_component`]                                               |
///
/// [MuPDF-compatible]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/include/mupdf/pdf/annot.h#L317
/// [`pdf_parse_link_action`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L519
/// [`pdf_new_action_from_link`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1177
/// [`is_file_uri`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L861
/// [`fz_is_external_link`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/fitz/link.c#L68
/// [`pdf_new_explicit_dest_from_uri`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L941
/// [`parse_uri_named_dest`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L908
/// [`parse_file_uri_path`]: https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L886
/// [`fz_decode_uri_component`]: https://github.com/ArtifexSoftware/mupdf/blob/b462c9bd31a7b023e4239b75c38f2e6098805c3e/source/fitz/string.c#L326
/// [`fz_cleanname`]: https://github.com/ArtifexSoftware/mupdf/blob/b462c9bd31a7b023e4239b75c38f2e6098805c3e/source/fitz/string.c#L469
pub(crate) fn parse_external_link(uri: &str) -> Option<PdfAction> {
    let uri = uri.trim();
    if uri.is_empty() {
        return None;
    }
    let (head, params) = uri
        .split_once('#')
        .map(|(head, params)| (head.trim(), params.trim()))
        .unwrap_or((uri, ""));

    // MuPDF: fragment-only URIs map to GoTo (`pdf_new_action_from_link` function)
    // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1189
    if head.is_empty() {
        return parse_explicit_params(params)
            .map(|(page, kind)| PdfDestination::Page { page, kind })
            // MuPDF: `parse_uri_named_dest` does decode
            // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L908
            .or_else(|| parse_name(params).map(PdfDestination::Named))
            .map(PdfAction::GoTo);
    }

    // MuPDF: `is_file_uri` checks for the "file:" scheme
    // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L861
    // If the path starts with "/", MuPDF adds the "file://" prefix; otherwise, it adds the "file:" prefix
    // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L1066
    let (link, is_explicit_file) = strip_prefix_icase(head, "file://")
        .or_else(|| strip_prefix_icase(head, "file:"))
        .map(|path| (path, true))
        .unwrap_or((head, false));

    if is_pdf_path(link) {
        let is_url = !is_explicit_file && is_external_link(link);

        let file = if is_url {
            FileSpec::Url(link.to_string())
        } else {
            // MuPDF: `parse_file_uri_path` does decode + cleanname
            // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L896
            FileSpec::Path(decode_and_clean_path(link))
        };

        let dest = if let Some((page, kind)) = parse_explicit_params(params) {
            PdfDestination::Page { page, kind }
        } else if let Some(name) = parse_name(params) {
            PdfDestination::Named(name)
        } else {
            PdfDestination::default()
        };

        return Some(PdfAction::GoToR { file, dest });
    }

    // MuPDF flattens `Launch` and `URI` actions into a single URI string, normalizing local files
    // to `file:` scheme or relative paths. We use heuristics to reconstruct the action type:
    //
    // 1. `file:` scheme or URIs that do not look like external links (local paths) -> `Launch`.
    // 2. URIs that look like external links (http, mailto, etc.) -> `Uri`.
    //
    // Note: The PDF specification allows a `Launch` with URL `FileSpec` (FS entry = URL).
    // However, it is impossible to distinguish a "Launch with URL FileSpec" from a standard
    // "URI Action" based solely on the flattened string provided by MuPDF. Therefore,
    // all detected files are mapped to `FileSpec::Path`, and all web links to `PdfAction::Uri`.
    //
    // Look at MuPDF's `pdf_parse_link_action`/`convert_file_spec_to_URI` functions:
    //
    // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L519
    // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L288
    let action = if is_explicit_file && !link.is_empty() {
        PdfAction::Launch(FileSpec::Path(decode_and_clean_path(link)))
    } else if !is_external_link(uri) {
        PdfAction::Launch(FileSpec::Path(decode_and_clean_path(uri)))
    } else {
        // URI entries are ASCII strings, so we return uri as it is
        // https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L545
        PdfAction::Uri(uri.to_string())
    };
    Some(action)
}

/// Rust port of `fz_is_external_link` function.
/// (<https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/fitz/link.c#L68>)
///
/// Checks if a string represents an external web URI (e.g., `http`, `mailto`).
///
/// # Heuristic
///
/// Checks for a colon (`:`) that appears at an index greater than 2 (i.e., the scheme length must be
/// at least 3 characters). This constraint is used to distinguish external links from
/// DOS/Windows drive letters (e.g., `C:` or `D:`).
pub(super) fn is_external_link(uri: &str) -> bool {
    match uri.find(':') {
        Some(pos) if pos > 2 => {
            if !uri.as_bytes()[0].is_ascii_alphabetic() {
                return false;
            }

            uri[1..pos]
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '+' || c == '-' || c == '.')
        }
        _ => false,
    }
}

/// Checks if the file path ends with .pdf (case-insensitive) and has at least one char name.
/// Heuristic for reconstructing GoToR/Launch from flattened URIs.
pub(super) fn is_pdf_path(file_name: &str) -> bool {
    Some(file_name)
        .filter(|s| s.len() > 3)
        .and_then(|s| s.get(s.len() - 4..))
        .is_some_and(|suffix| suffix.eq_ignore_ascii_case(".pdf"))
}

/// Parses explicit destination parameters from a fragment string into a page number and view kind.
///
/// # Supported Parameters
/// - page: The target page number (1-based in string, converted to 0-based).
/// - view: The zoom/view mode (Fit, FitH, FitV, etc.).
/// - zoom: XYZ zoom parameters.
/// - viewrect: Rectangle parameters for FitR.
///
/// # Behavior
/// - Iterates left-to-right.
/// - page parameter resets the view kind to Fit (matching Adobe spec).
/// - Returns None if the string is empty or no valid parameters are found.
///
/// This is the Rust analogue of MuPDF's logic found in `pdf_new_explicit_dest_from_uri` function
/// (<https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L941>).
fn parse_explicit_params(params: &str) -> Option<(u32, DestinationKind)> {
    if params.is_empty() {
        return None;
    }
    let mut page = None;
    let mut kind = None;

    for (k, v) in fragment_kv_pairs(params) {
        if k.eq_ignore_ascii_case("page") {
            if let Some(new_page) = parse_page_1based_to_0based(v) {
                page = Some(new_page);
                // reset view state on page change
                kind = Some(DestinationKind::default());
                continue;
            }
        }

        let new_kind = if k.eq_ignore_ascii_case("viewrect") {
            parse_viewrect(v)
        } else if k.eq_ignore_ascii_case("zoom") {
            Some(parse_zoom(v))
        } else if k.eq_ignore_ascii_case("view") {
            parse_view(v)
        } else {
            None
        };

        if let Some(k) = new_kind {
            kind = Some(k);
        }
    }

    match (page, kind) {
        (None, None) => None,
        (p, k) => Some((p.unwrap_or_default(), k.unwrap_or_default())),
    }
}

/// Extracts the destination name from a fragment string.
///
/// Handles both explicit `nameddest=Name` format and implicit raw names.
///
/// # Behavior
/// - If the string starts with `nameddest=` (case-insensitive), the prefix is stripped.
/// - Otherwise, the entire string is treated as the name.
/// - Returns `None` if the resulting name is empty.
///
/// This is the Rust analogue of MuPDF's logic found in `parse_uri_named_dest` function
/// (<https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L908>).
fn parse_name(fragment: &str) -> Option<String> {
    strip_prefix_icase(fragment, "nameddest=")
        .or(Some(fragment))
        .filter(|name| !name.is_empty())
        .map(decode_uri_component)
}

/// Iterates over key=value pairs in a URI fragment.
///
/// - Splits by & or #.
/// - Trims whitespace.
/// - Skips parts without an = sign.
fn fragment_kv_pairs(fragment: &str) -> impl Iterator<Item = (&str, &str)> {
    fragment
        .split(['&', '#'])
        .map(str::trim)
        .filter(|p| !p.is_empty())
        .filter_map(|part| part.split_once('=').map(|(k, v)| (k.trim(), v.trim())))
}

/// Parses a 1-based page number string to a 0-based integer.
/// Returns 0 if the result would be negative.
///
/// MuPDF: <https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L957>
fn parse_page_1based_to_0based(s: &str) -> Option<u32> {
    let n: i32 = s.parse().ok()?;
    let z = n.saturating_sub(1);
    Some(z.max(0) as u32)
}

/// Parses parameters for FitR (viewrect).
/// Requires 4 comma-separated floats (left, top, width, height).
///
/// MuPDF: <https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L963>
fn parse_viewrect(s: &str) -> Option<DestinationKind> {
    let mut floats = FloatParser::new(s);
    let x = floats.next()?;
    let y = floats.next()?;
    let w = floats.next()?;
    let h = floats.next()?;

    if w == 0.0 || h == 0.0 {
        return None;
    }

    Some(DestinationKind::FitR {
        left: x,
        bottom: y,
        right: x + w,
        top: y + h,
    })
}

/// Parses parameters for XYZ zoom.
/// Format: zoom=scale,left,top.
/// Scale <= 0 is normalized to 100%.
///
/// MuPDF: <https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L972>
fn parse_zoom(s: &str) -> DestinationKind {
    // zoom=scale[,left,top]
    let mut floats = FloatParser::new(s);
    let zoom = floats.next().map(|n| if n <= 0.0 { 100.0 } else { n });

    DestinationKind::XYZ {
        left: floats.next(),
        top: floats.next(),
        zoom,
    }
}

/// Parses parameters for standard views (Fit, FitH, FitV, etc.).
///
/// MuPDF: <https://github.com/ArtifexSoftware/mupdf/blob/60bf95d09f496ab67a5e4ea872bdd37a74b745fe/source/pdf/pdf-link.c#L983>
fn parse_view(s: &str) -> Option<DestinationKind> {
    if s.is_empty() {
        return None;
    }

    let mut iter = s.split(',').map(str::trim);
    if let Some(key) = iter.next() {
        if key.eq_ignore_ascii_case("Fit") {
            return Some(DestinationKind::Fit);
        } else if key.eq_ignore_ascii_case("FitB") {
            return Some(DestinationKind::FitB);
        }

        let val = iter
            .next()
            .and_then(|s| s.parse::<f32>().ok())
            .filter(|num| num.is_finite());

        if key.eq_ignore_ascii_case("FitH") {
            return Some(DestinationKind::FitH { top: val });
        } else if key.eq_ignore_ascii_case("FitBH") {
            return Some(DestinationKind::FitBH { top: val });
        } else if key.eq_ignore_ascii_case("FitV") {
            return Some(DestinationKind::FitV { left: val });
        } else if key.eq_ignore_ascii_case("FitBV") {
            return Some(DestinationKind::FitBV { left: val });
        }
    }

    None
}

/// Helper struct to parse valid, finite f32 values from a comma-separated string.
struct FloatParser<'a>(std::str::Split<'a, char>);

impl<'a> FloatParser<'a> {
    fn new(s: &'a str) -> Self {
        Self(s.split(','))
    }

    /// Returns the next float.
    /// Returns None if the next segment is empty, malformed, non-finite, or if the string ends.
    fn next(&mut self) -> Option<f32> {
        self.0
            .next()
            .and_then(|part| part.trim().parse::<f32>().ok())
            .filter(|num| num.is_finite())
    }
}

/// Helper to check and strip a prefix from a string in a case-insensitive manner.
/// Returns Some(remainder) if the prefix matches, otherwise None.
fn strip_prefix_icase<'a>(s: &'a str, pat: &str) -> Option<&'a str> {
    let len = pat.len();
    s.get(..len)
        .filter(|head| head.eq_ignore_ascii_case(pat))
        .and_then(|_| s.get(len..))
}

/// Decodes URL-encoded sequences and normalizes the path.
/// See [`decode_uri_component`] and [`cleanname`] documentation.
fn decode_and_clean_path(path: &str) -> String {
    let decoded = decode_uri_component(path);
    cleanname(&decoded)
}

/// Normalizes a path, removing . and .. segments and collapsing duplicate slashes.
///
/// Mirrors MuPDF's `fz_cleanname` behavior
/// (<https://github.com/ArtifexSoftware/mupdf/blob/b462c9bd31a7b023e4239b75c38f2e6098805c3e/source/fitz/string.c#L469>).
fn cleanname(name: &str) -> String {
    let rooted = name.starts_with('/');
    let mut parts: Vec<&str> = Vec::new();
    let mut dotdot_depth = 0; // how many ".." we've emitted (for relative paths)

    for component in name.split('/') {
        match component {
            "" | "." => continue,
            ".." => {
                if !rooted && parts.len() <= dotdot_depth {
                    // relative path and can't backtrack — keep the ".."
                    parts.push("..");
                    dotdot_depth += 1;
                } else if parts.len() > dotdot_depth {
                    parts.pop();
                }
                // rooted path: ".." at root is just ignored
            }
            part => parts.push(part),
        }
    }

    let joined = parts.join("/");

    if rooted {
        format!("/{joined}")
    } else if joined.is_empty() {
        ".".to_string()
    } else {
        joined
    }
}

/// Unescapes URL-encoded sequences (%XX) in a string.
///
/// Uses RFC 3986 compliant decoding via the percent-encoding crate.
/// Falls back to the original string if the decoded bytes are not valid UTF-8.
///
/// The same as MuPDF's `fz_decode_uri_component` function
/// (<https://github.com/ArtifexSoftware/mupdf/blob/b462c9bd31a7b023e4239b75c38f2e6098805c3e/source/fitz/string.c#L326>)
pub(super) fn decode_uri_component(s: &str) -> String {
    percent_decode_str(s)
        .decode_utf8()
        .map(|cow| cow.into_owned())
        .unwrap_or_else(|_| s.to_string())
}
