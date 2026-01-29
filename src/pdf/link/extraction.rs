//! Link extraction from PDF pages.
//!
//! Extracts link data from source pages using MuPDF's Page::links() API.
//! This matches PyMuPDF's approach using fz_load_links().
//!
//! Named destination links are resolved to concrete page destinations using
//! MuPDF's Document::resolve_link() API. If resolution fails, the link is dropped.
//!
//! URI parsing uses a lightweight heuristic for scheme detection (loosely based
//! on RFC 3986) and the percent-encoding crate for proper UTF-8 decoding.

use percent_encoding::percent_decode_str;

use crate::pdf::{PdfDocument, PdfPage};
use crate::{DestinationKind, Error};

use super::data::{LinkData, LinkDestination};

/// Extracts link data from a source page using MuPDF's links() API.
///
/// This function serves as the high-level entry point for link extraction. It retrieves
/// raw links using the underlying C API (fz_load_links). The returned links possess
/// bounds in the Fitz coordinate space.
///
/// # Resolution Logic
/// 1. Internal Links: Converted directly using the destination page number.
/// 2. External URIs: Parsed via parse_external_link to determine if they are:
///    - Remote PDF Links: Links to other PDF files (potentially with page fragments).
///    - Launch Actions: Links to local files.
///    - Web URIs: Standard HTTP/HTTPS links.
/// 3. Named Destinations: If a link points to a named destination (e.g., #nameddest=Chapter1),
///    it attempts to resolve this name to a concrete page number using doc.resolve_link().
///    Unresolvable named destinations are ignored.
///
/// # Arguments
///
/// * `page` - The source page (provides access to the link annotations).
/// * `doc` - The source document (context required for resolving named destinations).
pub fn extract_links_from_page(page: &PdfPage, doc: &PdfDocument) -> Result<Vec<LinkData>, Error> {
    // Use Page::links() which wraps fz_load_links
    let links_iter = page.links()?;

    let mut links = Vec::new();

    for link in links_iter {
        let destination = match link.dest {
            Some(dest) => {
                let page_num = dest.loc.page_number as i32;
                LinkDestination::Internal {
                    page: page_num,
                    kind: normalize_internal_fit_r(doc, page_num, dest.kind),
                }
            }
            None => match parse_external_link(&link.uri) {
                Some(destination) => match destination {
                    // If it parsed as a Named destination, try to resolve it to a concrete page/location
                    LinkDestination::Named(_) => match doc.resolve_link(&link.uri) {
                        Ok(Some(dest)) => LinkDestination::Internal {
                            page: dest.loc.page_number as i32,
                            kind: normalize_internal_fit_r(
                                doc,
                                dest.loc.page_number as i32,
                                dest.kind,
                            ),
                        },
                        _ => continue, // Drop unresolvable named destinations
                    },
                    // Other destinations are used as-is
                    destination => destination,
                },
                None => continue,
            },
        };

        links.push(LinkData {
            bounds: link.bounds,
            destination,
        });
    }

    Ok(links)
}

fn normalize_internal_fit_r(
    doc: &PdfDocument,
    page: i32,
    kind: DestinationKind,
) -> DestinationKind {
    if let DestinationKind::FitR { .. } = kind {
        if let Ok(dest_page) = doc.load_page(page) {
            if let Ok(pdf_page) = crate::pdf::PdfPage::try_from(dest_page) {
                if let Ok(ctm) = pdf_page.ctm() {
                    return kind.transform(&ctm);
                }
            }
        }
    }
    kind
}

/// Parses a URI string into a structured LinkDestination.
///
/// This is the Rust analogue of MuPDF's pdf_new_explicit_dest_from_uri and logic found
/// in pdf_resolve_link_dest.
///
/// Parse a remote link URI string that returned by MuPdf according to the Adobe specification
/// "Parameters for Opening PDF files" from the Adobe Acrobat SDK, version 8.1, which can, at
/// the time of writing, be found here:
///
/// <https://web.archive.org/web/20170921000830/http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/pdf_open_parameters.pdf>
pub fn parse_external_link(uri: &str) -> Option<LinkDestination> {
    let uri = uri.trim();
    if uri.is_empty() {
        return None;
    }
    let (head, params) = uri
        .split_once('#')
        .map(|(head, params)| (head.trim(), params.trim()))
        .unwrap_or((uri, ""));

    if head.is_empty() {
        return parse_explicit_params(params)
            .map(|(page, kind)| LinkDestination::Internal { page, kind })
            .or_else(|| parse_name(params).map(|n| LinkDestination::Named(url_unescape(n))));
    }

    let (link, is_explicit_file) = strip_prefix_icase(head, "file://")
        .or_else(|| strip_prefix_icase(head, "file:"))
        .map(|path| (path, true))
        .unwrap_or((head, false));

    if is_valid_pdf_path(link) {
        let (page, kind, named_dest) = params
            .is_empty()
            .then_some((0, DestinationKind::Fit, None))
            .or_else(|| parse_explicit_params(params).map(|(page, kind)| (page, kind, None)))
            .unwrap_or_else(|| (-1, DestinationKind::Fit, parse_name(params)));

        return Some(LinkDestination::Remote {
            is_url: !is_explicit_file && is_external_link(link),
            file: url_unescape(link),
            page,
            kind,
            named_dest: named_dest.map(url_unescape),
        });
    }

    if is_explicit_file && !link.is_empty() {
        Some(LinkDestination::Launch(url_unescape(link)))
    } else if !is_external_link(uri) {
        Some(LinkDestination::Launch(url_unescape(uri)))
    } else {
        Some(LinkDestination::Uri(url_unescape(uri)))
    }
}

/// Checks if a string represents an external web URI (e.g., http, mailto).
///
/// # Heuristic
/// Checks for a colon (:) after the second character. The substring before the colon
/// must start with an alphabetic character and contain only alphanumeric characters,
/// +, -, or .. This loosely follows RFC 3986 scheme syntax.
pub fn is_external_link(uri: &str) -> bool {
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
pub fn is_valid_pdf_path(file_name: &str) -> bool {
    Some(file_name)
        .filter(|s| s.len() > 4)
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
fn parse_explicit_params(params: &str) -> Option<(i32, DestinationKind)> {
    if params.is_empty() {
        return None;
    }
    let mut page = None;
    let mut kind = None;

    for (k, v) in fragment_kv_pairs(params) {
        if k.eq_ignore_ascii_case("page") {
            if let Some(new_page) = parse_page_1based_to_0based(v) {
                page = Some(new_page);
                kind = Some(DestinationKind::Fit); // reset view state on page change
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
        (p, k) => Some((p.unwrap_or(0), k.unwrap_or(DestinationKind::Fit))),
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
fn parse_name(fragment: &str) -> Option<&str> {
    strip_prefix_icase(fragment, "nameddest=")
        .or(Some(fragment))
        .filter(|name| !name.is_empty())
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
fn parse_page_1based_to_0based(s: &str) -> Option<i32> {
    let n: i32 = s.parse().ok()?;
    let z = n.saturating_sub(1);
    Some(z.max(0))
}

/// Parses parameters for FitR (viewrect).
/// Requires 4 comma-separated floats (left, top, width, height).
fn parse_viewrect(s: &str) -> Option<DestinationKind> {
    let mut floats = parse_finite_floats(s);
    let left = floats.next()?;
    let top = floats.next()?;
    let width = floats.next()?;
    let height = floats.next()?;

    if width == 0.0 || height == 0.0 {
        return None;
    }

    Some(DestinationKind::FitR {
        left,
        top,
        right: left + width,
        bottom: top + height,
    })
}

/// Parses parameters for XYZ zoom.
/// Format: zoom=scale,left,top.
/// Scale <= 0 is normalized to 100%.
fn parse_zoom(s: &str) -> DestinationKind {
    // zoom=scale[,left,top]
    let mut floats = parse_finite_floats(s);
    let zoom = floats.next().map(|n| if n <= 0.0 { 100.0 } else { n });

    DestinationKind::XYZ {
        left: floats.next(),
        top: floats.next(),
        zoom,
    }
}

/// Parses parameters for standard views (Fit, FitH, FitV, etc.).
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

        let val = iter.next().and_then(|s| s.parse::<f32>().ok());

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

/// Helper iterator to extract valid, finite f32 values from a comma-separated string.
/// Skips malformed numbers and non-finite values (NaN, Inf).
fn parse_finite_floats(s: &str) -> impl Iterator<Item = f32> + '_ {
    s.split(',')
        .map(str::trim)
        .flat_map(str::parse::<f32>)
        .filter(|num| num.is_finite())
}

/// Helper to check and strip a prefix from a string in a case-insensitive manner.
/// Returns Some(remainder) if the prefix matches, otherwise None.
fn strip_prefix_icase<'a>(s: &'a str, pat: &str) -> Option<&'a str> {
    let len = pat.len();
    s.get(..len)
        .filter(|head| head.eq_ignore_ascii_case(pat))
        .and_then(|_| s.get(len..))
}

/// Unescapes URL-encoded sequences (%XX) in a string.
///
/// Uses RFC 3986 compliant decoding via the percent-encoding crate.
/// Handles the double-percent %% quirk found in some PDF generators (compatibility with PyMuPDF)
/// by replacing it with %25. Falls back to the original string if UTF-8 decoding fails.
fn url_unescape(s: &str) -> String {
    // Handle escaped percent signs first (PyMuPDF compatibility)
    let s = s.replace("%%", "%25");

    percent_decode_str(&s)
        .decode_utf8()
        .map(|cow| cow.into_owned())
        .unwrap_or_else(|_| s.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_url_unescape() {
        // Simple ASCII percent-encoding
        assert_eq!(url_unescape("hello%20world"), "hello world");
        assert_eq!(url_unescape("test%2Fpath"), "test/path");
        assert_eq!(url_unescape("name%3Dvalue"), "name=value");

        // UTF-8 multi-byte sequences (Chinese character "中")
        assert_eq!(url_unescape("%E4%B8%AD"), "\u{4E2D}");
        // Japanese hiragana "あ"
        assert_eq!(url_unescape("%E3%81%82"), "\u{3042}");
        // Mixed ASCII and UTF-8
        assert_eq!(url_unescape("hello%E4%B8%AD%E6%96%87"), "hello\u{4E2D}\u{6587}");

        // PyMuPDF compatibility: %% becomes %25 then decodes to %
        assert_eq!(url_unescape("100%%"), "100%");
        assert_eq!(url_unescape("%%25"), "%25");

        // Strings without percent-encoding should pass through unchanged
        assert_eq!(url_unescape("hello"), "hello");
        assert_eq!(url_unescape("Chapter1"), "Chapter1");
        assert_eq!(url_unescape(""), "");

        // Invalid UTF-8 sequence should fall back to original string
        // %FF%FE is not valid UTF-8, so we expect the original back
        assert_eq!(url_unescape("%FF%FE"), "%FF%FE");
    }

    #[test]
    fn test_parse_uri_schemes() {
        match parse_external_link("http://example.com/page") {
            Some(LinkDestination::Uri(uri)) => assert_eq!(uri, "http://example.com/page"),
            other => panic!("Expected Uri, got {:?}", other),
        }

        match parse_external_link("https://example.com/secure") {
            Some(LinkDestination::Uri(uri)) => assert_eq!(uri, "https://example.com/secure"),
            other => panic!("Expected Uri, got {:?}", other),
        }

        match parse_external_link("mailto:user@example.com") {
            Some(LinkDestination::Uri(uri)) => assert_eq!(uri, "mailto:user@example.com"),
            other => panic!("Expected Uri, got {:?}", other),
        }

        match parse_external_link("ftp://ftp.example.com/file.txt") {
            Some(LinkDestination::Uri(uri)) => assert_eq!(uri, "ftp://ftp.example.com/file.txt"),
            other => panic!("Expected Uri, got {:?}", other),
        }

        match parse_external_link("tel:+1-555-123-4567") {
            Some(LinkDestination::Uri(uri)) => assert_eq!(uri, "tel:+1-555-123-4567"),
            other => panic!("Expected Uri, got {:?}", other),
        }

        // RFC 3986: schemes are case-insensitive
        match parse_external_link("HTTP://EXAMPLE.COM") {
            Some(LinkDestination::Uri(uri)) => assert_eq!(uri, "HTTP://EXAMPLE.COM"),
            other => panic!("Expected Uri, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_page_params() {
        assert_eq!(
            parse_external_link("#page=5"),
            Some(LinkDestination::Internal {
                page: 4,
                kind: DestinationKind::Fit
            })
        );

        assert_eq!(
            parse_external_link("#page=0"),
            Some(LinkDestination::Internal {
                page: 0,
                kind: DestinationKind::Fit
            })
        );

        assert_eq!(
            parse_external_link("#page=-3"),
            Some(LinkDestination::Internal {
                page: 0,
                kind: DestinationKind::Fit
            })
        );

        assert_eq!(
            parse_external_link("#page=0&zoom=50"),
            Some(LinkDestination::Internal {
                page: 0,
                kind: DestinationKind::XYZ {
                    left: None,
                    top: None,
                    zoom: Some(50.0),
                }
            })
        );

        assert_eq!(
            parse_external_link("#page=-5&zoom=-20,555"),
            Some(LinkDestination::Internal {
                page: 0,
                kind: DestinationKind::XYZ {
                    left: Some(555.0),
                    top: None,
                    zoom: Some(100.0),
                }
            })
        );

        assert_eq!(
            parse_external_link("#page=3&zoom=0,100,200"),
            Some(LinkDestination::Internal {
                page: 2,
                kind: DestinationKind::XYZ {
                    left: Some(100.0),
                    top: Some(200.0),
                    zoom: Some(100.0),
                }
            })
        );
    }

    #[test]
    fn test_parse_named_dest() {
        match parse_external_link("#nameddest=Chapter1") {
            Some(LinkDestination::Named(name)) => assert_eq!(name, "Chapter1"),
            other => panic!("Expected Named, got {:?}", other),
        }

        // UTF-8 encoded named destination
        match parse_external_link("#nameddest=%E7%AB%A0%E8%8A%82") {
            Some(LinkDestination::Named(name)) => assert_eq!(name, "\u{7AE0}\u{8282}"),
            other => panic!("Expected Named, got {:?}", other),
        }

        match parse_external_link("#Introduction") {
            Some(LinkDestination::Named(name)) => assert_eq!(name, "Introduction"),
            other => panic!("Expected Named, got {:?}", other),
        }

        // UTF-8 encoded named destination
        match parse_external_link("#%E7%AB%A0%E8%8A%82") {
            Some(LinkDestination::Named(name)) => assert_eq!(name, "\u{7AE0}\u{8282}"),
            other => panic!("Expected Named, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_remote_file_scheme() {
        let out = parse_external_link("file:///path/to/document.pdf");
        let expected = LinkDestination::Remote {
            is_url: false,
            file: "/path/to/document.pdf".to_string(),
            page: 0,
            kind: DestinationKind::Fit,
            named_dest: None,
        };
        assert_eq!(out, Some(expected));

        let out = parse_external_link("file:///path/to/document2.pdf#page=5");
        let expected = LinkDestination::Remote {
            is_url: false,
            file: "/path/to/document2.pdf".to_string(),
            page: 4,
            kind: DestinationKind::Fit,
            named_dest: None,
        };
        assert_eq!(out, Some(expected));

        let out = parse_external_link("file:///path/doc.pdf#nameddest=Chapter3");
        let expected = LinkDestination::Remote {
            is_url: false,
            file: "/path/doc.pdf".to_string(),
            page: -1,
            kind: DestinationKind::Fit,
            named_dest: Some("Chapter3".to_string()),
        };
        assert_eq!(out, Some(expected));
    }

    #[test]
    fn test_parse_relative_pdf_path() {
        let out = parse_external_link("manual.pdf#Chapter6");
        let expected = LinkDestination::Remote {
            is_url: false,
            file: "manual.pdf".to_string(),
            page: -1,
            kind: DestinationKind::Fit,
            named_dest: Some("Chapter6".to_string()),
        };
        assert_eq!(out, Some(expected));

        match parse_external_link("../other/document.pdf") {
            Some(LinkDestination::Remote { file, page, .. }) => {
                assert_eq!(file, "../other/document.pdf");
                assert_eq!(page, 0);
            }
            other => panic!("Expected Remote, got {:?}", other),
        }

        match parse_external_link("other.pdf#page=10") {
            Some(LinkDestination::Remote { file, page, .. }) => {
                assert_eq!(file, "other.pdf");
                assert_eq!(page, 9);
            }
            other => panic!("Expected Remote, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_non_pdf() {
        let out = parse_external_link("readme.txt");
        let expected = LinkDestination::Launch("readme.txt".to_string());
        assert_eq!(out, Some(expected));

        let out = parse_external_link("file:///path/to/document.doc");
        let expected = LinkDestination::Launch("/path/to/document.doc".to_string());
        assert_eq!(out, Some(expected));
    }

    #[test]
    fn test_is_external_link() {
        assert!(!is_external_link("/path/to/file.pdf"));
        assert!(!is_external_link("/usr/local/bin"));

        // UNC paths
        assert!(!is_external_link("\\\\server\\share\\file.pdf"));
        assert!(!is_external_link("//server/share/file.pdf"));

        // Windows drive letters
        assert!(!is_external_link("C:/docs/a.pdf"));
        assert!(!is_external_link("C:\\docs\\a.pdf"));

        // Relative paths
        assert!(!is_external_link("./file.pdf"));
        assert!(!is_external_link("../other/file.pdf"));

        // These should be detected as external URIs
        assert!(is_external_link("http://example.com"));
        assert!(is_external_link("mailto:user@example.com"));
        assert!(is_external_link("file:///path/to/file"));
    }

    #[test]
    fn test_is_valid_pdf_path() {
        assert!(is_valid_pdf_path("file.pdf"));
        assert!(is_valid_pdf_path("file.PDF"));
        assert!(is_valid_pdf_path("file.Pdf"));
        assert!(is_valid_pdf_path("FILE.PDF"));
        assert!(is_valid_pdf_path("f.pdf"));

        assert!(!is_valid_pdf_path("file.txt"));
        assert!(!is_valid_pdf_path("pdf"));
        assert!(!is_valid_pdf_path(".pdf"));
        assert!(!is_valid_pdf_path(".pd"));
    }

    #[test]
    fn test_case_insensitive() {
        let out = parse_external_link("https://path/doc.pdf#NaMedDesT=Chapter3");
        let expected = LinkDestination::Remote {
            is_url: true,
            file: "https://path/doc.pdf".to_string(),
            page: -1,
            kind: DestinationKind::Fit,
            named_dest: Some("Chapter3".to_string()),
        };
        assert_eq!(out, Some(expected));

        let out = parse_external_link("https://doc.pdf#PaGe=10&ViEw=fIt");
        let expected = LinkDestination::Remote {
            is_url: true,
            file: "https://doc.pdf".to_string(),
            page: 9,
            kind: DestinationKind::Fit,
            named_dest: None,
        };
        assert_eq!(out, Some(expected));
    }

    #[test]
    fn test_separator_hash_inside_params() {
        let out = parse_external_link("file:///doc.pdf#page=3#zoom=50");

        let expected = LinkDestination::Remote {
            is_url: false,
            file: "/doc.pdf".to_string(),
            page: 2,
            kind: DestinationKind::XYZ {
                left: None,
                top: None,
                zoom: Some(50.0),
            },
            named_dest: None,
        };
        assert_eq!(out, Some(expected));
    }
}
