use super::{FileSpec, PdfAction, PdfDestination};
use crate::DestinationKind;

#[test]
fn test_format_internal_default() {
    let action = PdfAction::GoTo(PdfDestination::Page {
        page: 0,
        kind: DestinationKind::default(),
    });
    assert_eq!(action.to_string(), "#page=1");
}

#[test]
fn test_format_internal_xyz_variants() {
    let action = PdfAction::GoTo(PdfDestination::Page {
        page: 2,
        kind: DestinationKind::XYZ {
            left: Some(100.0),
            top: None,
            zoom: Some(150.0),
        },
    });
    assert_eq!(action.to_string(), "#page=3&zoom=150,100,nan");

    let action = PdfAction::GoTo(PdfDestination::Page {
        page: 0,
        kind: DestinationKind::XYZ {
            left: Some(10.0),
            top: Some(20.0),
            zoom: None,
        },
    });
    assert_eq!(action.to_string(), "#page=1&zoom=nan,10,20");
}

#[test]
fn test_format_internal_fit_variants() {
    let action = PdfAction::GoTo(PdfDestination::Page {
        page: 0,
        kind: DestinationKind::FitH { top: None },
    });
    assert_eq!(action.to_string(), "#page=1&view=FitH");

    let action = PdfAction::GoTo(PdfDestination::Page {
        page: 4,
        kind: DestinationKind::FitBH { top: Some(200.0) },
    });
    assert_eq!(action.to_string(), "#page=5&view=FitBH,200");
}

#[test]
fn test_format_named_encoding() {
    let action = PdfAction::GoTo(PdfDestination::Named("章节".to_string()));
    assert_eq!(action.to_string(), "#nameddest=%E7%AB%A0%E8%8A%82");
}

#[test]
fn test_format_remote_path_default() {
    let action = PdfAction::GoToR {
        file: FileSpec::Path("/path with spaces.pdf".to_string()),
        dest: PdfDestination::default(),
    };
    assert_eq!(
        action.to_string(),
        "file:///path%20with%20spaces.pdf#page=1"
    );
}

#[test]
fn test_format_remote_url_with_fragment() {
    let action = PdfAction::GoToR {
        file: FileSpec::Url("https://example.com/doc.pdf#frag".to_string()),
        dest: PdfDestination::Page {
            page: 1,
            kind: DestinationKind::Fit,
        },
    };
    assert_eq!(
        action.to_string(),
        "https://example.com/doc.pdf#frag&page=2&view=Fit"
    );
}

#[test]
fn test_format_launch_path() {
    let action = PdfAction::Launch(FileSpec::Path("docs/readme.txt".to_string()));
    assert_eq!(action.to_string(), "file:docs/readme.txt#page=1");
}
