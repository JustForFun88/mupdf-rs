use crate::color::AnnotationColor;
use crate::pdf::annotation::AnnotationFlags;
use crate::pdf::{Intent, PdfAnnotationType, PdfDocument, PdfFilterOptions};
use crate::{Point, Rect, Size};

const PAGE_SIZE: Size = Size::A4;

/// Test helper that owns a [`PdfDocument`] with one blank page and provides
/// short-hand methods for creating and inspecting annotations.
struct AnnotTester {
    doc: PdfDocument,
}

impl AnnotTester {
    fn new() -> Self {
        let mut doc = PdfDocument::new();
        doc.new_page(PAGE_SIZE).unwrap();
        Self { doc }
    }

    fn page(&self) -> crate::pdf::PdfPage {
        self.doc.load_pdf_page(0).unwrap()
    }

    /// Create an annotation of the given type on page 0.
    fn create(&self, ty: PdfAnnotationType) -> crate::pdf::PdfAnnotation {
        self.page().create_annotation(ty).unwrap()
    }
}

// ===========================================================================
// PdfAnnotation::object()
// ===========================================================================

#[test]
fn annotation_object_returns_valid_dict() {
    let t = AnnotTester::new();
    let annot = t.create(PdfAnnotationType::Text);
    let obj = annot.object().unwrap();
    assert!(obj.is_dict().unwrap());
}

#[test]
fn annotation_object_has_correct_subtype() {
    let t = AnnotTester::new();
    let annot = t.create(PdfAnnotationType::Highlight);
    let obj = annot.object().unwrap();
    let subtype = obj.get_dict("Subtype").unwrap().unwrap();
    assert_eq!(subtype.as_name_str().unwrap(), "Highlight");
}

// ===========================================================================
// PdfAnnotation::type()
// ===========================================================================

#[test]
fn annotation_type_text() {
    let t = AnnotTester::new();
    let annot = t.create(PdfAnnotationType::Text);
    assert_eq!(annot.r#type().unwrap(), PdfAnnotationType::Text);
}

#[test]
fn annotation_type_highlight() {
    let t = AnnotTester::new();
    let annot = t.create(PdfAnnotationType::Highlight);
    assert_eq!(annot.r#type().unwrap(), PdfAnnotationType::Highlight);
}

#[test]
fn annotation_object_type_matches_all_creatable() {
    let types = [
        PdfAnnotationType::Text,
        PdfAnnotationType::FreeText,
        PdfAnnotationType::Line,
        PdfAnnotationType::Square,
        PdfAnnotationType::Circle,
        PdfAnnotationType::Highlight,
        PdfAnnotationType::Underline,
        PdfAnnotationType::Squiggly,
        PdfAnnotationType::StrikeOut,
        PdfAnnotationType::Stamp,
        PdfAnnotationType::Caret,
        PdfAnnotationType::Ink,
        PdfAnnotationType::Redact,
    ];

    let t = AnnotTester::new();
    let mut page = t.page();
    for &annot_type in &types {
        let annot = page.create_annotation(annot_type).unwrap();
        assert_eq!(annot.r#type().unwrap(), annot_type);
        let obj = annot.object().unwrap();
        assert!(obj.is_dict().unwrap());
        assert_eq!(obj.annot_type().unwrap(), annot_type);
    }
}

// ===========================================================================
// PdfObject::annot_type() — from raw dicts
// ===========================================================================

#[test]
fn annot_type_from_dict_with_subtype() {
    let pdf = PdfDocument::new();
    let mut obj = pdf.new_dict().unwrap();
    obj.dict_put_name("Subtype", "Text").unwrap();
    assert_eq!(obj.annot_type().unwrap(), PdfAnnotationType::Text);
}

#[test]
fn annot_type_from_dict_link() {
    let pdf = PdfDocument::new();
    let mut obj = pdf.new_dict().unwrap();
    obj.dict_put_name("Subtype", "Link").unwrap();
    assert_eq!(obj.annot_type().unwrap(), PdfAnnotationType::Link);
}

#[test]
fn annot_type_unknown_subtype() {
    let pdf = PdfDocument::new();
    let mut obj = pdf.new_dict().unwrap();
    obj.dict_put_name("Subtype", "Bogus").unwrap();
    assert_eq!(obj.annot_type().unwrap(), PdfAnnotationType::Unknown);
}

#[test]
fn annot_type_missing_subtype() {
    let pdf = PdfDocument::new();
    let obj = pdf.new_dict().unwrap();
    assert_eq!(obj.annot_type().unwrap(), PdfAnnotationType::Unknown);
}

// ===========================================================================
// Iteration via annotations()
// ===========================================================================

#[test]
fn iterate_annotations_matches_created() {
    let expected = [
        PdfAnnotationType::Text,
        PdfAnnotationType::Highlight,
        PdfAnnotationType::StrikeOut,
    ];

    let t = AnnotTester::new();
    let mut page = t.page();
    for &ty in &expected {
        page.create_annotation(ty).unwrap();
    }

    let annots: Vec<_> = page.annotations().collect();
    assert_eq!(annots.len(), expected.len());
    for (annot, &expected_type) in annots.iter().zip(&expected) {
        assert_eq!(annot.r#type().unwrap(), expected_type);
        assert_eq!(annot.object().unwrap().annot_type().unwrap(), expected_type);
    }
}

#[test]
fn iterate_empty_page_yields_nothing() {
    let t = AnnotTester::new();
    let annots: Vec<_> = t.page().annotations().collect();
    assert!(annots.is_empty());
}

// ===========================================================================
// PdfAnnotation::is_hot() / set_hot()
// ===========================================================================

#[test]
fn annotation_hot_default_false() {
    let t = AnnotTester::new();
    let annot = t.create(PdfAnnotationType::Text);
    assert!(!annot.is_hot());
}

#[test]
fn annotation_set_hot_true() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Text);
    annot.set_hot(true);
    assert!(annot.is_hot());
}

#[test]
fn annotation_set_hot_roundtrip() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Text);
    annot.set_hot(true);
    assert!(annot.is_hot());
    annot.set_hot(false);
    assert!(!annot.is_hot());
}

// ===========================================================================
// PdfAnnotation::is_active() / set_active()
// ===========================================================================

#[test]
fn annotation_active_default_false() {
    let t = AnnotTester::new();
    let annot = t.create(PdfAnnotationType::Text);
    assert!(!annot.is_active());
}

#[test]
fn annotation_set_active_true() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Text);
    annot.set_active(true).unwrap();
    assert!(annot.is_active());
}

#[test]
fn annotation_set_active_roundtrip() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Text);
    annot.set_active(true).unwrap();
    assert!(annot.is_active());
    annot.set_active(false).unwrap();
    assert!(!annot.is_active());
}

// ===========================================================================
// PdfAnnotation::rect() / set_rect()
// ===========================================================================

#[test]
fn annotation_set_rect() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Square);
    let rect = Rect {
        x0: 10.0,
        y0: 20.0,
        x1: 100.0,
        y1: 80.0,
    };
    annot.set_rect(rect).unwrap();
    let got = annot.rect().unwrap();
    assert_eq!(got.x0, 10.0);
    assert_eq!(got.y0, 20.0);
    assert_eq!(got.x1, 100.0);
    assert_eq!(got.y1, 80.0);
}

// ===========================================================================
// PdfAnnotation::color() / set_color()
// ===========================================================================

#[test]
fn annotation_set_color_rgb() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Highlight);
    let expected = AnnotationColor::Rgb {
        red: 1.0,
        green: 0.0,
        blue: 0.0,
    };
    annot.set_color(expected).unwrap();
    assert_eq!(annot.color().unwrap(), expected);
}

#[test]
fn annotation_set_color_gray() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Square);
    annot.set_color(AnnotationColor::Gray(0.5)).unwrap();
    match annot.color().unwrap() {
        AnnotationColor::Gray(g) => assert!((g - 0.5).abs() < 0.01),
        other => panic!("expected Gray, got {:?}", other),
    }
}

#[test]
fn annotation_set_color_cmyk() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Square);
    let expected = AnnotationColor::Cmyk {
        cyan: 1.0,
        magenta: 0.0,
        yellow: 0.0,
        key: 0.0,
    };
    annot.set_color(expected).unwrap();
    assert_eq!(annot.color().unwrap(), expected);
}

// ===========================================================================
// PdfAnnotation::flags() / set_flags()
// ===========================================================================

#[test]
fn annotation_set_flags_hidden() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Text);
    annot.set_flags(AnnotationFlags::IS_HIDDEN).unwrap();
    assert!(annot.flags().unwrap().contains(AnnotationFlags::IS_HIDDEN));
}

#[test]
fn annotation_set_flags_print() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Text);
    annot.set_flags(AnnotationFlags::IS_PRINT).unwrap();
    assert!(annot.flags().unwrap().contains(AnnotationFlags::IS_PRINT));
}

#[test]
fn annotation_set_flags_combined() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Text);
    let flags = AnnotationFlags::IS_PRINT | AnnotationFlags::IS_LOCKED;
    annot.set_flags(flags).unwrap();
    let got = annot.flags().unwrap();
    assert!(got.contains(AnnotationFlags::IS_PRINT));
    assert!(got.contains(AnnotationFlags::IS_LOCKED));
}

// ===========================================================================
// PdfAnnotation::border_width() / set_border_width()
// ===========================================================================

#[test]
fn annotation_set_border_width() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Square);
    annot.set_border_width(2.5).unwrap();
    assert!((annot.border_width().unwrap() - 2.5).abs() < 0.01);
}

#[test]
fn annotation_set_border_width_zero() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Circle);
    annot.set_border_width(0.0).unwrap();
    assert_eq!(annot.border_width().unwrap(), 0.0);
}

// ===========================================================================
// PdfAnnotation::author() / set_author()
// ===========================================================================

#[test]
fn annotation_author_default_none() {
    let t = AnnotTester::new();
    let annot = t.create(PdfAnnotationType::Text);
    let author = annot.author().unwrap();
    assert!(author.is_none() || author == Some(""));
}

#[test]
fn annotation_set_author() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Text);
    annot.set_author("Test Author").unwrap();
    assert_eq!(annot.author().unwrap(), Some("Test Author"));
}

#[test]
fn annotation_set_author_overwrite() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Text);
    annot.set_author("First").unwrap();
    assert_eq!(annot.author().unwrap(), Some("First"));
    annot.set_author("Second").unwrap();
    assert_eq!(annot.author().unwrap(), Some("Second"));
}

// ===========================================================================
// PdfAnnotation::line() / set_line()
// ===========================================================================

#[test]
fn annotation_set_line() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Line);
    let start = Point { x: 10.0, y: 20.0 };
    let end = Point { x: 200.0, y: 300.0 };
    annot.set_line(start, end).unwrap();

    let (a, b) = annot.line().unwrap();
    assert!((a.x - 10.0).abs() < 0.01);
    assert!((a.y - 20.0).abs() < 0.01);
    assert!((b.x - 200.0).abs() < 0.01);
    assert!((b.y - 300.0).abs() < 0.01);
}

// ===========================================================================
// PdfAnnotation::popup() / set_popup()
// ===========================================================================

#[test]
fn annotation_set_popup() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Text);
    let rect = Rect {
        x0: 100.0,
        y0: 100.0,
        x1: 300.0,
        y1: 200.0,
    };
    annot.set_popup(rect).unwrap();
    let got = annot.popup().unwrap();
    assert_eq!(got.x0, 100.0);
    assert_eq!(got.y0, 100.0);
    assert_eq!(got.x1, 300.0);
    assert_eq!(got.y1, 200.0);
}

// ===========================================================================
// PdfAnnotation::intent() / set_intent()
// ===========================================================================

#[test]
fn annotation_set_intent_line_arrow() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Line);
    annot.set_intent(Intent::LineArrow).unwrap();
    assert_eq!(annot.intent().unwrap(), Intent::LineArrow);
}

#[test]
fn annotation_set_intent_freetext_callout() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::FreeText);
    annot.set_intent(Intent::FreetextCallout).unwrap();
    assert_eq!(annot.intent().unwrap(), Intent::FreetextCallout);
}

#[test]
fn annotation_set_intent_polygon_cloud() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::Polygon);
    annot.set_intent(Intent::PolygonCloud).unwrap();
    assert_eq!(annot.intent().unwrap(), Intent::PolygonCloud);
}

// ===========================================================================
// PdfAnnotation::filter()
// ===========================================================================

#[test]
fn annotation_filter_default_options() {
    let t = AnnotTester::new();
    let mut annot = t.create(PdfAnnotationType::FreeText);
    let opt = PdfFilterOptions::default();
    annot.filter(opt).unwrap();
}

// ===========================================================================
// PdfPage::delete_annotation()
// ===========================================================================

#[test]
fn delete_annotation_removes_it() {
    let t = AnnotTester::new();
    let mut page = t.page();
    page.create_annotation(PdfAnnotationType::Text).unwrap();
    page.create_annotation(PdfAnnotationType::Highlight).unwrap();

    let annots: Vec<_> = page.annotations().collect();
    assert_eq!(annots.len(), 2);

    page.delete_annotation(&annots[0]).unwrap();
    let remaining: Vec<_> = page.annotations().collect();
    assert_eq!(remaining.len(), 1);
    assert_eq!(remaining[0].r#type().unwrap(), PdfAnnotationType::Highlight);
}
