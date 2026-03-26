use super::*;
use std::borrow::Cow;
use std::ffi::CString;

#[test]
fn basics() {
    // Empty inline
    let empty = PdfString::default();
    assert!(empty.is_inline());
    assert_eq!(empty.as_bytes(), b"");
    assert_eq!(empty.as_cstr().to_bytes_with_nul(), b"\0");

    // Short inline
    let c_str = CString::new("abc").unwrap();
    let short = PdfString::try_from(&c_str).unwrap();
    assert!(short.is_inline());
    assert_eq!(short.as_bytes(), b"abc");
    assert_eq!(short.as_cstr().to_bytes_with_nul(), b"abc\0");
    assert_eq!(short.as_str(), "abc");

    // Fully packed inline (23 bytes)
    let string23 = "x".repeat(23);
    let c_str23 = CString::new(string23.clone()).unwrap();
    let packed = PdfString::try_from(&c_str23).unwrap();
    assert!(packed.is_inline());
    assert_eq!(packed.len(), 23);
    assert_eq!(packed.as_bytes(), string23.as_bytes());
    assert_eq!(
        packed.as_cstr().to_bytes_with_nul(),
        c_str23.to_bytes_with_nul()
    );

    // Spilled (heap, 24 bytes)
    let string24 = "x".repeat(24);
    let c_str24 = CString::new(string24.clone()).unwrap();
    let spilled = PdfString::try_from(&c_str24).unwrap();
    assert!(spilled.is_heap());
    assert_eq!(spilled.as_bytes(), string24.as_bytes());
    assert_eq!(
        spilled.as_cstr().to_bytes_with_nul(),
        c_str24.to_bytes_with_nul()
    );

    // Clone preserves data
    let cloned = short.clone();
    assert_eq!(short, cloned);
    assert_eq!(cloned.as_cstr().to_bytes_with_nul(), b"abc\0");
}

#[test]
fn pdfvec_equality() {
    let short = PdfVec::from("hello".as_bytes());
    let long = PdfVec::from([b'x'; 30].as_slice());
    let other = PdfVec::from("world".as_bytes());

    assert!(short.is_inline());
    assert!(long.is_heap());

    // PdfVec == PdfVec
    assert_eq!(short, short.clone());
    assert_eq!(long, long.clone());
    assert_ne!(short, other);

    // PdfVec == &[u8]
    assert_eq!(short, b"hello".as_slice());
    assert_ne!(short, b"world".as_slice());

    // PdfVec == Vec<u8>
    assert_eq!(short, b"hello".to_vec());
    assert_ne!(short, b"world".to_vec());

    // PdfVec == &str  (str: AsRef<[u8]>)
    assert_eq!(short, "hello");
    assert_ne!(short, "world");

    // &PdfVec == PdfVec
    assert_eq!(&short, short.clone());
    assert_ne!(&short, other.clone());

    // Vec<u8> == PdfVec
    assert_eq!(b"hello".to_vec(), short);
    assert_ne!(b"world".to_vec(), short);

    // Vec<u8> == &PdfVec
    assert_eq!(b"hello".to_vec(), &short);
    assert_ne!(b"world".to_vec(), &short);

    // &Vec<u8> == PdfVec
    let v = b"hello".to_vec();
    assert_eq!(&v, short);

    // [u8] == PdfVec
    let hello_bytes: &[u8] = b"hello";
    assert_eq!(*hello_bytes, short);

    // [u8] == &PdfVec
    assert_eq!(*hello_bytes, &short);

    // &[u8] == PdfVec
    assert_eq!(hello_bytes, short);
    assert_ne!(b"world".as_slice(), short);

    // &&[u8] == PdfVec
    let s_u8: &&[u8] = &b"hello".as_slice();
    assert_eq!(s_u8, short);
    assert_eq!(short, s_u8);

    // &&str == PdfVec
    let s_u8: &&&[u8] = &&b"hello".as_slice();
    assert_eq!(short, s_u8);

    let long_bytes = vec![b'x'; 30];
    assert_eq!(long, long_bytes);
    assert_eq!(long_bytes, long);
    assert_eq!(&long, long.clone());
    assert_eq!(long_bytes.as_slice(), long);
}

// ---------------------------------------------------------------------------
// PdfString equality — exercises every PartialEq impl (blanket + reverse)
// ---------------------------------------------------------------------------

#[test]
#[allow(clippy::needless_borrow)]
fn pdfstring_equality() {
    let short = PdfString::from("hello");
    let long = PdfString::from(&"y".repeat(30));
    let other = PdfString::from("world");

    assert!(short.is_inline());
    assert!(long.is_heap());

    // PdfString == PdfString
    assert_eq!(short, short.clone());
    assert_eq!(long, long.clone());
    assert_ne!(short, other);

    // PdfString == &str
    assert_eq!(short, "hello");
    assert_ne!(short, "world");

    // PdfString == String
    assert_eq!(short, String::from("hello"));
    assert_ne!(short, String::from("world"));

    // PdfString == Cow<str>
    let cow_borrowed: Cow<str> = Cow::Borrowed("hello");
    let cow_owned: Cow<str> = Cow::Owned(String::from("hello"));
    assert_eq!(short, cow_borrowed);
    assert_eq!(short, cow_owned);

    // &PdfString == PdfString
    assert_eq!(&short, short.clone());
    assert_ne!(&short, other.clone());

    // String == PdfString
    assert_eq!(String::from("hello"), short);
    assert_ne!(String::from("world"), short);

    // String == &PdfString
    assert_eq!(String::from("hello"), &short);
    assert_ne!(String::from("world"), &short);

    // &String == PdfString
    let s = String::from("hello");
    assert_eq!(&s, short);

    // str == PdfString
    assert_eq!(*"hello", short);

    // str == &PdfString
    assert_eq!(*"hello", &short);

    // &str == PdfString
    assert_eq!("hello", short);
    assert_ne!("world", short);

    // &&str == PdfString
    let r: &&str = &&"hello";
    assert_eq!(r, short);

    // Cow<str> == PdfString
    assert_eq!(cow_borrowed, short);
    assert_eq!(cow_owned, short);

    // &Cow<str> == PdfString
    assert_eq!(&cow_borrowed, short);

    // &PdfString == String
    assert_eq!(&short, String::from("hello"));

    // &PdfString == Cow<str>
    assert_eq!(&short, cow_borrowed);

    let long_str = "y".repeat(30);
    assert_eq!(long, long_str.as_str());
    assert_eq!(long_str, long);
    assert_eq!(&long, long.clone());
}
