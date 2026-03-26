//! Core 24-byte representation shared by [`PdfVec`](super::PdfVec) and [`PdfString`](super::PdfString).
//!
//! It securely stores up to 23 bytes inline or spills to the heap, using the
//! 24th byte as a length tag, heap discriminant, NUL terminator, and niche
//! optimization enabler all at once.
//!
//! # Acknowledgements
//!
//! The "magic last byte as discriminant" trick is inspired by `ecow::EcoString` implementation from
//! https://crates.io/crates/ecow crate. The niche optimization technique (making `Option<T>` zero-cost)
//! is inspired by `compact_str::CompactString` implementation from https://crates.io/crates/compact_str.
//! crate.

use core::ffi::CStr;
use core::mem::{align_of, size_of, ManuallyDrop};
use core::slice;

use ecow::EcoVec;

/// Total byte footprint of `Repr` including the discriminant byte.
pub(super) const INLINE_TOTAL: usize = 24;

/// Maximum payload bytes in inline mode (the 24th byte is the discriminant).
const INLINE_CAPACITY: usize = INLINE_TOTAL - 1;

/// Padding between the `EcoVec` and the discriminant byte in `HeapRepr`.
const PAD_LENGTH: usize = INLINE_TOTAL - size_of::<EcoVec<u8>>() - size_of::<LastByte>();

/// Canonical 24-byte storage type.
///
/// The compiler always sees this as `[u8; 23] + LastByte`. For the heap variant, the same
/// memory is reinterpreted as a [`HeapRepr`] via pointer cast. Alignment is set to match
/// `EcoVec<u8>` so the cast is valid.
///
/// # The Magic 24th Byte
///
/// The entire memory layout revolves around the 24th byte (`last`), which
/// serves four distinct purposes at once:
///
/// 1. Inline Length Tag: For inline strings, the last byte encodes the `remaining capacity`
///    using the formula `rem = INLINE_TOTAL - (len + 1)`. If you store a 10-byte string,
///    the last byte is `24 - 10 - 1 = 13`. For a 23-byte string, the last byte becomes
///    `24 - 23 - 1 = 0`, meaning it also will serve as the NUL terminator.
///
/// 2. NUL Terminator: By storing the remainder rather than the length, a fully packed
///    23-byte inline string results in `rem = 24 - (23 + 1) = 0`. Thus, the 24th byte
///    becomes `0` (`\0`), acting as the C-string NUL terminator. For shorter strings,
///    the unused bytes are zero-padded, ensuring a NUL is always present at `bytes[len]`.
///
/// 3. Heap Marker: If the slice exceeds 23 bytes, it spills to an `EcoVec`. The 24th byte
///    is then set to `24` (`LastByte::Spilled`), which marks a heap allocation.
///
/// 4. Niche Optimization Room: Because `last` only ever takes on valid values from `0..=24`,
///    the remaining bit patterns (25..=255) are unused by `LastByte`. The Rust compiler can
///    use these for "niche" optimization, meaning `Option<Repr>` will be the same size as
///    `Repr`. This is why we don't use a `union` for the `Repr` layout, since the compiler
///    could not recognize the optimization room for unions (inspired by
///    [`compact_str::CompactString`](https://docs.rs/compact_str)).
#[cfg(target_pointer_width = "64")]
#[repr(C, align(8))]
pub(super) struct Repr {
    bytes: [u8; INLINE_CAPACITY],
    last: LastByte,
}

#[cfg(target_pointer_width = "32")]
#[repr(C, align(4))]
pub(super) struct Repr {
    bytes: [u8; INLINE_CAPACITY],
    last: LastByte,
}

/// Discriminant stored in the 24th byte (index 23) of every `Repr`.
///
/// For inline variants the value encodes "remaining capacity":
/// `InlineRem23` (= 23) means the buffer is empty (0 content bytes + 1 NUL = 1 byte used),
/// `InlineRem0` (= 0) means the buffer is fully packed (23 content bytes + 1 NUL = 24 bytes).
///
/// `Spilled` (= 24) is impossible for any inline variant because the remainder can never exceed 23.
/// This unused value becomes the heap discriminant and provides a niche for `Option` optimization.
#[allow(dead_code)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
enum LastByte {
    InlineRem0 = 0,
    InlineRem1 = 1,
    InlineRem2 = 2,
    InlineRem3 = 3,
    InlineRem4 = 4,
    InlineRem5 = 5,
    InlineRem6 = 6,
    InlineRem7 = 7,
    InlineRem8 = 8,
    InlineRem9 = 9,
    InlineRem10 = 10,
    InlineRem11 = 11,
    InlineRem12 = 12,
    InlineRem13 = 13,
    InlineRem14 = 14,
    InlineRem15 = 15,
    InlineRem16 = 16,
    InlineRem17 = 17,
    InlineRem18 = 18,
    InlineRem19 = 19,
    InlineRem20 = 20,
    InlineRem21 = 21,
    InlineRem22 = 22,
    InlineRem23 = 23,
    Spilled = 24,
}

impl LastByte {
    /// Returns `true` if this discriminant indicates heap storage.
    #[inline(always)]
    const fn is_heap(self) -> bool {
        self as u8 == Self::Spilled as u8
    }

    /// Constructs an inline discriminant from the number of unused bytes.
    ///
    /// # Panics
    /// Panics if `rem > 23`.
    #[inline(always)]
    const fn from_inline_rem(rem: usize) -> Self {
        assert!(rem <= 23, "inline remainder must be in 0..=23");
        unsafe { core::mem::transmute(rem as u8) }
    }
}

/// Layout for the heap variant.
///
/// The `EcoVec<u8>` always stores the payload with a trailing NUL byte.
/// Padding aligns the discriminant to byte 23, matching `Repr::last`.
#[repr(C)]
pub(super) struct HeapRepr {
    vector: ManuallyDrop<EcoVec<u8>>,
    _pad: [u8; PAD_LENGTH],
    last: LastByte, // always LastByte::Spilled
}

impl HeapRepr {
    /// Wraps an `EcoVec` into a heap repr with `Spilled` discriminant.
    #[inline]
    fn new(vector: EcoVec<u8>) -> Self {
        Self {
            vector: ManuallyDrop::new(vector),
            _pad: [0; PAD_LENGTH],
            last: LastByte::Spilled,
        }
    }

    /// Borrows the underlying `EcoVec`.
    #[inline]
    fn vector(&self) -> &EcoVec<u8> {
        &self.vector
    }

    /// Drops the `EcoVec` in place. Must be called exactly once.
    #[inline]
    unsafe fn drop_vector(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.vector) };
    }
}

impl Repr {
    /// Creates a `Repr` from a `CStr`, going inline if possible.
    #[inline]
    pub(super) fn from_cstr(c_str: &CStr) -> Self {
        let bytes_with_nul = c_str.to_bytes_with_nul();
        // to_bytes_with_nul returns slice with length at least 1
        let len = bytes_with_nul.len() - 1;

        if len < INLINE_TOTAL {
            Self::inline(&bytes_with_nul[..len])
        } else {
            Self::from_heap(HeapRepr::new(EcoVec::from(bytes_with_nul)))
        }
    }

    /// Creates a `Repr` from a byte slice, appending a NUL terminator.
    #[inline]
    pub(super) fn from_slice(bytes: &[u8]) -> Self {
        if bytes.len() < INLINE_TOTAL {
            Self::inline(bytes)
        } else {
            let mut vector = EcoVec::with_capacity(bytes.len() + 1);
            vector.extend_from_slice(bytes);
            vector.push(0);
            Self::from_heap(HeapRepr::new(vector))
        }
    }

    /// Packs raw bytes (without a NUL terminator) into the inline 24-byte structure.
    #[inline(always)]
    fn inline(bytes: &[u8]) -> Self {
        let len = bytes.len();
        debug_assert!(len < INLINE_TOTAL);

        let mut data = [0_u8; INLINE_CAPACITY];
        data[..len].copy_from_slice(bytes);

        // rem is the space left after the string + its logical NUL
        let rem = INLINE_TOTAL - (len + 1);

        // The `data` was initialized with zeros, so it automatically has a NUL terminator
        // after copying from the slice for payloads under 23 bytes. For exactly 23 bytes,
        // the calculated `rem` (0) in the 24th byte serves as the NUL.
        Self {
            bytes: data,
            last: LastByte::from_inline_rem(rem),
        }
    }

    /// Transmutes a `HeapRepr` into a `Repr`.
    #[inline]
    fn from_heap(spilled: HeapRepr) -> Self {
        // SAFETY: Both types are the same size, alignment, and repr(C).
        // `last = Spilled` is a valid `LastByte` discriminant.
        unsafe { core::mem::transmute(spilled) }
    }

    /// Returns `true` if the data is heap-allocated.
    #[inline]
    pub(super) const fn is_heap(&self) -> bool {
        self.last.is_heap()
    }

    /// Returns the payload bytes without the trailing NUL.
    #[inline]
    pub(super) fn to_bytes(&self) -> &[u8] {
        if self.is_heap() {
            // SAFETY: dara is stored in heap (checked above).
            let heap = unsafe { self.as_heap() };
            let slice = heap.vector().as_slice();
            debug_assert!(!slice.is_empty());
            debug_assert_eq!(slice[slice.len() - 1], 0);
            &slice[..slice.len() - 1]
        } else {
            // SAFETY: dara is stored in heap (checked above).
            let len = unsafe { self.inline_len() };
            &self.bytes[..len]
        }
    }

    /// Returns the content as a `&CStr` without allocation.
    #[inline]
    pub(super) fn as_cstr(&self) -> &CStr {
        if self.is_heap() {
            // SAFETY: dara is stored in heap (checked above).
            let spilled = unsafe { self.as_heap() };
            // SAFETY: EcoVec storage always includes a single trailing NUL.
            unsafe { CStr::from_bytes_with_nul_unchecked(spilled.vector().as_slice()) }
        } else {
            // SAFETY: dara is stored in stack (checked above).
            let bytes_with_nul = unsafe { self.inline_bytes_with_nul() };
            // SAFETY: inline representation preserves a valid trailing NUL.
            unsafe { CStr::from_bytes_with_nul_unchecked(bytes_with_nul) }
        }
    }

    /// Returns the payload length for an inline variant.
    ///
    /// # Safety
    /// Must only be called when `!is_heap()`.
    #[inline]
    unsafe fn inline_len(&self) -> usize {
        debug_assert!(!self.is_heap());
        INLINE_CAPACITY - self.last as u8 as usize
    }

    /// Reinterprets `&self` as `&HeapRepr`.
    ///
    /// # Safety
    /// Must only be called when `is_heap()`.
    #[inline]
    const unsafe fn as_heap(&self) -> &HeapRepr {
        debug_assert!(self.is_heap());
        unsafe { &*(self as *const Self).cast::<HeapRepr>() }
    }

    /// Reinterprets `&mut self` as `&mut HeapRepr`.
    ///
    /// # Safety
    /// Must only be called when `is_heap()`.
    #[inline]
    unsafe fn as_mut_heap(&mut self) -> &mut HeapRepr {
        debug_assert!(self.is_heap());
        unsafe { &mut *(self as *mut Self).cast::<HeapRepr>() }
    }

    /// Returns a slice covering `payload + NUL` for the inline variant.
    ///
    /// # Safety
    /// Must only be called when `!is_heap()`.
    #[inline]
    unsafe fn inline_bytes_with_nul(&self) -> &[u8] {
        debug_assert!(!self.is_heap());

        let len_with_nul = self.inline_len() + 1;

        // SAFETY:
        // 1. `Repr` is `#[repr(C)]`, so `bytes` (23) and `last` (1) form
        //    a contiguous 24-byte block in memory.
        // 2. Since `self.inline_len() <= 23`, `len_with_nul <= 24`. We will never read past
        //    the bounds of the struct.
        unsafe { slice::from_raw_parts((self as *const Self).cast::<u8>(), len_with_nul) }
    }
}

/// Default is an empty inline string (0 payload bytes, remainder = 23).
impl Default for Repr {
    #[inline]
    fn default() -> Self {
        Repr {
            bytes: [0u8; INLINE_CAPACITY],
            last: LastByte::InlineRem23,
        }
    }
}

impl Clone for Repr {
    /// Clones the representation. This is a fast, `O(1)` operation.
    ///
    /// - Inline: Performs a simple bitwise copy of the 24-byte struct.
    /// - Heap: Cheaply increments the reference count of the underlying [`EcoVec`].
    #[inline]
    fn clone(&self) -> Self {
        if self.is_heap() {
            // SAFETY: dara is stored in heap (checked above).
            let spilled = unsafe { self.as_heap() };
            let cloned = spilled.vector().clone();
            Repr::from_heap(HeapRepr::new(cloned))
        } else {
            // Inline variant is plain bytes + enum so copying the 24-bytes is fine.
            Repr {
                bytes: self.bytes,
                last: self.last,
            }
        }
    }
}

impl Drop for Repr {
    #[inline]
    fn drop(&mut self) {
        if self.is_heap() {
            // SAFETY: dara is stored in heap (checked above), and we drop exactly once.
            unsafe { self.as_mut_heap().drop_vector() };
        }
    }
}

// SAFETY: `EcoVec<u8>` is Send + Sync, and the inline variant is plain bytes.
unsafe impl Send for Repr {}
unsafe impl Sync for Repr {}

// Base type guarantees
const _: () = assert!(size_of::<EcoVec<u8>>() == 2 * size_of::<usize>());
const _: () = assert!(size_of::<LastByte>() == 1);

// Size guarantees
const _: () = assert!(size_of::<Repr>() == INLINE_TOTAL);
const _: () = assert!(size_of::<HeapRepr>() == INLINE_TOTAL);

// Alignment guarantees
const _: () = assert!(align_of::<Repr>() == align_of::<HeapRepr>());
const _: () = assert!(align_of::<HeapRepr>() == align_of::<EcoVec<u8>>());

// Offset guarantees: discriminant sits at byte 23 in both layouts
const _: () = assert!(core::mem::offset_of!(Repr, last) == INLINE_TOTAL - 1);
const _: () = assert!(core::mem::offset_of!(HeapRepr, last) == INLINE_TOTAL - 1);
