pub mod dec;
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ByteOrder {
    Little,
    Big,
}

impl ByteOrder {
    pub const fn host() -> Self {
        #[cfg(target_endian = "little")]
        {
            Self::Little
        }
        #[cfg(target_endian = "big")]
        {
            Self::Big
        }
    }
}

impl Default for ByteOrder {
    fn default() -> Self {
        Self::host()
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum SizeWidth {
    Size32,
    Size64,
}

impl SizeWidth {
    pub const fn host() -> Self {
        #[cfg(target_pointer_width = "32")]
        {
            Self::Size32
        }
        #[cfg(target_pointer_width = "64")]
        {
            Self::Size64
        }
    }
}

impl Default for SizeWidth {
    fn default() -> Self {
        Self::host()
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Configuration {
    pub usize_len: SizeWidth,
    pub order: ByteOrder,

    #[doc(hidden)]
    pub __non_exhaustive: (),
}

impl Configuration {
    pub const fn host() -> Self {
        Self {
            usize_len: SizeWidth::host(),
            order: ByteOrder::host(),
            __non_exhaustive: (),
        }
    }
}

impl Default for Configuration {
    fn default() -> Self {
        Self::host()
    }
}

/// An integer type which can represent every possible value of [`usize`] in an encoded file regardless of host width
///
/// # Implementation Note
/// The current type is `u64`, but this is not guaranteed
pub type EncodedUsize = u64;
/// An integer type which can represent every possible value of [`isize`] in an encoded file regardless of host width
///
/// # Implementation Note
/// The current type is `i64`, but this is not guaranteed
pub type EncodedIsize = i64;
