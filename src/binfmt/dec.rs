use core::slice;
use std::{
    cmp::Ordering,
    marker::PhantomData,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
};

use crate::ir::PhantomIrMarker;

use super::{ByteOrder, Configuration, EncodedIsize, EncodedUsize, SizeWidth};

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    OutOfRangeUnsigned(u128),
    OutOfRangeSigned(i128),
    FormatError(&'static str),
    ZeroNonZero,
    BadType(&'static str),
    InvalidUtf8,
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(e) => e.fmt(f),
            Self::OutOfRangeUnsigned(n) => f.write_fmt(format_args!("Value {n} out of range")),
            Self::OutOfRangeSigned(n) => f.write_fmt(format_args!("Value {n} out of range")),
            Self::FormatError(n) => f.write_fmt(format_args!("Format Error: {n}")),
            Self::ZeroNonZero => f.write_fmt(format_args!("NonZero Primitive value was 0")),
            Self::BadType(v) => f.write_fmt(format_args!("Typed value was incorrect type, {v}")),
            Self::InvalidUtf8 => f.write_fmt(format_args!("Invalid UTF-8 string")),
        }
    }
}

impl core::error::Error for Error {}

pub type Result<T> = core::result::Result<T, Error>;

pub trait Reader {
    fn read(&mut self, bytes: &mut [u8]) -> Result<()>;
}

impl<W: std::io::Read> Reader for W {
    fn read(&mut self, bytes: &mut [u8]) -> Result<()> {
        self.read_exact(bytes).map_err(Error::Io)
    }
}

pub struct Decoder<R, C = ()> {
    base: R,
    config: Configuration,
    context: C,
}

impl<R> Decoder<R> {
    pub const fn new_host(base: R) -> Self {
        Self::with_context_for_host(base, ())
    }

    pub const fn with_config(base: R, config: Configuration) -> Self {
        Self::with_context_and_config(base, config, ())
    }
}

impl<C, R> Decoder<R, C> {
    pub const fn with_context_for_host(base: R, context: C) -> Self {
        Self::with_context_and_config(base, Configuration::host(), context)
    }
    pub const fn with_context_and_config(base: R, config: Configuration, context: C) -> Self {
        Self {
            base,
            config,
            context,
        }
    }

    pub const fn inner_mut(&mut self) -> &mut R {
        &mut self.base
    }

    pub const fn config(&self) -> &Configuration {
        &self.config
    }

    pub const fn config_mut(&mut self) -> &mut Configuration {
        &mut self.config
    }

    pub const fn context(&self) -> &C {
        &self.context
    }
}

impl<C, R: Reader> Reader for Decoder<R, C> {
    fn read(&mut self, bytes: &mut [u8]) -> Result<()> {
        self.base.read(bytes)
    }
}

impl<C, R: Reader> Decoder<R, C> {
    pub fn read_encoded_usize(&mut self) -> Result<EncodedUsize> {
        match self.config.usize_len {
            SizeWidth::Size32 => u32::decode(self).map(|v| v as EncodedUsize),
            SizeWidth::Size64 => u64::decode(self).map(|v| v as EncodedUsize),
        }
    }

    pub fn read_encoded_isize(&mut self) -> Result<EncodedIsize> {
        self.read_encoded_usize().map(|v| v as EncodedIsize)
    }

    pub fn read_usize(&mut self) -> Result<usize> {
        self.read_encoded_usize().and_then(|v| {
            v.try_into()
                .map_err(|e| Error::OutOfRangeUnsigned(v as u128))
        })
    }

    pub fn read_isize(&mut self) -> Result<isize> {
        self.read_encoded_isize()
            .and_then(|v| v.try_into().map_err(|e| Error::OutOfRangeSigned(v as i128)))
    }

    pub fn read_bytes<const N: usize>(&mut self) -> Result<[u8; N]> {
        let mut res = [0u8; N];

        self.read(&mut res)?;

        Ok(res)
    }
}

pub trait Decode<C>: Sized {
    fn decode<R: Reader>(decoder: &mut Decoder<R, C>) -> Result<Self>;

    /// Decodes several values of type `Self` from the reader, optimizing for bulk reads if possible.
    ///
    /// The default implementation calls [`Decode::decode`] N times in a row to populate the array
    fn decode_array<R: Reader, const N: usize>(decoder: &mut Decoder<R, C>) -> Result<[Self; N]> {
        let mut buf = MaybeUninit::<[Self; N]>::uninit();

        let p = buf.as_mut_ptr().cast::<Self>();
        for i in 0..N {
            unsafe { p.add(i).write(Self::decode(decoder)?) }
        }

        Ok(unsafe { buf.assume_init() })
    }

    /// Decodes a value of type `Self` from the reader, allowing reuse of an existing buffer if required.
    ///
    /// The default implementation replaces `self` with the result of [`Decode::decode`]
    ///
    /// # Errors
    /// If an error occurs, `self` is left in a valid, but unspecified state.
    fn decode_in_place<R: Reader>(&mut self, decoder: &mut Decoder<R, C>) -> Result<()> {
        *self = Self::decode(decoder)?;

        Ok(())
    }

    /// Decodes consecutive copies of self, storing them in a slice, allowing optimizations dependant on bulk-reads and allowing reuse of existing values.
    ///
    /// The default implementation calls [`Decode::decode_in_place`] on each element of `this`
    /// # Errors
    /// If an error occurs, `this` is left in a valid, but unspecified state.
    fn decode_in_place_slice<R: Reader>(
        this: &mut [Self],
        decoder: &mut Decoder<R, C>,
    ) -> Result<()> {
        for n in this {
            n.decode_in_place(decoder);
        }
        Ok(())
    }

    /// Decodes `len` consecutive copies of `Self`, storing them in a [`Vec`], allowing optimizations dependant on bulk reads.
    ///
    /// `this` must be empty (this can be relied upon by the implementation for correctness, though not soundness)
    ///
    /// The default implementation repeatedly calls `Self::decode`
    /// # Errors
    /// If an error occurs, `this` is left in a valid, but unspecified state.
    /// In particular, `this.len()` may be any value.
    fn decode_fill<R: Reader>(
        this: &mut Vec<Self>,
        len: usize,
        decoder: &mut Decoder<R, C>,
    ) -> Result<()> {
        this.reserve(len);

        for _ in 0..len {
            this.push(Self::decode(decoder)?)
        }

        Ok(())
    }
}

impl<C, T: Decode<C>, const N: usize> Decode<C> for [T; N] {
    fn decode<R: Reader>(decoder: &mut Decoder<R, C>) -> Result<[T; N]> {
        T::decode_array(decoder)
    }

    fn decode_in_place<R: Reader>(&mut self, decoder: &mut Decoder<R, C>) -> Result<()> {
        T::decode_in_place_slice(self, decoder)
    }

    fn decode_in_place_slice<R: Reader>(
        this: &mut [Self],
        decoder: &mut Decoder<R, C>,
    ) -> Result<()> {
        T::decode_in_place_slice(this.as_flattened_mut(), decoder)
    }
}

impl<C, T: Decode<C>> Decode<C> for Vec<T> {
    fn decode_array<R: Reader, const N: usize>(decoder: &mut Decoder<R, C>) -> Result<[Self; N]> {
        let mut buf = MaybeUninit::<[Self; N]>::uninit();

        let p = buf.as_mut_ptr().cast::<Self>();
        for i in 0..N {
            unsafe { p.add(i).write(Self::decode(decoder)?) }
        }

        Ok(unsafe { buf.assume_init() })
    }

    fn decode_in_place<R: Reader>(&mut self, decoder: &mut Decoder<R, C>) -> Result<()> {
        self.truncate(0);
        let len = decoder.read_usize()?;
        self.reserve(len);
        T::decode_fill(self, len, decoder)?;
        Ok(())
    }

    fn decode<R: Reader>(decoder: &mut Decoder<R, C>) -> Result<Self> {
        let len = decoder.read_usize()?;
        let mut v = Vec::with_capacity(len);

        T::decode_fill(&mut v, len, decoder)?;

        Ok(v)
    }
}

impl<C> Decode<C> for u8 {
    fn decode<R: Reader>(decoder: &mut Decoder<R, C>) -> Result<Self> {
        let mut v = 0;
        Self::decode_in_place(&mut v, decoder)?;

        Ok(v)
    }
    fn decode_in_place<R: Reader>(&mut self, decoder: &mut Decoder<R, C>) -> Result<()> {
        Self::decode_in_place_slice(core::slice::from_mut(self), decoder)
    }

    fn decode_array<R: Reader, const N: usize>(decoder: &mut Decoder<R, C>) -> Result<[Self; N]> {
        let mut arr = [0u8; N];

        Self::decode_in_place_slice(&mut arr, decoder)?;
        Ok(arr)
    }

    fn decode_fill<R: Reader>(
        this: &mut Vec<Self>,
        len: usize,
        decoder: &mut Decoder<R, C>,
    ) -> Result<()> {
        this.resize(len, 0);
        Self::decode_in_place_slice(this, decoder)
    }

    fn decode_in_place_slice<R: Reader>(
        this: &mut [Self],
        decoder: &mut Decoder<R, C>,
    ) -> Result<()> {
        decoder.read(this)
    }
}

macro_rules! impl_decode_for_prim {
    ($($prim_ty:ty),* $(,)?) => {
        $(impl<C> Decode<C> for $prim_ty {
            fn decode<R: Reader>(decoder: &mut Decoder<R, C>) -> Result<Self> {
                let mut bytes = [0u8; core::mem::size_of::<Self>()];
                decoder.read(&mut bytes)?;
                match decoder.config().order {
                    ByteOrder::Little => Ok(<$prim_ty>::from_le_bytes(bytes)),
                    ByteOrder::Big => Ok(<$prim_ty>::from_be_bytes(bytes)),
                }
            }

            fn decode_array<R: Reader, const N: usize>(
                decoder: &mut Decoder<R, C>,
            ) -> Result<[Self; N]> {
                let mut bytes = [0; N];

                Self::decode_in_place_slice(&mut bytes, decoder)?;
                Ok(bytes)
            }

            fn decode_in_place_slice<R: Reader>(
                this: &mut [Self],
                decoder: &mut Decoder<R, C>,
            ) -> Result<()> {
                type Bytes = [u8; core::mem::size_of::<$prim_ty>()];

                let bytes = unsafe { &mut *(this as *mut [Self] as *mut [Bytes]) };

                decoder.read(bytes.as_flattened_mut())?;

                match decoder.config().order {
                    ByteOrder::Little => {
                        for n in this {
                            *n = (*n).to_le()
                        }
                    }
                    ByteOrder::Big => {
                        for n in this {
                            *n = (*n).to_be()
                        }
                    }
                }

                Ok(())
            }

            fn decode_fill<R: Reader>(
                this: &mut Vec<Self>,
                n: usize,
                decoder: &mut Decoder<R, C>,
            ) -> Result<()> {
                this.resize(n, 0);
                Self::decode_in_place_slice(this, decoder)
            }
        })*
    };
}

impl_decode_for_prim!(i8, u16, i16, u32, i32, u64, i64, u128, i128);

impl<C> Decode<C> for usize {
    fn decode<R: Reader>(decoder: &mut Decoder<R, C>) -> Result<Self> {
        decoder.read_usize()
    }

    fn decode_array<R: Reader, const N: usize>(decoder: &mut Decoder<R, C>) -> Result<[Self; N]> {
        let mut arr = [0; N];

        Self::decode_in_place_slice(&mut arr, decoder)?;

        Ok(arr)
    }

    fn decode_fill<R: Reader>(
        this: &mut Vec<Self>,
        len: usize,
        decoder: &mut Decoder<R, C>,
    ) -> Result<()> {
        this.resize(len, 0);
        Self::decode_in_place_slice(this, decoder)
    }

    fn decode_in_place_slice<R: Reader>(
        this: &mut [Self],
        decoder: &mut Decoder<R, C>,
    ) -> Result<()> {
        match decoder.config().usize_len.cmp(&SizeWidth::host()) {
            Ordering::Equal => {
                if decoder.config().order == ByteOrder::host() {
                    type Bytes = [u8; core::mem::size_of::<usize>()];

                    let bytes = unsafe { &mut *(this as *mut [Self] as *mut [Bytes]) };

                    decoder.read(bytes.as_flattened_mut())?;

                    match decoder.config().order {
                        ByteOrder::Little => {
                            for n in this {
                                *n = (*n).to_le()
                            }
                        }
                        ByteOrder::Big => {
                            for n in this {
                                *n = (*n).to_be()
                            }
                        }
                    }
                } else {
                }
            }
            _ => {
                // I could optimize the case of a smaller size than host, but meh
                for n in this {
                    n.decode_in_place(decoder)?;
                }
            }
        }

        Ok(())
    }
}

impl<C> Decode<C> for isize {
    fn decode<R: Reader>(decoder: &mut Decoder<R, C>) -> Result<Self> {
        decoder.read_isize()
    }

    fn decode_array<R: Reader, const N: usize>(decoder: &mut Decoder<R, C>) -> Result<[Self; N]> {
        let mut arr = [0; N];

        Self::decode_in_place_slice(&mut arr, decoder)?;

        Ok(arr)
    }

    fn decode_fill<R: Reader>(
        this: &mut Vec<Self>,
        len: usize,
        decoder: &mut Decoder<R, C>,
    ) -> Result<()> {
        this.resize(len, 0);
        Self::decode_in_place_slice(this, decoder)
    }

    fn decode_in_place_slice<R: Reader>(
        this: &mut [Self],
        decoder: &mut Decoder<R, C>,
    ) -> Result<()> {
        usize::decode_in_place_slice(
            unsafe { &mut *(this as *mut [isize] as *mut [usize]) },
            decoder,
        )
    }
}

struct DropGuardOverwriteVal<'a, T: Copy>(&'a mut [T], T);

impl<'a, T: Copy> Deref for DropGuardOverwriteVal<'a, T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<'a, T: Copy> DerefMut for DropGuardOverwriteVal<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}

impl<'a, T: Copy> Drop for DropGuardOverwriteVal<'a, T> {
    fn drop(&mut self) {
        for v in &mut *self.0 {
            *v = self.1;
        }
    }
}

macro_rules! impl_non_zero_prim {
    ($($prim_ty:ty),*) => {
        $(
            impl<C> Decode<C> for ::core::num::NonZero<$prim_ty> {
                fn decode<R: Reader>(decoder: &mut Decoder<R, C>) -> Result<Self> {

                    <$prim_ty as Decode<C>>::decode(decoder)
                        .and_then(|v| ::core::num::NonZero::new(v).ok_or(Error::ZeroNonZero))
                }

                fn decode_array<R: Reader, const N: usize>(decoder: &mut Decoder<R, C>) -> Result<[Self; N]> {
                    let arr: [$prim_ty; N] = <$prim_ty as Decode<C>>::decode_array(decoder)?;

                    for i in 0..N {
                        if arr[i] == 0 {
                            return Err(Error::ZeroNonZero);
                        }
                    }

                    Ok(unsafe{crate::mem::transmute(arr)})
                }

                fn decode_in_place_slice<R: Reader>(this: &mut [Self], decoder: &mut Decoder<R, C>) -> Result<()> {
                    let slice = this as *mut [Self] as *mut [$prim_ty];

                    let mut guard = DropGuardOverwriteVal(unsafe { &mut *slice }, !0);

                    <$prim_ty as Decode<C>>::decode_in_place_slice(&mut guard, decoder)?; // Errors and panics will nuke any `0`s  due to the guard

                    let mut res = Ok(());
                    for i in &mut *guard {
                        if *i == 0 {
                            *i = !0;
                            res = Err(Error::ZeroNonZero);

                        }
                    }

                    core::mem::forget(guard);

                    res
                }

                fn decode_fill<R: Reader>(this: &mut Vec<Self>, len: usize, decoder: &mut Decoder<R, C>) -> Result<()> {
                    // Needed for safety: Since otherwise 0 values can show up in part of the vec that isn't initialized
                    this.truncate(0);
                    this.reserve(len);
                    let ptr = this.as_mut_ptr().cast::<$prim_ty>();

                    unsafe { core::ptr::write_bytes(ptr, 0, len); }

                    let slice = unsafe { core::slice::from_raw_parts_mut(ptr, len) };

                    <$prim_ty as Decode<C>>::decode_in_place_slice(slice, decoder)?;

                    for i in slice {
                        if *i == 0 {
                            return Err(Error::ZeroNonZero);
                        }
                    }

                    // SAFETY: Initialized above
                    unsafe{ this.set_len(len); }

                    Ok(())
                }
            }
        )*
    };
}

impl_non_zero_prim! {
    u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize
}

impl<C> Decode<C> for String {
    fn decode<R: Reader>(decoder: &mut Decoder<R, C>) -> Result<Self> {
        let len = usize::decode(decoder)?;
        let mut v = Vec::with_capacity(len);
        u8::decode_fill(&mut v, len, decoder)?;

        String::from_utf8(v).map_err(|_| Error::InvalidUtf8)
    }

    fn decode_in_place<R: Reader>(&mut self, decoder: &mut Decoder<R, C>) -> Result<()> {
        let r = unsafe { self.as_mut_vec() };
        r.truncate(0);
        let len = usize::decode(decoder)?;
        r.reserve(len);
        let ptr = r.as_mut_ptr();
        unsafe {
            core::ptr::write_bytes(ptr, 0, len);
        }
        let slice = unsafe { core::slice::from_raw_parts_mut(ptr, len) };
        u8::decode_in_place_slice(slice, decoder)?;

        str::from_utf8(slice).map_err(|_| Error::InvalidUtf8)?;

        unsafe {
            r.set_len(len);
        }

        Ok(())
    }
}
