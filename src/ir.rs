use crate::{
    binfmt::dec::{Decode, Decoder, Reader},
    fmt_helpers::DebugWithConstants,
    ir::{
        constant::ConstantPool,
        file::{File, FileBuilder},
    },
};
use std::marker::PhantomData;

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PhantomIrMarker<'ir>(pub(crate) PhantomData<&'ir mut &'ir mut ()>);

impl<'ir> core::fmt::Debug for PhantomIrMarker<'ir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("PhantomIrMarker")
    }
}

impl<'ir> DebugWithConstants<'ir> for PhantomIrMarker<'ir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _: &ConstantPool<'ir>) -> std::fmt::Result {
        f.write_str("PhantomIrMarker")
    }
}

impl<'ir, H: HasIr<'ir>> Decode<H> for PhantomIrMarker<'ir> {
    fn decode<R: Reader>(decoder: &mut Decoder<R, H>) -> crate::binfmt::dec::Result<Self> {
        Ok(decoder.context().marker())
    }
}

pub trait HasIr<'ir> {
    fn marker(&self) -> PhantomIrMarker<'ir>;
}

impl<'a, 'ir, I: HasIr<'ir>> HasIr<'ir> for &'a I {
    fn marker(&self) -> PhantomIrMarker<'ir> {
        <I as HasIr<'ir>>::marker(self)
    }
}

impl<'ir> HasIr<'ir> for PhantomIrMarker<'ir> {
    fn marker(&self) -> PhantomIrMarker<'ir> {
        *self
    }
}

pub struct IrCtx<'ir>(PhantomIrMarker<'ir>);

impl<'ir> HasIr<'ir> for IrCtx<'ir> {
    fn marker(&self) -> PhantomIrMarker<'ir> {
        self.0
    }
}
impl<'ir> IrCtx<'ir> {
    pub fn build_file(self, f: impl FnOnce(&mut FileBuilder<'ir>) -> File<'ir>) -> File<'ir> {
        f(&mut FileBuilder::new())
    }
}

pub fn with_context<R>(f: impl for<'ir> FnOnce(IrCtx<'ir>) -> R) -> R {
    f(IrCtx(PhantomIrMarker(PhantomData)))
}

pub mod constant;
pub mod decls;
pub mod expr;
pub mod file;
pub mod metadata;
pub mod pretty;
pub mod symbol;
pub mod types;

pub mod intrinsics;

#[cfg(any(test, feature = "test-files"))]
mod test_files_impl;

#[cfg(feature = "test-files")]
pub mod test_files {
    pub use super::test_files_impl::*;
}
