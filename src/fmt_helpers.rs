use std::{marker::PhantomData, ops::Deref};

use crate::ir::constant::{Constant, ConstantPool};

pub trait DebugWithConstants<'ir> {
    fn fmt(&self, f: &mut core::fmt::Formatter, pool: &ConstantPool<'ir>) -> core::fmt::Result;
}

mod impls;

pub use lxca_derive::DebugWithConstants;

/// Helper type for implementing [`DebugWithConstants<'ir>`]
pub struct WithConstants<'a, 'ir, T: ?Sized>(pub &'a T, pub &'a ConstantPool<'ir>);

impl<'a, 'ir, T: ?Sized + core::fmt::Debug + 'a> Deref for WithConstants<'a, 'ir, T> {
    type Target = dyn core::fmt::Debug + 'a;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, 'ir, T: ?Sized + DebugWithConstants<'ir>> core::fmt::Debug for WithConstants<'a, 'ir, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <T as DebugWithConstants<'ir>>::fmt(self.0, f, self.1)
    }
}
