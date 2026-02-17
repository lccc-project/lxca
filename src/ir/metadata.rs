use std::{
    any::{Any, TypeId},
    borrow::Borrow,
    iter::FusedIterator,
    mem::{ManuallyDrop, MaybeUninit},
    num::NonZero,
    ops::{Deref, DerefMut},
};

use crate::{
    delegate_to_debug,
    fmt_helpers::DebugWithConstants,
    ir::{
        constant::{ConstantPool, Internalizable},
        pretty,
        symbol::Symbol,
    },
    traits::DynHash,
};

use super::constant::{BorrowConstant, Constant};

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct MetadataList<'ir>(pub(crate) Vec<Metadata<'ir>>);

impl<'ir> super::pretty::PrettyPrint<'ir> for MetadataList<'ir> {
    fn fmt(&self, f: &mut super::pretty::PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
        for meta in &self.0 {
            f.write_str(" ")?;
            super::pretty::PrettyPrint::fmt(meta, f)?;
        }

        Ok(())
    }
}

impl<'ir> Deref for MetadataList<'ir> {
    type Target = Vec<Metadata<'ir>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'ir> DerefMut for MetadataList<'ir> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'ir> Borrow<[Metadata<'ir>]> for MetadataList<'ir> {
    fn borrow(&self) -> &[Metadata<'ir>] {
        self
    }
}

impl<'ir> BorrowConstant<'ir> for [Metadata<'ir>] {
    type Constant = MetadataList<'ir>;
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct MetadataFlags : u32 {
        const OPT_REQUIRED = 0x00000001;
        const OPT_PRESERVE = 0x00000002;
        const CG_REQUIRED = 0x00000004;
    }
}

delegate_to_debug!(MetadataFlags);

impl<'ir> super::pretty::PrettyPrint<'ir> for MetadataFlags {
    fn fmt(&self, f: &mut super::pretty::PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
        for flag in self.iter() {
            bitflags::bitflags_match! {flag, {
                Self::OPT_REQUIRED => f.write_str("opt required"),
                Self::OPT_PRESERVE => f.write_str("opt preserve"),
                Self::CG_REQUIRED => f.write_str("cg required"),
                _ => Ok(())
            }}?;

            f.write_str(" ")?;
        }

        Ok(())
    }
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct Metadata<'ir> {
    flags: MetadataFlags,
    body: MetadataBody<'ir>,
}

impl<'ir> super::pretty::PrettyPrint<'ir> for Metadata<'ir> {
    fn fmt(&self, f: &mut super::pretty::PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
        super::pretty::PrettyPrint::fmt(&self.flags, f)?;

        super::pretty::PrettyPrint::fmt(&self.body, f)
    }
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub enum MetadataBody<'ir> {
    Interned(Constant<'ir, [Metadata<'ir>]>),
    Simple(Constant<'ir, Symbol>),
    KeyValue {
        key: Constant<'ir, Symbol>,
        value: MetadataValue<'ir>,
    },
    Structural(Box<dyn DynMetadataTarget<'ir>>),
}

impl<'ir> super::pretty::PrettyPrint<'ir> for MetadataBody<'ir> {
    fn fmt(&self, f: &mut super::pretty::PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
        match self {
            MetadataBody::Interned(constant) => {
                f.write_fmt(format_args!("/* ${} */ [", constant.key()))?;
                let mut sep = "";
                for m in constant.get(f.constants()) {
                    f.write_str(sep)?;
                    sep = " ";
                    super::pretty::PrettyPrint::fmt(m, f)?;
                }
                f.write_str("]")
            }
            MetadataBody::Simple(constant) => {
                f.write_str("#[")?;
                pretty::PrettyPrint::fmt(constant, f)?;
                f.write_str("]")
            }
            MetadataBody::KeyValue { key, value } => {
                f.write_str("#[")?;
                pretty::PrettyPrint::fmt(key, f)?;
                f.write_str(" = ")?;
                pretty::PrettyPrint::fmt(value, f)?;
                f.write_str("]")
            }
            MetadataBody::Structural(targ) => {
                f.write_str("#[")?;
                pretty::PrettyPrint::fmt(targ, f)?;
                f.write_str("]")
            }
        }
    }
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub enum MetadataValue<'ir> {
    Body(Constant<'ir, [Metadata<'ir>]>),
    String(Constant<'ir, str>),
    Symbol(Constant<'ir, Symbol>),
    Integer(Constant<'ir, u128>),
    Label(Constant<'ir, Symbol>),
}

impl<'ir> pretty::PrettyPrint<'ir> for MetadataValue<'ir> {
    fn fmt(&self, f: &mut pretty::PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
        match self {
            MetadataValue::Body(constant) => {
                f.write_fmt(format_args!("/* ${} */ [", constant.key()))?;
                let mut sep = "";
                for m in constant.get(f.constants()) {
                    f.write_str(sep)?;
                    sep = " ";
                    super::pretty::PrettyPrint::fmt(m, f)?;
                }
                f.write_str("]")
            }
            MetadataValue::String(constant) => super::pretty::PrettyPrint::fmt(constant, f),
            MetadataValue::Symbol(constant) => super::pretty::PrettyPrint::fmt(constant, f),
            MetadataValue::Integer(constant) => super::pretty::PrettyPrint::fmt(constant, f),
            MetadataValue::Label(constant) => {
                f.write_str("local ")?;
                super::pretty::PrettyPrint::fmt(constant, f)
            }
        }
    }
}

/// # Safety
/// [`IrAny::Sentinel`] must be a unique type for the family of `Self` parameterized by the branding lifetime `'ir` :
/// that is, if `T<'ir>` denotes `Self`, And `U<'ir>: IrAny<'ir>` denotes some other type then, `T<'ir>::Sentinel = U<'ir>::Sentinel` if and only if `T` and `U` are the same type constructor.
/// Additionally [`IrAny::type_id`] must agree with [`Sentinel::type_id`][Any::type_id].
///
/// Typically, [`IrAny::Sentinel`] is the base of the family (IE. `T<'static>`).
pub unsafe trait IrAny<'ir>: 'ir {
    type Sentinel: Any
    where
        Self: Sized;

    fn type_id(&self) -> TypeId;

    #[doc(hidden)]
    fn __non_exhaustive(&self, _: core::convert::Infallible) -> !;
}

#[macro_export]
macro_rules! impl_ir_any {
    ($($seg:ident)::+) => {
        unsafe impl<'__ir> $crate::ir::metadata::IrAny<'__ir> for $($seg)::+ <'__ir> where $($seg)::+ <'static>: 'static {
            type Sentinel = $($seg)::+ <'static>;
            fn type_id(&self) -> $crate::macros::_core::any::TypeId {
                $crate::macros::_core::any::TypeId
            }

            fn __non_exhaustive(&self, inf: $crate::macros::_core::convert::Infallible) -> ! {
                match inf {}
            }
        }
    };
}

pub trait MetadataTarget<'ir>:
    IrAny<'ir>
    + DebugWithConstants<'ir>
    + Send
    + Sync
    + core::hash::Hash
    + core::cmp::Eq
    + Clone
    + super::pretty::PrettyPrint<'ir>
{
    const FLAGS: MetadataFlags = MetadataFlags::empty();
    fn encode(&self, pool: &mut ConstantPool<'ir>) -> Metadata<'ir>;
}

pub trait DynMetadataTarget<'ir>:
    IrAny<'ir> + DebugWithConstants<'ir> + Send + Sync + DynHash + super::pretty::PrettyPrint<'ir>
{
    fn encode(&self, pool: &mut ConstantPool<'ir>) -> Metadata<'ir>;

    fn dyn_clone(&self) -> Box<dyn DynMetadataTarget<'ir>>;

    fn dyn_eq(&self, other: &dyn DynMetadataTarget<'ir>) -> bool;
}

impl<'ir, M: MetadataTarget<'ir>> DynMetadataTarget<'ir> for M {
    fn encode(&self, pool: &mut ConstantPool<'ir>) -> Metadata<'ir> {
        <M as MetadataTarget<'ir>>::encode(self, pool)
    }

    fn dyn_clone(&self) -> Box<dyn DynMetadataTarget<'ir>> {
        Box::new(self.clone())
    }

    fn dyn_eq(&self, other: &dyn DynMetadataTarget<'ir>) -> bool {
        if let Some(val) = other.downcast_ref::<Self>() {
            self == val
        } else {
            false
        }
    }
}

impl<'ir, M: MetadataTarget<'ir>> From<M> for Box<dyn DynMetadataTarget<'ir>> {
    fn from(value: M) -> Self {
        Box::new(value)
    }
}

impl<'ir> dyn DynMetadataTarget<'ir> {
    fn check_downcast<T: IrAny<'ir>>(&self) -> bool {
        self.type_id() == TypeId::of::<T::Sentinel>()
    }
    pub fn downcast_ref<T: IrAny<'ir>>(&self) -> Option<&T> {
        if self.check_downcast::<T>() {
            Some(unsafe { &*(self as *const Self as *const T) })
        } else {
            None
        }
    }

    pub fn downcast_mut<T: IrAny<'ir>>(&mut self) -> Option<&mut T> {
        if self.check_downcast::<T>() {
            Some(unsafe { &mut *(self as *mut Self as *mut T) })
        } else {
            None
        }
    }

    pub fn downcast<T: IrAny<'ir>>(self: Box<Self>) -> Result<Box<T>, Box<Self>> {
        if self.check_downcast::<T>() {
            let ptr = Box::into_raw(self);

            Ok(unsafe { Box::from_raw(ptr as *mut T) })
        } else {
            Err(self)
        }
    }
}

impl<'ir> core::hash::Hash for dyn DynMetadataTarget<'ir> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.dyn_hash(state)
    }
}

impl<'ir> Clone for Box<dyn DynMetadataTarget<'ir>> {
    fn clone(&self) -> Self {
        self.dyn_clone()
    }
}

impl<'ir> core::cmp::PartialEq for dyn DynMetadataTarget<'ir> {
    fn eq(&self, other: &Self) -> bool {
        self.dyn_eq(other)
    }
}

impl<'ir> core::cmp::Eq for dyn DynMetadataTarget<'ir> {}

pub struct MetadataListBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    entries: Vec<Metadata<'ir>>,
}

impl<'ir, 'a> MetadataListBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            entries: Vec::new(),
        }
    }

    pub fn finish(&mut self) -> MetadataList<'ir> {
        MetadataList(core::mem::take(&mut self.entries))
    }

    pub fn entry<F: for<'b> FnOnce(&mut MetadataBuilder<'ir, 'b>) -> Metadata<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        self.entries.push(f(&mut MetadataBuilder::new(self.pool)));
        self
    }

    pub fn entries<
        I: IntoIterator,
        F: for<'b> FnMut(&mut MetadataBuilder<'ir, 'b>, I::Item) -> Metadata<'ir>,
    >(
        &mut self,
        mut f: F,
        iter: I,
    ) -> &mut Self {
        let f = &mut f;
        for udata in iter {
            self.entry(|builder| f(builder, udata));
        }
        self
    }

    pub fn structural<M: MetadataTarget<'ir>>(&mut self, meta: M) -> &mut Self {
        self.entry(move |builder| builder.with_flags(M::FLAGS).structural(meta))
    }

    pub fn simple<S: Internalizable<'ir, Symbol>>(&mut self, name: S) -> &mut Self {
        self.entry(move |builder| builder.simple(name))
    }

    pub fn intern<
        R: Internalizable<'ir, [Metadata<'ir>]>,
        F: for<'b> FnOnce(&mut MetadataListBuilder<'ir, 'b>) -> R,
    >(
        &mut self,
        f: F,
    ) -> &mut Self {
        self.entry(move |builder| builder.intern(f))
    }
}

pub struct MetadataBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    flags: MetadataFlags,
}

impl<'a, 'ir> MetadataBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            flags: MetadataFlags::empty(),
        }
    }

    fn finish(&mut self, body: MetadataBody<'ir>) -> Metadata<'ir> {
        Metadata {
            flags: self.flags,
            body,
        }
    }

    pub fn with_flags(&mut self, flags: MetadataFlags) -> &mut Self {
        self.flags |= flags;
        self
    }

    pub fn simple<S: Internalizable<'ir, Symbol>>(&mut self, name: S) -> Metadata<'ir> {
        let name = self.pool.intern(name);
        self.finish(MetadataBody::Simple(name))
    }

    pub fn intern<
        R: Internalizable<'ir, [Metadata<'ir>]>,
        F: for<'b> FnOnce(&mut MetadataListBuilder<'ir, 'b>) -> R,
    >(
        &mut self,
        f: F,
    ) -> Metadata<'ir> {
        let body = f(&mut MetadataListBuilder::new(self.pool));
        let entry = self.pool.intern(body);

        self.finish(MetadataBody::Interned(entry))
    }

    pub fn structural<M: Into<Box<dyn DynMetadataTarget<'ir>>>>(
        &mut self,
        target: M,
    ) -> Metadata<'ir> {
        self.finish(MetadataBody::Structural(target.into()))
    }

    pub fn key_value<
        S: Internalizable<'ir, Symbol>,
        F: for<'b> FnOnce(&mut MetadataValueBuilder<'ir, 'b>) -> MetadataValue<'ir>,
    >(
        &mut self,
        name: S,
        value: F,
    ) -> Metadata<'ir> {
        let value = value(&mut MetadataValueBuilder::new(self.pool));
        let name = self.pool.intern(name);

        self.finish(MetadataBody::KeyValue { key: name, value })
    }
}

pub struct MetadataValueBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
}

impl<'ir, 'a> MetadataValueBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self { pool }
    }

    pub fn int<I: Internalizable<'ir, u128>>(&mut self, val: I) -> MetadataValue<'ir> {
        MetadataValue::Integer(self.pool.intern(val))
    }

    pub fn string<S: Internalizable<'ir, str>>(&mut self, val: S) -> MetadataValue<'ir> {
        MetadataValue::String(self.pool.intern(val))
    }

    pub fn symbol<S: Internalizable<'ir, Symbol>>(&mut self, val: S) -> MetadataValue<'ir> {
        MetadataValue::Symbol(self.pool.intern(val))
    }

    pub fn label<S: Internalizable<'ir, Symbol>>(&mut self, label: S) -> MetadataValue<'ir> {
        MetadataValue::Label(self.pool.intern(label))
    }

    pub fn body<
        R: Internalizable<'ir, [Metadata<'ir>]>,
        F: for<'b> FnOnce(&mut MetadataListBuilder<'ir, 'b>) -> R,
    >(
        &mut self,
        f: F,
    ) -> MetadataValue<'ir> {
        let body = f(&mut MetadataListBuilder::new(self.pool));
        MetadataValue::Body(self.pool.intern(body))
    }
}

pub trait NestedMetadata<'ir> {
    fn list_metadata(&self) -> &MetadataList<'ir>;

    fn next<'a>(&'a self, cp: &'a ConstantPool<'ir>) -> Option<&'a Self>;
}

pub struct MetadataIter<'ir, 'iter, R> {
    pool: &'iter ConstantPool<'ir>,
    next: Option<&'iter R>,
    list: core::slice::Iter<'iter, Metadata<'ir>>,
}

impl<'ir, 'iter, R> MetadataIter<'ir, 'iter, R> {
    pub fn new(src: &'iter R, pool: &'iter ConstantPool<'ir>) -> Self {
        Self {
            pool,
            next: Some(src),
            list: const { &[] }.iter(),
        }
    }

    fn advance_src(&mut self) -> Option<()>
    where
        R: NestedMetadata<'ir>,
    {
        let src = self.next?;

        self.next = src.next(self.pool);
        self.list = src.list_metadata().0.iter();

        Some(())
    }
}

impl<'ir, 'iter, R: NestedMetadata<'ir>> Iterator for MetadataIter<'ir, 'iter, R> {
    type Item = &'iter Metadata<'ir>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(next) = self.list.next() {
                return Some(next);
            }

            self.advance_src()?;
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lower, upper) = self.list.size_hint();

        if self.next.is_none() {
            (lower, upper)
        } else {
            (lower, None)
        }
    }

    fn advance_by(&mut self, n: usize) -> Result<(), std::num::NonZero<usize>> {
        let Some(mut count) = NonZero::new(n) else {
            return Ok(());
        };

        loop {
            match self.list.advance_by(count.get()) {
                Ok(()) => break Ok(()),
                Err(n) => {
                    self.advance_src().ok_or(n)?;
                    count = n;
                }
            }
        }
    }

    fn count(mut self) -> usize
    where
        Self: Sized,
    {
        let mut count = 0;
        loop {
            count += self.list.len();
            let Some(()) = self.advance_src() else {
                return count;
            };
        }
    }
}

impl<'ir, 'iter, R: NestedMetadata<'ir>> FusedIterator for MetadataIter<'ir, 'iter, R> {}
