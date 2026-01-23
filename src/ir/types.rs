use lxca_derive::DebugWithConstants;

use crate::{
    delegate_to_debug,
    ir::{
        constant::{ConstantPool, Internalizable},
        metadata::{Metadata, MetadataBuilder, MetadataList},
        symbol::Symbol,
    },
};

use super::constant::Constant;

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct Type<'ir> {
    metadata: MetadataList<'ir>,
    body: TypeBody<'ir>,
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub enum TypeBody<'ir> {
    Interned(Constant<'ir, Type<'ir>>),
    Named(Constant<'ir, Symbol>),
    Integer(IntType),
    Char(u16),
    Pointer(PointerType<'ir>),
    Function(Signature<'ir>),
    Void,
}

pub struct TypeBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    metadata: Vec<Metadata<'ir>>,
}

impl<'ir, 'a> TypeBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            metadata: Vec::new(),
        }
    }

    pub fn with_metadata<F: for<'b> FnOnce(&mut MetadataBuilder<'ir, 'b>) -> Metadata<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let meta = f(&mut MetadataBuilder::new(self.pool));
        self.metadata.push(meta);
        self
    }

    fn finish(&mut self, body: TypeBody<'ir>) -> Type<'ir> {
        let metadata = MetadataList(core::mem::take(&mut self.metadata));

        Type { metadata, body }
    }

    pub fn void(&mut self) -> Type<'ir> {
        self.finish(TypeBody::Void)
    }

    pub fn int_type(&mut self, ity: IntType) -> Type<'ir> {
        self.finish(TypeBody::Integer(ity))
    }

    pub fn int(&mut self, width: u16) -> Type<'ir> {
        self.int_type(IntType::int(width))
    }

    pub fn uint(&mut self, width: u16) -> Type<'ir> {
        self.int_type(IntType::uint(width))
    }

    pub fn char(&mut self, width: u16) -> Type<'ir> {
        self.finish(TypeBody::Char(width))
    }

    pub fn function<F: for<'b> FnOnce(&mut SignatureBuilder<'ir, 'b>) -> Signature<'ir>>(
        &mut self,
        f: F,
    ) -> Type<'ir> {
        let sig = f(&mut SignatureBuilder::new(self.pool));
        self.finish(TypeBody::Function(sig))
    }

    pub fn named<S: Internalizable<'ir, Symbol>>(&mut self, name: S) -> Type<'ir> {
        let name = self.pool.intern(name);
        self.finish(TypeBody::Named(name))
    }
}

impl<'ir> Type<'ir> {
    pub const fn int(width: u16) -> Type<'ir> {
        Type {
            metadata: MetadataList(Vec::new()),
            body: TypeBody::Integer(IntType::int(width)),
        }
    }

    pub const fn uint(width: u16) -> Type<'ir> {
        Type {
            metadata: MetadataList(Vec::new()),
            body: TypeBody::Integer(IntType::uint(width)),
        }
    }

    pub const fn void() -> Type<'ir> {
        Type {
            metadata: MetadataList(Vec::new()),
            body: TypeBody::Void,
        }
    }

    pub const fn intern(ty: Constant<'ir, Type<'ir>>) -> Type<'ir> {
        Type {
            metadata: MetadataList(Vec::new()),
            body: TypeBody::Interned(ty),
        }
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct IntType {
    signed: bool,
    width: u16,
}

impl core::fmt::Debug for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}{}",
            if self.signed { "i" } else { "u" },
            self.width
        ))
    }
}

impl IntType {
    pub const fn int(width: u16) -> IntType {
        IntType {
            signed: true,
            width,
        }
    }

    pub const fn uint(width: u16) -> IntType {
        IntType {
            signed: false,
            width,
        }
    }
}

delegate_to_debug!(IntType);

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct PointerType<'ir> {
    ty: Constant<'ir, Type<'ir>>,
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub enum Signature<'ir> {
    Interned(Constant<'ir, Signature<'ir>>),
    InPlace {
        tag: Constant<'ir, str>,
        ret_ty: Constant<'ir, Type<'ir>>,
        params: Vec<Type<'ir>>,
    },
}

pub struct SignatureBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    tag: Option<Constant<'ir, str>>,
    params: Vec<Type<'ir>>,
}

impl<'ir, 'a> SignatureBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            tag: None,
            params: Vec::new(),
        }
    }

    pub fn finish<T: Internalizable<'ir, Type<'ir>>>(&mut self, ret_ty: T) -> Signature<'ir> {
        let tag = self.tag.take().unwrap_or_else(|| self.pool.intern("C"));
        let ret_ty = self.pool.intern(ret_ty);
        Signature::InPlace {
            tag,
            ret_ty,
            params: core::mem::take(&mut self.params),
        }
    }

    pub fn build_return<
        R: Internalizable<'ir, Type<'ir>>,
        F: for<'b> FnOnce(&mut TypeBuilder<'ir, 'b>) -> R,
    >(
        &mut self,
        f: F,
    ) -> Signature<'ir> {
        let retty = f(&mut TypeBuilder::new(self.pool));
        self.finish(retty)
    }

    pub fn with_tag<S: Internalizable<'ir, str>>(&mut self, tag: S) -> &mut Self {
        let tag = self.pool.intern(tag);
        self.tag = Some(tag);
        self
    }

    pub fn param(&mut self, ty: Type<'ir>) -> &mut Self {
        self.params.push(ty);
        self
    }

    pub fn build_param<F: for<'b> FnOnce(&mut TypeBuilder<'ir, 'b>) -> Type<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let ty = f(&mut TypeBuilder::new(self.pool));
        self.param(ty);
        self
    }
}
