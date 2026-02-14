use lxca_derive::DebugWithConstants;

use crate::{
    delegate_to_debug,
    ir::{
        constant::{ConstantPool, Internalizable},
        expr::BasicBlockBuilder,
        metadata::{Metadata, MetadataBuilder, MetadataIter, NestedMetadata},
        types::SignatureBuilder,
    },
};

use super::{
    PhantomIrMarker,
    constant::Constant,
    expr::{BasicBlock, Value},
    metadata::MetadataList,
    symbol::Symbol,
    types::{Signature, Type},
};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Linkage {
    External,
    Internal,
    Weak,
    Const,
}

delegate_to_debug!(Linkage);

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct Declaration<'ir> {
    metadata: MetadataList<'ir>,
    linkage: Linkage,
    name: Constant<'ir, Symbol>,
    body: DeclarationBody<'ir>,
}

pub struct DeclBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    linkage: Linkage,
    metadata: Vec<Metadata<'ir>>,
}

impl<'ir, 'a> DeclBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            linkage: Linkage::Internal,
            metadata: Vec::new(),
        }
    }

    fn finish(
        &mut self,
        name: Constant<'ir, Symbol>,
        body: DeclarationBody<'ir>,
    ) -> Declaration<'ir> {
        Declaration {
            metadata: MetadataList(core::mem::take(&mut self.metadata)),
            linkage: self.linkage,
            name,
            body,
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

    pub fn linkage(&mut self, link: Linkage) -> &mut Self {
        self.linkage = link;
        self
    }

    pub fn object<
        S: Internalizable<'ir, Symbol>,
        F: for<'b> FnOnce(&mut ObjectBodyBuilder<'ir, 'b>) -> ObjectBody<'ir>,
    >(
        &mut self,
        name: S,
        f: F,
    ) -> Declaration<'ir> {
        let body = f(&mut ObjectBodyBuilder::new(self.pool));
        let name = self.pool.intern(name);
        self.finish(name, DeclarationBody::Object(body))
    }

    pub fn function<
        S: Internalizable<'ir, Symbol>,
        F: for<'b> FnOnce(&mut FunctionBodyBuilder<'ir, 'b>) -> FunctionBody<'ir>,
    >(
        &mut self,
        name: S,
        f: F,
    ) -> Declaration<'ir> {
        let body = f(&mut FunctionBodyBuilder::new(self.pool));
        let name = self.pool.intern(name);
        self.finish(name, DeclarationBody::Function(body))
    }
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub enum DeclarationBody<'ir> {
    Object(ObjectBody<'ir>),
    Function(FunctionBody<'ir>),
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct FunctionBody<'ir> {
    ty: Signature<'ir>,
    function_metadata: MetadataList<'ir>,
    body: Option<Vec<BasicBlock<'ir>>>,
    param_names: Option<Vec<Constant<'ir, Symbol>>>,
}

impl<'ir> NestedMetadata<'ir> for FunctionBody<'ir> {
    fn list_metadata(&self) -> &MetadataList<'ir> {
        &self.function_metadata
    }

    fn next<'a>(&'a self, cp: &'a ConstantPool<'ir>) -> Option<&'a Self> {
        None
    }
}

impl<'ir> FunctionBody<'ir> {
    pub fn is_extern(&self) -> bool {
        self.body.is_some()
    }

    pub fn body(&self) -> Option<&[BasicBlock<'ir>]> {
        self.body.as_deref()
    }

    pub fn signature(&self) -> &Signature<'ir> {
        &self.ty
    }

    pub fn metadata<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> MetadataIter<'ir, 'a, Self> {
        MetadataIter::new(self, pool)
    }

    pub fn param_names(&self) -> &[Constant<'ir, Symbol>] {
        self.param_names.as_deref().unwrap_or(const { &[] })
    }
}

pub struct FunctionBodyBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    sig: Option<Signature<'ir>>,
    body: Option<Vec<BasicBlock<'ir>>>,
    metadata: Vec<Metadata<'ir>>,
    param_names: Option<Vec<Constant<'ir, Symbol>>>,
}

impl<'ir, 'a> FunctionBodyBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            sig: None,
            body: None,
            metadata: Vec::new(),
            param_names: None,
        }
    }

    pub fn finish(&mut self) -> FunctionBody<'ir> {
        let sig = self.sig.take().expect("Must set signature first");
        let body = self.body.take();
        let metadata = core::mem::take(&mut self.metadata);
        FunctionBody {
            ty: sig,
            body,
            function_metadata: MetadataList(metadata),
            param_names: self.param_names.take(),
        }
    }

    pub fn with_param_name(&mut self, name: Constant<'ir, Symbol>) -> &mut Self {
        self.param_names.get_or_insert_with(Vec::new).push(name);
        self
    }

    pub fn with_metadata<F: for<'b> FnOnce(&mut MetadataBuilder<'ir, 'b>) -> Metadata<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let meta = f(&mut MetadataBuilder::new(self.pool));
        self.metadata.push(meta);
        self
    }

    pub fn signature<S: Internalizable<'ir, Signature<'ir>>>(&mut self, sig: S) -> &mut Self {
        let sig = self.pool.intern(sig);
        self.sig = Some(Signature::interned(sig));
        self
    }

    pub fn build_signature<F: for<'b> FnOnce(&mut SignatureBuilder<'ir, 'b>) -> Signature<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        self.sig = Some(f(&mut SignatureBuilder::new(self.pool)));
        self
    }

    pub fn build_basic_block<
        F: for<'b> FnOnce(&mut BasicBlockBuilder<'ir, 'b>) -> BasicBlock<'ir>,
    >(
        &mut self,
        f: F,
    ) -> &mut Self {
        let bb = f(&mut BasicBlockBuilder::new(self.pool));

        self.body.get_or_insert_with(Vec::new).push(bb);
        self
    }
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct ObjectBody<'ir> {
    ty: Type<'ir>,
    object_metadata: MetadataList<'ir>,
    initializer: Option<Initializer<'ir>>,
}

pub struct ObjectBodyBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
}

impl<'ir, 'a> ObjectBodyBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self { pool }
    }
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub enum Initializer<'ir> {
    ZeroInit,
    Constant(Value<'ir>),
    Include(Constant<'ir, str>),
}
