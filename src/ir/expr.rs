use core::marker::PhantomData;
use core::num::NonZeroU32;

use crate::{
    delegate_to_debug,
    fmt_helpers::DebugWithConstants,
    ir::{
        constant::{BoxOrConstant, ConstantPool, Internalizable},
        metadata::MetadataBuilder,
        symbol::Symbol,
        types::{IntType, Signature, TypeBuilder},
    },
};

use super::{
    PhantomIrMarker,
    constant::Constant,
    metadata::{Metadata, MetadataList},
    types::Type,
};

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct Value<'ir> {
    ty: Type<'ir>,
    body: ValueBody<'ir>,
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub enum ValueBody<'ir> {
    Interned(Constant<'ir, Value<'ir>>),
    Integer(Constant<'ir, u128>),
    StringLiteral(Constant<'ir, str>),
    ByteLiteral(Constant<'ir, [u8]>),
    Null,
    GlobalAddr(Constant<'ir, Symbol>),
    LocalAddr(Constant<'ir, str>),
    Uninit,
    Invalid,
    ZeroInit,
    Struct(StructValue<'ir>),
}

pub struct ValueBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    ty: Option<Type<'ir>>,
}

impl<'ir, 'a> ValueBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self { pool, ty: None }
    }

    fn finish(&mut self, body: ValueBody<'ir>) -> Value<'ir> {
        let ty = self.ty.take().expect("Type must be set");
        Value { ty, body }
    }

    pub fn ty(&mut self, ty: Type<'ir>) -> &mut Self {
        self.ty = Some(ty);
        self
    }

    pub fn with_ty<F: for<'b> FnOnce(&mut TypeBuilder<'ir, 'b>) -> Type<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let ty = f(&mut TypeBuilder::new(self.pool));
        self.ty(ty)
    }

    pub fn invalid(&mut self) -> Value<'ir> {
        self.finish(ValueBody::Invalid)
    }

    pub fn uninit(&mut self) -> Value<'ir> {
        self.finish(ValueBody::Uninit)
    }

    pub fn zero_init(&mut self) -> Value<'ir> {
        self.finish(ValueBody::ZeroInit)
    }

    pub fn null(&mut self) -> Value<'ir> {
        self.finish(ValueBody::Null)
    }

    pub fn int<I: Internalizable<'ir, u128>>(&mut self, i: I) -> Value<'ir> {
        let val = self.pool.intern(i);
        self.finish(ValueBody::Integer(val))
    }
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct StructValue<'ir> {
    pub fields: Vec<(Constant<'ir, str>, Value<'ir>)>,
    pub pad_fill: PadFill,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum PadFill {
    ZeroInit,
    Uninit,
}

delegate_to_debug!(PadFill);

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct BasicBlock<'ir> {
    bb_metadata: MetadataList<'ir>,
    label: Constant<'ir, Symbol>,
    params: Vec<(Constant<'ir, Symbol>, Type<'ir>)>,
    stats: Vec<Statement<'ir>>,
    term: Terminator<'ir>,
}

pub struct BasicBlockBuilder<'ir, 'b> {
    pool: &'b mut ConstantPool<'ir>,
    metadata: Vec<Metadata<'ir>>,
    params: Vec<(Constant<'ir, Symbol>, Type<'ir>)>,
    stats: Vec<Statement<'ir>>,
}

impl<'ir, 'a> BasicBlockBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            metadata: Vec::new(),
            params: Vec::new(),
            stats: Vec::new(),
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

    pub fn param<S: Internalizable<'ir, Symbol>>(&mut self, name: S, ty: Type<'ir>) -> &mut Self {
        let name = self.pool.intern(name);
        self.params.push((name, ty));
        self
    }

    pub fn finish<
        S: Internalizable<'ir, Symbol>,
        F: for<'b> FnOnce(&mut TermBuilder<'ir, 'b>) -> Terminator<'ir>,
    >(
        &mut self,
        name: S,
        f: F,
    ) -> BasicBlock<'ir> {
        let term = f(&mut TermBuilder::new(self.pool));
        let name = self.pool.intern(name);

        BasicBlock {
            bb_metadata: MetadataList(core::mem::take(&mut self.metadata)),
            label: name,
            params: core::mem::take(&mut self.params),
            stats: core::mem::take(&mut self.stats),
            term,
        }
    }
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub enum ExprBody<'ir> {
    Interned(Constant<'ir, Expr<'ir>>),
    Const(Value<'ir>),
    UnaryOp(UnaryOp, OverflowBehaviour, BoxOrConstant<'ir, Expr<'ir>>),
    BinaryOp(
        BinaryOp,
        OverflowBehaviour,
        BoxOrConstant<'ir, Expr<'ir>>,
        BoxOrConstant<'ir, Expr<'ir>>,
    ),
    ReadField(
        BoxOrConstant<'ir, Expr<'ir>>,
        Constant<'ir, Type<'ir>>,
        Constant<'ir, str>,
    ),
    ProjectField(
        BoxOrConstant<'ir, Expr<'ir>>,
        Constant<'ir, Type<'ir>>,
        Constant<'ir, str>,
    ),
    Struct(
        Constant<'ir, Type<'ir>>,
        Vec<(Constant<'ir, str>, Expr<'ir>)>,
    ),
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct Expr<'ir> {
    ty: Type<'ir>,
    meta: MetadataList<'ir>,
    body: ExprBody<'ir>,
}

pub struct ExprBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    metadata: Vec<Metadata<'ir>>,
    ty: Option<Type<'ir>>,
}

impl<'ir, 'a> ExprBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            metadata: Vec::new(),
            ty: None,
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

    fn finish(&mut self, body: ExprBody<'ir>) -> Expr<'ir> {
        let ty = self.ty.take().expect("Type must be set");

        let metadata = MetadataList(core::mem::take(&mut self.metadata));

        Expr {
            ty,
            meta: metadata,
            body,
        }
    }

    pub fn value<F: for<'b> FnOnce(&mut ValueBuilder<'ir, 'b>) -> Value<'ir>>(
        &mut self,
        f: F,
    ) -> Expr<'ir> {
        let mut value = f(&mut ValueBuilder::new(self.pool));

        if self.ty.is_none() {
            let ty = core::mem::replace(&mut value.ty, Type::void());
            let ty = self.pool.intern(ty);

            value.ty = Type::intern(ty);
            self.ty = Some(Type::intern(ty));
        }

        self.finish(ExprBody::Const(value))
    }

    pub fn const_int<I: Internalizable<'ir, u128>>(&mut self, ity: IntType, val: I) -> Expr<'ir> {
        self.value(move |builder| builder.with_ty(|builder| builder.int_type(ity)).int(val))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnaryOp {
    BitNot,
    LogicNot,
    Minus,
}

delegate_to_debug!(UnaryOp);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

delegate_to_debug!(BinaryOp);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum OverflowBehaviour {
    Wrap,
    Unchecked,
}

delegate_to_debug!(OverflowBehaviour);

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub enum Statement<'ir> {
    Assign(AssignStatement<'ir>),
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct AssignStatement<'ir> {
    pub id: Constant<'ir, str>,
    pub value: Expr<'ir>,
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct Terminator<'ir> {
    pub meta: MetadataList<'ir>,
    pub body: TerminatorBody<'ir>,
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub enum TerminatorBody<'ir> {
    Unreachable,
    Jump(JumpTarget<'ir>),
    Call(CallTerm<'ir>),
    Tailcall(FunctionCall<'ir>),
    Return(Expr<'ir>),
}

pub struct TermBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    metadata: Vec<Metadata<'ir>>,
}

impl<'ir, 'a> TermBuilder<'ir, 'a> {
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

    fn finish(&mut self, body: TerminatorBody<'ir>) -> Terminator<'ir> {
        let metadata = core::mem::take(&mut self.metadata);

        Terminator {
            meta: MetadataList(metadata),
            body: body,
        }
    }

    pub fn unreachable(&mut self) -> Terminator<'ir> {
        self.finish(TerminatorBody::Unreachable)
    }

    pub fn return_val<F: for<'b> FnOnce(&mut ExprBuilder<'ir, 'b>) -> Expr<'ir>>(
        &mut self,
        f: F,
    ) -> Terminator<'ir> {
        let expr = f(&mut ExprBuilder::new(self.pool));
        self.finish(TerminatorBody::Return(expr))
    }
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct JumpTarget<'ir> {
    pub target: Constant<'ir, str>,
    pub args: Vec<Constant<'ir, str>>,
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct CallTerm<'ir> {
    pub func: FunctionCall<'ir>,
    pub next: JumpTarget<'ir>,
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct FunctionCall<'ir> {
    pub target: Expr<'ir>,
    pub sig: Signature<'ir>,
    pub call_metadata: MetadataList<'ir>,
    pub params: Vec<Expr<'ir>>,
}
