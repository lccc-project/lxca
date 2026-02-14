use core::marker::PhantomData;
use core::num::NonZeroU32;

use crate::{
    delegate_to_debug,
    fmt_helpers::DebugWithConstants,
    ir::{
        constant::{BoxOrConstant, ConstantPool, Internalizable},
        metadata::{MetadataBuilder, MetadataIter, NestedMetadata},
        symbol::Symbol,
        types::{IntType, Signature, SignatureBuilder, TypeBuilder},
    },
};

use super::{
    constant::Constant,
    metadata::{Metadata, MetadataList},
    types::Type,
};

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct Value<'ir> {
    ty: Type<'ir>,
    body: ValueBody<'ir>,
}

impl<'ir> Value<'ir> {
    pub fn ty(&self) -> &Type<'ir> {
        &self.ty
    }

    pub fn body<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> &'a ValueBody<'ir> {
        match &self.body {
            ValueBody::Interned(v) => v.get(pool).body(pool),
            body => body,
        }
    }
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

    pub fn global<S: Internalizable<'ir, Symbol>>(&mut self, sym: S) -> Value<'ir> {
        let val = self.pool.intern(sym);
        self.finish(ValueBody::GlobalAddr(val))
    }

    pub fn global_address<
        R: Internalizable<'ir, Type<'ir>>,
        F: for<'b> FnOnce(&mut TypeBuilder<'ir, 'b>) -> R,
        S: Internalizable<'ir, Symbol>,
    >(
        &mut self,
        ptr_builder: F,
        sym: S,
    ) -> Value<'ir> {
        self.with_ty(move |ty| ty.pointer(move |ptr| ptr.finish_with(ptr_builder)))
            .global(sym)
    }

    pub fn int<I: Internalizable<'ir, u128>>(&mut self, i: I) -> Value<'ir> {
        let val = self.pool.intern(i);
        self.finish(ValueBody::Integer(val))
    }

    pub fn string<S: Internalizable<'ir, str>>(&mut self, s: S) -> Value<'ir> {
        let val = self.pool.intern(s);
        self.finish(ValueBody::StringLiteral(val))
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

impl<'ir> NestedMetadata<'ir> for BasicBlock<'ir> {
    fn list_metadata(&self) -> &MetadataList<'ir> {
        &self.bb_metadata
    }

    fn next<'a>(&'a self, _: &'a ConstantPool<'ir>) -> Option<&'a Self> {
        None
    }
}

impl<'ir> BasicBlock<'ir> {
    pub fn metadata<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> MetadataIter<'ir, 'a, Self> {
        MetadataIter::new(self, pool)
    }
    pub fn stmts(&self) -> &[Statement<'ir>] {
        &self.stats
    }

    pub fn term(&self) -> &Terminator<'ir> {
        &self.term
    }

    pub fn label(&self) -> Constant<'ir, Symbol> {
        self.label
    }

    pub fn params(&self) -> &[(Constant<'ir, Symbol>, Type<'ir>)] {
        &self.params
    }
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
    BinaryOp(BinaryExpr<'ir>),
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

impl<'ir> NestedMetadata<'ir> for Expr<'ir> {
    fn list_metadata(&self) -> &MetadataList<'ir> {
        &self.meta
    }

    fn next<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> Option<&'a Self> {
        match &self.body {
            ExprBody::Interned(intern) => Some(intern.get(pool)),
            _ => None,
        }
    }
}

impl<'ir> Expr<'ir> {
    pub fn list_metadata<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> MetadataIter<'ir, 'a, Self> {
        MetadataIter::new(self, pool)
    }

    pub fn ty(&self) -> &Type<'ir> {
        &self.ty
    }

    pub fn body<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> &'a ExprBody<'ir> {
        match &self.body {
            ExprBody::Interned(intern) => intern.get(pool).body(pool),
            body => body,
        }
    }
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

    pub fn ty(&mut self, ty: Type<'ir>) -> &mut Self {
        self.ty = Some(ty);
        self
    }

    pub fn ty_with<F: FnOnce(&mut TypeBuilder<'ir, '_>) -> Type<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let ty = f(&mut TypeBuilder::new(self.pool));
        self.ty = Some(ty);
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

    pub fn binop<F: FnOnce(&mut BinaryOpBuilder<'ir, '_>) -> BinaryExpr<'ir>>(
        &mut self,
        f: F,
    ) -> Expr<'ir> {
        let binexpr = f(&mut BinaryOpBuilder::new(self.pool));

        self.finish(ExprBody::BinaryOp(binexpr))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnaryOp {
    BitNot,
    LogicNot,
    Minus,
}

delegate_to_debug!(UnaryOp);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

delegate_to_debug!(BinaryOp);

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct BinaryExpr<'ir>(
    pub BinaryOp,
    pub OverflowBehaviour,
    pub BoxOrConstant<'ir, Expr<'ir>>,
    pub BoxOrConstant<'ir, Expr<'ir>>,
);

pub struct BinaryOpBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    overflow: OverflowBehaviour,
    left: Option<BoxOrConstant<'ir, Expr<'ir>>>,
    right: Option<BoxOrConstant<'ir, Expr<'ir>>>,
}

impl<'ir, 'a> BinaryOpBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            overflow: OverflowBehaviour::Wrap,
            left: None,
            right: None,
        }
    }

    pub fn left<E: Internalizable<'ir, Expr<'ir>>>(&mut self, left: E) -> &mut Self {
        let left = self.pool.intern(left);
        self.left = Some(BoxOrConstant::Interned(left));
        self
    }

    pub fn right<E: Internalizable<'ir, Expr<'ir>>>(&mut self, right: E) -> &mut Self {
        let right = self.pool.intern(right);
        self.right = Some(BoxOrConstant::Interned(right));
        self
    }

    pub fn left_with<F: for<'b> FnOnce(&mut ExprBuilder<'ir, 'b>) -> Expr<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let left = f(&mut ExprBuilder::new(self.pool));
        self.left = Some(BoxOrConstant::Boxed(Box::new(left)));
        self
    }

    pub fn right_with<F: for<'b> FnOnce(&mut ExprBuilder<'ir, 'b>) -> Expr<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let right = f(&mut ExprBuilder::new(self.pool));
        self.right = Some(BoxOrConstant::Boxed(Box::new(right)));
        self
    }

    pub fn overflow(&mut self, behaviour: OverflowBehaviour) -> &mut Self {
        self.overflow = behaviour;
        self
    }

    pub fn finish(&mut self, op: BinaryOp) -> BinaryExpr<'ir> {
        let left = self.left.take().expect("Left Expression must be set");
        let right = self.right.take().expect("Right Expression must be set");

        BinaryExpr(op, self.overflow, left, right)
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
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
    pub id: Constant<'ir, Symbol>,
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

    pub fn call<F: for<'b> FnOnce(&mut CallBuilder<'ir, 'b>) -> CallTerm<'ir>>(
        &mut self,
        f: F,
    ) -> Terminator<'ir> {
        let term = f(&mut CallBuilder::new(self.pool));
        self.finish(TerminatorBody::Call(term))
    }
}

#[derive(Clone, DebugWithConstants, Hash, PartialEq, Eq)]
pub struct JumpTarget<'ir> {
    pub target: Constant<'ir, Symbol>,
    pub metadata: MetadataList<'ir>,
    pub args: Vec<Constant<'ir, Symbol>>,
}

pub struct JumpBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    metadata: Vec<Metadata<'ir>>,
    args: Vec<Constant<'ir, Symbol>>,
}

impl<'ir, 'a> JumpBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            metadata: Vec::new(),
            args: Vec::new(),
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

    pub fn finish<S: Internalizable<'ir, Symbol>>(&mut self, targ: S) -> JumpTarget<'ir> {
        let name = self.pool.intern(targ);
        let metadata = MetadataList(core::mem::take(&mut self.metadata));
        let args = core::mem::take(&mut self.args);

        JumpTarget {
            target: name,
            metadata,
            args,
        }
    }

    pub fn arg<S: Internalizable<'ir, Symbol>>(&mut self, var: S) -> &mut Self {
        let var = self.pool.intern(var);
        self.args.push(var);
        self
    }
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

pub struct CallBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    metadata: Vec<Metadata<'ir>>,
    params: Vec<Expr<'ir>>,
    sig: Option<Signature<'ir>>,
}

impl<'ir, 'a> CallBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self {
            pool,
            metadata: Vec::new(),
            params: Vec::new(),
            sig: None,
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

    pub fn signature<S: Internalizable<'ir, Signature<'ir>>>(&mut self, sig: S) -> &mut Self {
        let sig = self.pool.intern(sig);

        self.sig = Some(Signature::interned(sig));
        self
    }

    pub fn signature_with<F: for<'b> FnOnce(&mut SignatureBuilder<'ir, 'b>) -> Signature<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let sig = f(&mut SignatureBuilder::new(self.pool));

        self.sig = Some(sig);
        self
    }

    pub fn arg<F: for<'b> FnOnce(&mut ExprBuilder<'ir, 'b>) -> Expr<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let arg = f(&mut ExprBuilder::new(self.pool));

        self.params.push(arg);

        self
    }

    pub fn finish_with_next<
        E: for<'b> FnOnce(&mut ExprBuilder<'ir, 'b>) -> Expr<'ir>,
        J: for<'b> FnOnce(&mut JumpBuilder<'ir, 'b>) -> JumpTarget<'ir>,
    >(
        &mut self,
        jump: J,
        target: E,
    ) -> CallTerm<'ir> {
        let jump = jump(&mut JumpBuilder::new(self.pool));
        let call = self.finish(target);

        CallTerm {
            func: call,
            next: jump,
        }
    }

    pub fn finish<E: for<'b> FnOnce(&mut ExprBuilder<'ir, 'b>) -> Expr<'ir>>(
        &mut self,
        target: E,
    ) -> FunctionCall<'ir> {
        let target = target(&mut ExprBuilder::new(self.pool));

        let sig = self.sig.take().expect("Signature Must be set");
        let metadata = MetadataList(core::mem::take(&mut self.metadata));
        let params = core::mem::take(&mut self.params);

        FunctionCall {
            target,
            sig,
            call_metadata: metadata,
            params,
        }
    }
}
