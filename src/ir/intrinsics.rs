use lxca_derive::DebugWithConstants;

use crate::ir::constant::ConstantPool;
use crate::ir::pretty::PrettyPrint;

use crate::ir::types::{Signature, TypeBody};
use crate::ir::{constant::Constant, symbol::Symbol};

#[derive(Copy, Clone, DebugWithConstants, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum Intrinsic<'ir> {
    Arch(Constant<'ir, Symbol>, Constant<'ir, Symbol>),
    BlackBox,
    SpinLoop,
}

impl<'ir> Intrinsic<'ir> {
    pub fn check_signature(&self, sig: &Signature<'ir>, constants: &ConstantPool<'ir>) -> bool {
        match self {
            Self::Arch(_, _) => true, // We cannot validate the signatures of architecture intrinsics without a def
            Self::BlackBox => match (sig.ret_ty(constants), sig.params(constants)) {
                (ty1, [ty2]) => true, // Check equality later
                _ => false,
            },
            Self::SpinLoop => {
                match (sig.ret_ty(constants).body(constants), sig.params(constants)) {
                    (TypeBody::Void, []) => true,
                    _ => false,
                }
            }
        }
    }
}

impl<'ir> PrettyPrint<'ir> for Intrinsic<'ir> {
    fn fmt(&self, f: &mut super::pretty::PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
        match self {
            Intrinsic::Arch(arch, intrin_name) => {
                f.write_str("lxca::")?;
                arch.fmt(f)?;
                f.write_str("::")?;
                intrin_name.fmt(f)
            }
            Intrinsic::BlackBox => f.write_str("lxca::generic::hint::black_box"),
            Intrinsic::SpinLoop => f.write_str("lxca::generic::hint::spin_loop"),
        }
    }
}
