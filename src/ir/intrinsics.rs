use lxca_derive::DebugWithConstants;

use crate::ir::constant::ConstantPool;
use crate::ir::expr::Value;
use crate::ir::pretty::PrettyPrint;

use crate::ir::types::{Signature, TypeBody};
use crate::ir::{constant::Constant, symbol::Symbol};

#[derive(Copy, Clone, DebugWithConstants, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum Intrinsic<'ir> {
    Arch(Constant<'ir, Symbol>, Constant<'ir, Symbol>),
    BlackBox,
    SpinLoop,
    Alloc,
    AllocZeroed,
    Dealloc,
    AllocArray,
    AllocArrayZeroed,
    Realloc,
    ReallocZeroed,
    ReturnAddress,
}

impl<'ir> Intrinsic<'ir> {
    pub fn check_signature(
        &self,
        sig: &Signature<'ir>,
        cparams: &[Value<'ir>],
        constants: &ConstantPool<'ir>,
    ) -> bool {
        match self {
            Self::Alloc
            | Self::AllocZeroed
            | Self::AllocArray
            | Self::AllocArrayZeroed
            | Self::Realloc
            | Self::ReallocZeroed
            | Self::Dealloc => {
                let ty = cparams[0].ty();

                let Some(t_sig) = ty.call_signature(constants) else {
                    return false;
                };

                let params = sig.params(constants);
                let retty = sig.ret_ty(constants);

                if !t_sig.sig_eq(sig, constants) {
                    if !matches!(self, Self::Dealloc) {
                        return false;
                    }

                    let t_params = t_sig.params(constants);
                    let t_retty = t_sig.ret_ty(constants);

                    if !t_retty.type_eq(retty, constants) {
                        return false;
                    }

                    match (params, t_params) {
                        ([ptr1, _, align1], [ptr2, align2]) => {
                            if !ptr1.type_eq(ptr2, constants) || !align1.type_eq(align2, constants)
                            {
                                return false;
                            }
                            // We validate that these types are correct at the next step
                        }
                        _ => return false,
                    }
                }

                match (self, retty.body(constants)) {
                    (Self::Dealloc, TypeBody::Void) => {}
                    (Self::Alloc | Self::AllocArray | Self::Realloc, TypeBody::Pointer(_)) => {}
                    _ => return false,
                }

                match (self, params) {
                    (Self::Alloc | Self::AllocZeroed, [size]) => match size.body(constants) {
                        TypeBody::Integer(_) => true,
                        _ => false,
                    },
                    (Self::Alloc | Self::AllocZeroed, [size, align]) => {
                        match [size.body(constants), align.body(constants)] {
                            [TypeBody::Integer(ity1), TypeBody::Integer(ity2)] => ity1 == ity2,
                            _ => false,
                        }
                    }
                    (Self::AllocArray | Self::AllocArrayZeroed, [count, esize]) => {
                        match [count.body(constants), esize.body(constants)] {
                            [TypeBody::Integer(ity1), TypeBody::Integer(ity2)] => ity1 == ity2,
                            _ => false,
                        }
                    }
                    (Self::AllocArray | Self::AllocArrayZeroed, [count, esize, align]) => {
                        match [
                            count.body(constants),
                            esize.body(constants),
                            align.body(constants),
                        ] {
                            [
                                TypeBody::Integer(ity1),
                                TypeBody::Integer(ity2),
                                TypeBody::Integer(ity3),
                            ] => ity1 == ity2 && ity2 == ity3,
                            _ => false,
                        }
                    }
                    (Self::Realloc | Self::ReallocZeroed, [ptr, new_size]) => {
                        match [ptr.body(constants), new_size.body(constants)] {
                            [TypeBody::Pointer(_), TypeBody::Integer(_)] => {
                                ptr.type_eq(retty, constants)
                            }
                            _ => false,
                        }
                    }
                    (Self::Realloc | Self::ReallocZeroed, [ptr, old_size, new_size]) => {
                        match [
                            ptr.body(constants),
                            old_size.body(constants),
                            new_size.body(constants),
                        ] {
                            [
                                TypeBody::Pointer(_),
                                TypeBody::Integer(ity1),
                                TypeBody::Integer(ity2),
                            ] => ptr.type_eq(retty, constants) && ity1 == ity2,
                            _ => false,
                        }
                    }
                    (Self::Realloc | Self::ReallocZeroed, [ptr, old_size, align, new_size]) => {
                        match [
                            ptr.body(constants),
                            old_size.body(constants),
                            align.body(constants),
                            new_size.body(constants),
                        ] {
                            [
                                TypeBody::Pointer(_),
                                TypeBody::Integer(ity1),
                                TypeBody::Integer(ity2),
                                TypeBody::Integer(ity3),
                            ] => ptr.type_eq(retty, constants) && ity1 == ity2 && ity2 == ity3,
                            _ => false,
                        }
                    }
                    (Self::Dealloc, [ptr]) => match [ptr.body(constants)] {
                        [TypeBody::Pointer(_)] => true,
                        _ => false,
                    },
                    (Self::Dealloc, [ptr, size]) => {
                        match [ptr.body(constants), size.body(constants)] {
                            [TypeBody::Pointer(_), TypeBody::Integer(_)] => {
                                ptr.type_eq(retty, constants)
                            }
                            _ => false,
                        }
                    }

                    (Self::Dealloc, [ptr, size, align]) => {
                        match [
                            ptr.body(constants),
                            size.body(constants),
                            align.body(constants),
                        ] {
                            [
                                TypeBody::Pointer(_),
                                TypeBody::Integer(ity1),
                                TypeBody::Integer(ity2),
                            ] => ity1 == ity2,
                            _ => false,
                        }
                    }
                    _ => false,
                }
            }
            Self::Arch(_, _) => true, // We cannot validate the signatures of architecture intrinsics without a def
            Self::BlackBox => match (sig.ret_ty(constants), sig.params(constants)) {
                (ty1, [ty2]) => ty1.type_eq(ty2, constants), // Check equality later
                _ => false,
            },
            Self::SpinLoop => {
                match (sig.ret_ty(constants).body(constants), sig.params(constants)) {
                    (TypeBody::Void, []) => true,
                    _ => false,
                }
            }
            Self::ReturnAddress => {
                match (sig.ret_ty(constants).body(constants), sig.params(constants)) {
                    (TypeBody::Pointer(_), []) => true,
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
            Intrinsic::Alloc => f.write_str("lxca::generic::alloc::alloc"),
            Intrinsic::AllocArray => f.write_str("lxca::generic::alloc::alloc_array"),
            Intrinsic::Realloc => f.write_str("lxca::generic::alloc::realloc"),
            Intrinsic::AllocZeroed => f.write_str("lxca::generic::alloc::alloc_zeroed"),
            Intrinsic::AllocArrayZeroed => f.write_str("lxca::generic::alloc::alloc_array_zeroed"),
            Intrinsic::ReallocZeroed => f.write_str("lxca::generic::alloc::realloc_zeroed"),
            Intrinsic::Dealloc => f.write_str("lxca::generic::alloc::dealloc"),
            Intrinsic::ReturnAddress => f.write_str("lxca::generic::debug::return_address"),
        }
    }
}
