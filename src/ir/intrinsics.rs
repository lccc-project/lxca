use std::mem::MaybeUninit;

use lxca_derive::DebugWithConstants;

use crate::ir::constant::ConstantPool;
use crate::ir::expr::{Value, ValueBody};
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
    Memcpy {inline: bool},
    Memmove {inline: bool},
    Memcmp {inline: bool},
    MemcmpEq {inline: bool},
    Memchr {inline: bool},
    Memset {inline: bool},
    Strcpy {inline: bool},
    Strchr {inline: bool},
    Strstr {inline: bool},
    Strcmp {inline: bool},
    StrcmpEq {inline: bool},
    Strlen {inline: bool},
}

fn map_slice_and<'a, const N: usize, T, R, F: FnMut(&'a T) -> R>(arr: &'a [T], mut f: F) -> Option<[R; N]> {
    if arr.len() != N {
        return None
    }
    let mut ret: MaybeUninit<[R; N]> = MaybeUninit::uninit();

    let ptr = ret.as_mut_ptr().cast::<R>();

    for (i, n) in arr.iter().enumerate() {
        unsafe {
            ptr.add(i).write(f(n))
        }
    }

    Some(unsafe {ret.assume_init()})
}

macro_rules! match_params {
    ($sig:expr => $constants:expr) => {
        map_slice_and($sig.params($constants), |ty| ty.body($constants))
    }
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
            Self::Memcpy {..} | Self::Memmove {..} => {
                match cparams {
                    [] => {}
                    [volatile] => {
                        match volatile.ty().body(constants) {
                            TypeBody::Integer(val) if val.width == 1 => {}
                            _ => return false
                        }
                        match volatile.body(constants) {
                            ValueBody::Integer(val) if let -1..=1 = val.borrow::<i128>().read(constants) => {}
                            _ => return false
                        }
                    }
                    _ => return false,
                }
                match map_slice_and(sig.params(constants) , |ty| ty.body(constants)){
                    Some([TypeBody::Pointer(destp), TypeBody::Pointer(srcp), TypeBody::Integer(_)]) => {
                                if !destp.ty(constants).type_eq(srcp.ty(constants), constants) {
                                    false
                                } else {
                                    match sig.ret_ty(constants).body(constants) {
                                        TypeBody::Void => true,
                                        TypeBody::Pointer(ptr) => ptr.ty(constants).type_eq(destp.ty(constants), constants),
                                        _ => false,
                                    }
                                }
                            }
                            _ => false,
                }
            }

            Self::Memcmp {..} | Self::MemcmpEq {..} => {
                match cparams {
                    [] => {}
                    [volatile] => {
                        match volatile.ty().body(constants) {
                            TypeBody::Integer(val) if val.width == 1 => {}
                            _ => return false
                        }
                        match volatile.body(constants) {
                            ValueBody::Integer(val) if let -1..=1 = val.borrow::<i128>().read(constants) => {}
                            _ => return false
                        }
                    }
                    _ => return false,
                }

                match (sig.ret_ty(constants).body(constants), map_slice_and(sig.params(constants), |sig| sig.body(constants))) {
                    (TypeBody::Integer(retty), Some([TypeBody::Pointer(src1), TypeBody::Pointer(src2), TypeBody::Integer(_)])) => {

                        retty.signed && src1.ty(constants).type_eq(src2.ty(constants), constants)
                    }
                    _ => false
                }
            }

            Self::Memchr { .. } | Self::Memset { .. } => {
                match cparams {
                    [] => {}
                    [volatile] => {
                        match volatile.ty().body(constants) {
                            TypeBody::Integer(val) if val.width == 1 => {}
                            _ => return false
                        }
                        match volatile.body(constants) {
                            ValueBody::Integer(val) if let -1..=1 = val.borrow::<i128>().read(constants) => {}
                            _ => return false
                        }
                    }
                    _ => return false,
                }

                match match_params!(sig => constants) {
                    Some([TypeBody::Pointer(src), TypeBody::Integer(_), TypeBody::Integer(_)]) => {
                        match sig.ret_ty(constants).body(constants) {
                            TypeBody::Pointer(ret) => ret.ty(constants).type_eq(src.ty(constants), constants),
                            TypeBody::Void => matches!(self, Self::Memset { .. }),
                            _ => false,
                        }
                    }

                    _ => false
                }
            }

            Self::Strcpy { .. } => {
                match cparams {
                    [] => {}
                    [volatile] => {
                        match volatile.ty().body(constants) {
                            TypeBody::Integer(val) if val.width == 1 => {}
                            _ => return false
                        }
                        match volatile.body(constants) {
                            ValueBody::Integer(val) if let -1..=1 = val.borrow::<i128>().read(constants) => {}
                            _ => return false
                        }
                    }
                    _ => return false,
                }

                match (match_params!(sig => constants), match_params!(sig => constants)) {
                    | (Some([TypeBody::Pointer(dest), TypeBody::Pointer(src)]), None) 
                    | (None, Some([TypeBody::Pointer(dest), TypeBody::Pointer(src), TypeBody::Integer(_)])) => {
                        match dest.ty(constants).body(constants) {
                            TypeBody::Char(_) => {
                                match sig.ret_ty(constants).body(constants) {
                                    TypeBody::Pointer(retty) => dest.ty(constants).type_eq(retty.ty(constants), constants) 
                                        && dest.ty(constants).type_eq(src.ty(constants), constants),
                                    TypeBody::Void => dest.ty(constants).type_eq(src.ty(constants), constants),
                                    _ => false,
                                }
                            }
                            _ => false
                        }
                    }
                    _ => false
                }
            }
            Self::Strcmp { .. } | Self::StrcmpEq { .. } => {
                match cparams {
                    [] => {}
                    [volatile] => {
                        match volatile.ty().body(constants) {
                            TypeBody::Integer(val) if val.width == 1 => {}
                            _ => return false
                        }
                        match volatile.body(constants) {
                            ValueBody::Integer(val) if let -1..=1 = val.borrow::<i128>().read(constants) => {}
                            _ => return false
                        }
                    }
                    _ => return false,
                }

                match (match_params!(sig => constants), match_params!(sig => constants)) {
                    | (Some([TypeBody::Pointer(dest), TypeBody::Pointer(src)]), None) 
                    | (None, Some([TypeBody::Pointer(dest), TypeBody::Pointer(src), TypeBody::Integer(_)])) => {
                        match dest.ty(constants).body(constants) {
                            TypeBody::Char(_) => {
                                match sig.ret_ty(constants).body(constants) {
                                    TypeBody::Integer(ity) => ity.signed && dest.ty(constants).type_eq(src.ty(constants), constants),
                                    _ => false,
                                }
                            }
                            _ => false
                        }
                    }
                    _ => false
                }
            }

            Self::Strlen { .. } => {
                match cparams {
                    [] => {}
                    [volatile] => {
                        match volatile.ty().body(constants) {
                            TypeBody::Integer(val) if val.width == 1 => {}
                            _ => return false
                        }
                        match volatile.body(constants) {
                            ValueBody::Integer(val) if let -1..=1 = val.borrow::<i128>().read(constants) => {}
                            _ => return false
                        }
                    }
                    _ => return false,
                }

                match (sig.ret_ty(constants).body(constants), match_params!(sig => constants), match_params!(sig => constants)) {
                    | (TypeBody::Integer(_), Some([TypeBody::Pointer(dest)]), None) 
                    | (TypeBody::Integer(_), None, Some([TypeBody::Pointer(dest), TypeBody::Integer(_)])) => {
                        match dest.ty(constants).body(constants) {
                            TypeBody::Char(_) => true,
                            _ => false,
                        }
                    }
                    _ => false,
                }
            }

            Self::Strchr { .. } => {
                match cparams {
                    [] => {}
                    [volatile] => {
                        match volatile.ty().body(constants) {
                            TypeBody::Integer(val) if val.width == 1 => {}
                            _ => return false
                        }
                        match volatile.body(constants) {
                            ValueBody::Integer(val) if let -1..=1 = val.borrow::<i128>().read(constants) => {}
                            _ => return false
                        }
                    }
                    _ => return false,
                }

                match (sig.ret_ty(constants).body(constants), match_params!(sig => constants), match_params!(sig => constants)) {
                    | (TypeBody::Pointer(retty), Some([TypeBody::Pointer(src), TypeBody::Char(cbits)]), None)
                    | (TypeBody::Pointer(retty), None, Some([TypeBody::Pointer(src), TypeBody::Char(cbits), TypeBody::Integer(_)])) => {
                        match src.ty(constants).body(constants) {
                            TypeBody::Char(srcbits) => cbits == srcbits && src.ty(constants).type_eq(retty.ty(constants), constants),
                            _ => false
                        }
                    }
                    _ => false
                }
            }

            Self::Strstr { .. } => {
                match cparams {
                    [] => {}
                    [volatile] => {
                        match volatile.ty().body(constants) {
                            TypeBody::Integer(val) if val.width == 1 => {}
                            _ => return false
                        }
                        match volatile.body(constants) {
                            ValueBody::Integer(val) if let -1..=1 = val.borrow::<i128>().read(constants) => {}
                            _ => return false
                        }
                    }
                    _ => return false,
                }

                match (sig.ret_ty(constants).body(constants), match_params!(sig => constants), match_params!(sig => constants)) {
                    | (TypeBody::Pointer(retty), Some([TypeBody::Pointer(src), TypeBody::Pointer(pat)]), None)
                    | (TypeBody::Pointer(retty), None, Some([TypeBody::Pointer(src), TypeBody::Pointer(pat), TypeBody::Integer(_)])) => {
                        match src.ty(constants).body(constants) {
                            TypeBody::Char(_) => src.ty(constants).type_eq(pat.ty(constants), constants) && src.ty(constants).type_eq(retty.ty(constants), constants),
                            _ => false
                        }
                    }
                    _ => false
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
            Intrinsic::Memcpy {inline: false} => f.write_str("lxca::generic::mem::memcpy"),
            Intrinsic::Memmove {inline: false} => f.write_str("lxca::generic::mem::memmove"),
            Intrinsic::Memcmp {inline: false} => f.write_str("lxca::generic::mem::memcmp"),
            Intrinsic::MemcmpEq {inline: false} => f.write_str("lxca::generic::mem::memcmpeq"),
            Intrinsic::Memchr { inline: false } => f.write_str("lxca::generic::mem::memchr"),
            Intrinsic::Memset { inline: false } => f.write_str("lxca::generic::mem::memchr"),
            Intrinsic::Memcpy {inline: true} => f.write_str("lxca::generic::mem::inline::memcpy"),
            Intrinsic::Memmove {inline: true} => f.write_str("lxca::generic::mem::inline::memmove"),
            Intrinsic::Memcmp {inline: true} => f.write_str("lxca::generic::mem::inline::memcmp"),
            Intrinsic::MemcmpEq {inline: true} => f.write_str("lxca::generic::mem::inline::memcmpeq"),
            Intrinsic::Memchr { inline: true } => f.write_str("lxca::generic::mem::inline::memchr"),
            Intrinsic::Memset { inline: true } => f.write_str("lxca::generic::mem::inline::memchr"),
            Intrinsic::Strcpy { inline: false } => f.write_str("lxca::generic::mem::strcpy"),
            Intrinsic::Strchr { inline: false } => f.write_str("lxca::generic::mem::strchr"),
            Intrinsic::Strstr { inline: false } => f.write_str("lxca::generic::mem::strstr"),
            Intrinsic::Strcmp { inline: false } => f.write_str("lxca::generic::mem::strcmp"),
            Intrinsic::StrcmpEq { inline: false } => f.write_str("lxca::generic::mem::strcmpeq"),
            Intrinsic::Strlen { inline: false } => f.write_str("lxca::generic::mem::strlen"),
            Intrinsic::Strcpy { inline: true } => f.write_str("lxca::generic::mem::inline::strcpy"),
            Intrinsic::Strchr { inline: true } => f.write_str("lxca::generic::mem::inline::strchr"),
            Intrinsic::Strstr { inline: true } => f.write_str("lxca::generic::mem::inline::strstr"),
            Intrinsic::Strcmp { inline: true } => f.write_str("lxca::generic::mem::inline::strcmp"),
            Intrinsic::StrcmpEq { inline: true } => f.write_str("lxca::generic::mem::inline::strcmpeq"),
            Intrinsic::Strlen { inline: true } => f.write_str("lxca::generic::mem::inline::strlen"),
        }
    }
}
