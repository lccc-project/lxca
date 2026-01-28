use super::IrCtx;
use crate::{
    ir::{
        constant::{Constant, Internalizable},
        expr::BinaryOp,
        file::File,
        types::{IntType, Signature, Type},
    },
    sym,
};

pub fn return_42<'ir>(targ: impl Internalizable<'ir, str>, ctx: IrCtx<'ir>) -> File<'ir> {
    ctx.build_file(|builder| {
        builder.declare(|f| {
            f.function(sym!(return_42), |f| {
                f.build_signature(|f| f.finish(Type::int(32)));
                f.build_basic_block(|bb| {
                    bb.finish(sym!(0), |term| {
                        term.return_val(|expr| expr.const_int(IntType::int(32), 42u128))
                    })
                });
                f.finish()
            })
        });

        builder.finish(targ)
    })
}

pub fn hello_world<'ir>(targ: impl Internalizable<'ir, str>, ctx: IrCtx<'ir>) -> File<'ir> {
    ctx.build_file(|builder| {
        let pchar = builder.build_constant(|cons| {
            cons.type_with(|ty| ty.pointer(|ptr| ptr.finish(Type::char(8))))
        });
        let puts_sig: Constant<Signature> = builder.build_constant(|cons| {
            cons.signature_with(|sig| sig.param(Type::intern(pchar)).finish(Type::int(32)))
        });
        builder
            .declare(|f| f.function(sym!(puts), |f| f.signature(puts_sig).finish()))
            .declare(|f| {
                f.function(sym!(main), |f| {
                    f.build_signature(|sig| sig.finish(Type::int(32)))
                        .build_basic_block(|bb| {
                            bb.finish(sym!(0), |term| {
                                term.call(|call| {
                                    call.arg(|expr| {
                                        expr.value(|v| {
                                            v.ty(Type::intern(pchar)).string("Hello World!\0")
                                        })
                                    })
                                    .signature(puts_sig)
                                    .finish_with_next(
                                        |jump| jump.arg(sym!(#return)).finish(sym!(1)),
                                        |expr| {
                                            expr.value(|f| {
                                                f.global_address(
                                                    |ty| {
                                                        ty.function(|_| {
                                                            Signature::interned(puts_sig)
                                                        })
                                                    },
                                                    sym!(puts),
                                                )
                                            })
                                        },
                                    )
                                })
                            })
                        })
                        .build_basic_block(|bb| {
                            bb.param(sym!(0), Type::int(32)).finish(sym!(1), |term| {
                                term.return_val(|expr| expr.const_int(IntType::int(32), 0u128))
                            })
                        })
                        .finish()
                })
            })
            .finish(targ)
    })
}

pub fn addition<'ir>(targ: impl Internalizable<'ir, str>, ctx: IrCtx<'ir>) -> File<'ir> {
    ctx.build_file(|builder| {
        builder.declare(|f| {
            f.function(sym!(return_42), |f| {
                f.build_signature(|f| f.finish(Type::int(32)));
                f.build_basic_block(|bb| {
                    bb.finish(sym!(0), |term| {
                        term.return_val(|expr| {
                            expr.ty(Type::int(32)).binop(|bin| {
                                bin.left_with(|expr| expr.const_int(IntType::int(32), 42u128))
                                    .right_with(|expr| expr.const_int(IntType::int(32), 27u128))
                                    .finish(BinaryOp::Add)
                            })
                        })
                    })
                });
                f.finish()
            })
        });

        builder.finish(targ)
    })
}
