use super::IrCtx;
use crate::{
    ir::{
        constant::Internalizable,
        file::File,
        types::{IntType, Type},
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
