use std::collections::HashMap;

use crate::ir::{constant::{Constant, ConstantPool}, file::File, symbol::Symbol, types::{Signature, Type}};

enum SigOrType<'a, 'ir> {
    Type(&'a Type<'ir>),
    Signature(&'a Signature<'ir>),
}

pub enum Error<'ir> {
    WrongType(Constant<'ir, Symbol>, Constant<'ir, Type<'ir>>),
}

pub struct IrVerifier<'a, 'ir> {
    def_tys: HashMap<Constant<'ir, Symbol>, SigOrType<'a, 'ir>>,
    constants: &'a ConstantPool<'ir>,
}

impl<'a, 'ir> IrVerifier<'a, 'ir> {
    pub fn new(constants: &'a ConstantPool<'ir>) -> Self {
        Self { def_tys: HashMap::new(), constants }
    }
}

