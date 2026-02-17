use std::{
    borrow::{Borrow, Cow},
    ops::Deref,
};

use crate::{delegate_to_debug, ir::constant::ConstantPoolEntry};

use super::constant::{BorrowConstant, ConstantAs};

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct SymbolDef(String);

impl core::fmt::Debug for SymbolDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl SymbolDef {
    pub const fn new(st: String) -> SymbolDef {
        SymbolDef(st)
    }

    pub const fn as_symbol(&self) -> &Symbol {
        Symbol::new(self.0.as_str())
    }

    pub const fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn into_string(self) -> String {
        self.0
    }
}

impl Deref for SymbolDef {
    type Target = Symbol;
    fn deref(&self) -> &Self::Target {
        self.as_symbol()
    }
}

impl Borrow<Symbol> for SymbolDef {
    fn borrow(&self) -> &Symbol {
        self.as_symbol()
    }
}

impl PartialEq<Symbol> for SymbolDef {
    fn eq(&self, other: &Symbol) -> bool {
        self.as_symbol() == other
    }
}

impl PartialEq<&Symbol> for SymbolDef {
    fn eq(&self, other: &&Symbol) -> bool {
        self.as_symbol() == *other
    }
}

#[derive(Hash, PartialEq, Eq)]
pub struct Symbol(str);

impl core::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl core::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl Symbol {
    pub const fn new(st: &str) -> &Symbol {
        unsafe { &*(st as *const str as *const Symbol) }
    }

    pub fn into_def(&self) -> SymbolDef {
        SymbolDef(self.0.to_string())
    }

    pub const fn as_str(&self) -> &str {
        &self.0
    }
}

delegate_to_debug!(Symbol, SymbolDef);

impl<'a, 'ir> ConstantAs<'a, 'ir> for &'a Symbol {
    fn read_as(val: &'a Self::Constant) -> Self {
        Symbol::new(&val.0)
    }
}

impl<'ir> BorrowConstant<'ir> for Symbol {
    type Constant = SymbolDef;
}

impl ToOwned for Symbol {
    type Owned = SymbolDef;

    fn to_owned(&self) -> Self::Owned {
        self.into_def()
    }
}

impl<'a> From<Cow<'a, Symbol>> for SymbolDef {
    fn from(value: Cow<'a, Symbol>) -> Self {
        match value {
            Cow::Borrowed(v) => v.into_def(),
            Cow::Owned(other) => other,
        }
    }
}

impl<'ir> From<&Symbol> for SymbolDef {
    fn from(value: &Symbol) -> Self {
        value.into_def()
    }
}

impl<'ir> From<&Symbol> for ConstantPoolEntry<'ir> {
    fn from(value: &Symbol) -> Self {
        ConstantPoolEntry::Symbol(value.into_def())
    }
}

impl<'ir> From<Cow<'_, Symbol>> for ConstantPoolEntry<'ir> {
    fn from(value: Cow<Symbol>) -> Self {
        ConstantPoolEntry::Symbol(value.into())
    }
}

impl<'ir> From<Box<Symbol>> for ConstantPoolEntry<'ir> {
    fn from(value: Box<Symbol>) -> Self {
        ConstantPoolEntry::Symbol(value.into_def())
    }
}

#[macro_export]
macro_rules! sym {
    (# $ident:ident) => {
        const { $crate::ir::symbol::Symbol::new($crate::macros::_core::concat!("#", $crate::macros::_core::stringify!($ident))) }
    };
    ($($i:ident)::+) => {
        const { $crate::ir::symbol::Symbol::new($crate::macros::_core::concat!("" $(, $crate::macros::_core::stringify!($i), )"::"+ ))}
    };
    ($lit:literal) => {
        const {
            let _v: $crate::macros::_core::primitive::u128 = $lit; // Filter out string literals and negative numbers
            const { $crate::ir::symbol::Symbol::new($crate::macros::_core::stringify!($lit)) }
        }
    }
}
