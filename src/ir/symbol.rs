use std::{
    borrow::{Borrow, Cow},
    ops::Deref,
};

use crate::{delegate_to_debug, ir::constant::{BorrowConstant, BoxOrConstant, Constant, ConstantAs, ConstantPoolEntry, Internalizable}};


pub trait SymbolTypeMatch<S: ?Sized>{}

impl SymbolTypeMatch<Symbol> for Symbol {}

impl SymbolTypeMatch<Symbol> for SymbolDef {}

impl SymbolTypeMatch<SymbolDef> for Symbol {}

impl<'ir, S: ?Sized, T: SymbolTypeMatch<S> + ?Sized> SymbolTypeMatch<Constant<'ir, S>> for T{}
impl<'ir, S: ?Sized, T: SymbolTypeMatch<S> + ?Sized> SymbolTypeMatch<BoxOrConstant<'ir, S>> for T{}
impl<'a, S: ?Sized, T: SymbolTypeMatch<S> + ?Sized> SymbolTypeMatch<&'a S> for T{}
impl<'a, S: ?Sized + ToOwned, T: SymbolTypeMatch<S> + ?Sized> SymbolTypeMatch<Cow<'a, S>> for T{}
impl<'a, S: ?Sized, T: SymbolTypeMatch<S> + ?Sized> SymbolTypeMatch<&'a mut S> for T{}
impl<S: ?Sized, T: SymbolTypeMatch<S> + ?Sized> SymbolTypeMatch<Box<S>> for T{}

mod private {
    pub trait Sealed<S: ?Sized> {}
}

use private::Sealed;

pub trait InternalizeAsSym<'ir, S: ?Sized + BorrowConstant<'ir>>: Internalizable<'ir, S> + Sealed<S>{}

impl<S: ?Sized, R: ?Sized> Sealed<S> for R where S: SymbolTypeMatch<R>{}

impl<'ir, S: ?Sized + BorrowConstant<'ir>, R: Sealed<S> + Internalizable<'ir, S>> InternalizeAsSym<'ir, S> for R {}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct SymbolDef(String);

impl core::fmt::Debug for SymbolDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl core::fmt::Display for SymbolDef {
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

    pub fn components(&self) -> Components<'_> {
        Components(self.0.split("::"))
    }
}

macro_rules! sym_wrapper {
    ($id:ident) => {
        #[derive(Hash, PartialEq, Eq)]
        pub struct $id(Symbol);

        impl core::fmt::Debug for $id {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                self.0.fmt(f)
            }
        }
        impl core::fmt::Display for $id {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                self.0.fmt(f)
            }
        }

        impl $id {
            pub const fn new(st: &str) -> &Self {
                unsafe { &*(st as *const str as *const Self)}
            }

            pub const fn as_str(&self) -> &str {
                self.0.as_str()
            }

            pub const fn as_symbol(&self) -> &Symbol {
                &self.0
            }

            pub fn into_def(&self) -> SymbolDef {
                self.0.into_def()
            }
        }

        impl<'ir> BorrowConstant<'ir> for $id {
            type Constant = SymbolDef;
        }

        impl<'a, 'ir> ConstantAs<'a, 'ir> for &'a $id {
            fn read_as(val: &'a Self::Constant) -> Self {
                $id::new(&val.0)
            }
        }

        impl<'ir> From<&$id> for SymbolDef {
            fn from(value: &$id) -> Self {
                value.into_def()
            }
        }

        impl<'ir> From<&$id> for ConstantPoolEntry<'ir> {
            fn from(value: &$id) -> Self {
                ConstantPoolEntry::Symbol(value.into_def())
            }
        }

        impl PartialEq<$id> for SymbolDef {
            fn eq(&self, other: &$id) -> bool {
                self.as_symbol() == other.as_symbol()
            }
        }

        impl PartialEq<&$id> for SymbolDef {
            fn eq(&self, other: &&$id) -> bool {
                self.as_symbol() == other.as_symbol()
            }
        }

        impl SymbolTypeMatch<$id> for $id{}
        impl SymbolTypeMatch<$id> for Symbol {}
        impl SymbolTypeMatch<$id> for SymbolDef {}

        impl SymbolTypeMatch<SymbolDef> for $id {}

        delegate_to_debug!($id);
        crate::ir::pretty::delegate_to_display!($id);
    };
}

sym_wrapper!(VarSym);
sym_wrapper!(LabelSym);

pub struct Components<'a>(core::str::Split<'a, &'static str>);

impl<'a> Iterator for Components<'a> {
    type Item = &'a Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Symbol::new)
    }
}

impl<'a> DoubleEndedIterator for Components<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next().map(Symbol::new)
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
        const { $crate::ir::symbol::VarSym::new($crate::macros::_core::concat!("#", $crate::macros::_core::stringify!($ident))) }
    };
    ($($i:ident)::+) => {
        const { $crate::ir::symbol::Symbol::new($crate::macros::_core::concat!("" $(, $crate::macros::_core::stringify!($i), )"::"+ ))}
    };
    (%$lit:literal) => {
        const {
            let _v: $crate::macros::_core::primitive::u128 = $lit; // Filter out string literals and negative numbers
            const { $crate::ir::symbol::VarSym::new($crate::macros::_core::concat!("%", $crate::macros::_core::stringify!($lit))) }
        }
    };
    (%$ident:ident) => {
        const {
            const { $crate::ir::symbol::VarSym::new($crate::macros::_core::concat!("%", $crate::macros::_core::stringify!($ident))) }
        }
    };
    (@$lit:literal) => {
        const {
            let _v: $crate::macros::_core::primitive::u128 = $lit; // Filter out string literals and negative numbers
            const { $crate::ir::symbol::LabelSym::new($crate::macros::_core::concat!("@", $crate::macros::_core::stringify!($lit))) }
        }
    };
    (@$ident:ident) => {
        const {
            const { $crate::ir::symbol::LabelSym::new($crate::macros::_core::concat!("@", $crate::macros::_core::stringify!($ident))) }
        }
    };
}
