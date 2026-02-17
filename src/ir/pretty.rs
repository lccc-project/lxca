use std::{
    ops::{Deref, DerefMut},
    rc::Rc,
    sync::Arc,
};

use uuid::Uuid;

use crate::ir::{
    constant::ConstantPool,
    symbol::{Symbol, SymbolDef},
};

pub trait PrettyPrint<'ir> {
    fn fmt(&self, f: &mut PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result;
}

macro_rules! impl_smart_ptr{
    (for<$base_name:ident $(: ?$sized:ident)? $(, $aux_names:ident)* $(,)?> $ty:ty) => {
        impl<'ir, $base_name: PrettyPrint<'ir> $(+ ?$sized)?, $($aux_names),*> PrettyPrint<'ir> for $ty {
            fn fmt(&self, __f:&mut PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
                <$base_name as PrettyPrint>::fmt(self, __f)
            }
        }
    };
}

impl_smart_ptr!(for<T: ?Sized> &T);
impl_smart_ptr!(for<T: ?Sized> &mut T);
impl_smart_ptr!(for<T: ?Sized> Box<T>);
impl_smart_ptr!(for<T: ?Sized> Arc<T>);
impl_smart_ptr!(for<T: ?Sized> Rc<T>);

macro_rules! delegate_to_display {
    ($($ty:ty),*) => {
        $(
            impl<'ir> PrettyPrint<'ir> for $ty {
                fn fmt(&self, __f: &mut PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
                    core::fmt::Display::fmt(self, __f)
                }
            }
        )*
    };
}

pub(crate) use delegate_to_display;

delegate_to_display!(
    u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize, f32, f64, bool, Symbol,
    SymbolDef, Uuid
);

impl<'ir> PrettyPrint<'ir> for str {
    fn fmt(&self, f: &mut PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
        f.write_str("\"")?;
        core::fmt::Display::fmt(&self.escape_default(), f)?;
        f.write_str("\"")
    }
}

impl<'ir> PrettyPrint<'ir> for String {
    fn fmt(&self, f: &mut PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
        f.write_str("\"")?;
        f.write_str(self)?;
        f.write_str("\"")
    }
}

pub struct PrettyPrinter<'a, 'fmt, 'ir> {
    fmt: &'a mut core::fmt::Formatter<'fmt>,
    constants: &'a ConstantPool<'ir>,
    tab_level: usize,
}

impl<'a, 'fmt, 'ir> Deref for PrettyPrinter<'a, 'fmt, 'ir> {
    type Target = core::fmt::Formatter<'fmt>;

    fn deref(&self) -> &Self::Target {
        self.fmt
    }
}

impl<'a, 'fmt, 'ir> DerefMut for PrettyPrinter<'a, 'fmt, 'ir> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.fmt
    }
}

impl<'a, 'fmt, 'ir> PrettyPrinter<'a, 'fmt, 'ir> {
    pub fn write_tabs(&mut self) -> core::fmt::Result {
        for _core in 0..self.tab_level {
            self.write_str("    ")?;
        }
        Ok(())
    }
    pub fn nest<'b>(&'b mut self) -> PrettyPrinter<'b, 'fmt, 'ir> {
        PrettyPrinter {
            fmt: self.fmt,
            constants: self.constants,
            tab_level: self.tab_level + 1,
        }
    }

    pub fn constants(&self) -> &'a ConstantPool<'ir> {
        self.constants
    }

    pub(crate) fn new(
        fmt: &'a mut core::fmt::Formatter<'fmt>,
        constants: &'a ConstantPool<'ir>,
    ) -> Self {
        Self {
            fmt,
            constants,
            tab_level: 0,
        }
    }
}
