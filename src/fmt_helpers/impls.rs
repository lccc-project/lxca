use std::{fmt::Debug, num::NonZero};

use super::*;

macro_rules! impl_smart_ptr{
    (for<$base_name:ident => $r_type:ty $(, $aux_names:ident)* $(,)?> $ty:ty) => {
        impl<'ir, $base_name: DebugWithConstants<'ir>, $($aux_names),*> DebugWithConstants<'ir> for $ty {
            fn fmt(&self, __f:&mut core::fmt::Formatter, __pool: &ConstantPool<'ir>) -> core::fmt::Result {
                <$r_type as DebugWithConstants<'ir>>::fmt(self, __f, __pool)
            }
        }
    };
    (for<$base_name:ident $(: ?$sized:ident)? $(, $aux_names:ident)* $(,)?> $ty:ty) => {
        impl<'ir, $base_name: DebugWithConstants<'ir> $(+ ?$sized)?, $($aux_names),*> DebugWithConstants<'ir> for $ty {
            fn fmt(&self, __f:&mut core::fmt::Formatter, __pool: &ConstantPool<'ir>) -> core::fmt::Result {
                <$base_name as DebugWithConstants<'ir>>::fmt(self, __f, __pool)
            }
        }
    };
}

impl<'ir, T: DebugWithConstants<'ir>> DebugWithConstants<'ir> for [T] {
    fn fmt(&self, f: &mut core::fmt::Formatter, pool: &ConstantPool<'ir>) -> core::fmt::Result {
        f.debug_list()
            .entries(self.iter().map(|e| WithConstants(e, pool)))
            .finish()
    }
}

impl<'ir, T: DebugWithConstants<'ir>, const N: usize> DebugWithConstants<'ir> for [T; N] {
    fn fmt(&self, f: &mut core::fmt::Formatter, pool: &ConstantPool<'ir>) -> core::fmt::Result {
        f.debug_list()
            .entries(self.iter().map(|e| WithConstants(e, pool)))
            .finish()
    }
}

impl_smart_ptr!(for<T: ?Sized> &T);
impl_smart_ptr!(for<T: ?Sized> &mut T);
impl_smart_ptr!(for<T: ?Sized> Box<T>);
impl_smart_ptr!(for<T: ?Sized> std::rc::Rc<T>);
impl_smart_ptr!(for<T: ?Sized> std::sync::Arc<T>);
impl_smart_ptr!(for<T => [T]> Vec<T>);

macro_rules! impl_tuple {
    ($($name:ident),*) => {
        impl<'ir, $($name: DebugWithConstants<'ir>),*> DebugWithConstants<'ir> for ($($name,)*) {
            #[allow(non_snake_case)]
            fn fmt(&self, f: &mut core::fmt::Formatter, #[allow(unused_variables)] pool: &ConstantPool<'ir>) -> core::fmt::Result {
                let ($($name,)*) = self;

                f.debug_tuple("")
                    $(.field(&WithConstants($name, pool)))*
                    .finish()
            }
        }
    }
}

impl_tuple!();
impl_tuple!(A);
impl_tuple!(A, B);
impl_tuple!(A, B, C);
impl_tuple!(A, B, C, D);
impl_tuple!(A, B, C, D, E);
impl_tuple!(A, B, C, D, E, F);
impl_tuple!(A, B, C, D, E, F, G);
impl_tuple!(A, B, C, D, E, F, G, H);
impl_tuple!(A, B, C, D, E, F, G, H, I);
impl_tuple!(A, B, C, D, E, F, G, H, I, J);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M);

#[macro_export]
macro_rules! delegate_to_debug {
    (for<$($name:ident),*> $ty:ty) => {
        impl<'ir, $($name),*> $crate::fmt_helpers::DebugWithConstants<'ir> for $ty where $ty: ::core::fmt::Debug {
            fn fmt(&self, __f: &mut ::core::fmt::Formatter, __pool: &$crate::ir::constant::ConstantPool) -> ::core::fmt::Result {
                <$ty as ::core::fmt::Debug>::fmt(self, __f)
            }
        }
    };
    ($(for<$($name:ident),*> $ty:ty),+) => {
        $($crate::delegate_to_debug!(for<$($name),*> $ty);)*
    };
    ($ty:ty) => {
        impl<'ir> $crate::fmt_helpers::DebugWithConstants<'ir> for $ty {
            fn fmt(&self, __f: &mut core::fmt::Formatter, __pool: &$crate::ir::constant::ConstantPool) -> core::fmt::Result {
                <$ty as core::fmt::Debug>::fmt(self, __f)
            }
        }
    };
    ($($ty:ty),+) => {
        $($crate::delegate_to_debug!($ty);)*
    };
}

delegate_to_debug!(for<T> *const T, for<T> *mut T, for<T> PhantomData<T>);
delegate_to_debug!(
    u128, u64, u32, u16, u8, i128, i64, i32, i16, i8, usize, isize, f32, f64, bool
);
delegate_to_debug!(Uuid, str);
use indexmap::{IndexMap, IndexSet};
use uuid::Uuid;

impl<'ir, K: DebugWithConstants<'ir>, V: DebugWithConstants<'ir>, S> DebugWithConstants<'ir>
    for IndexMap<K, V, S>
{
    fn fmt(&self, f: &mut core::fmt::Formatter, pool: &ConstantPool<'ir>) -> core::fmt::Result {
        f.debug_map()
            .entries(
                self.iter()
                    .map(|(k, v)| (WithConstants(k, pool), WithConstants(v, pool))),
            )
            .finish()
    }
}

impl<'ir, E: DebugWithConstants<'ir>, S> DebugWithConstants<'ir> for IndexSet<E, S> {
    fn fmt(&self, f: &mut core::fmt::Formatter, pool: &ConstantPool<'ir>) -> core::fmt::Result {
        f.debug_set()
            .entries(self.iter().map(|e| WithConstants(e, pool)))
            .finish()
    }
}

impl<'ir, T: DebugWithConstants<'ir>> DebugWithConstants<'ir> for Option<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter, pool: &ConstantPool<'ir>) -> core::fmt::Result {
        match self {
            None => f.write_str("None"),
            Some(v) => f
                .debug_tuple("Some")
                .field(&WithConstants(v, pool))
                .finish(),
        }
    }
}

delegate_to_debug!(
    NonZero<u128>,
    NonZero<u64>,
    NonZero<u32>,
    NonZero<u16>,
    NonZero<u8>,
    NonZero<i128>,
    NonZero<i64>,
    NonZero<i32>,
    NonZero<i16>,
    NonZero<i8>,
    NonZero<usize>,
    NonZero<isize>
);
