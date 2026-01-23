use std::{
    borrow::{Borrow, Cow},
    hash::{BuildHasher, Hash, Hasher, RandomState},
    marker::PhantomData,
    num::{NonZero, NonZeroUsize},
};

use hashbrown::HashTable;
use uuid::Uuid;

use crate::{
    binfmt::dec::Decode,
    delegate_to_debug,
    fmt_helpers::DebugWithConstants,
    ir::{expr::Expr, types::Signature},
};

use super::{
    HasIr, PhantomIrMarker, expr::Value, metadata::MetadataList, symbol::SymbolDef, types::Type,
};

pub trait ConstantAs<'a, 'ir>: BorrowConstant<'ir> {
    fn read_as(val: &'a Self::Constant) -> Self;
}

impl<'a, 'ir> ConstantAs<'a, 'ir> for u128 {
    fn read_as(val: &'a Self::Constant) -> Self {
        *val
    }
}

impl<'a, 'ir> ConstantAs<'a, 'ir> for &'a str {
    fn read_as(val: &'a Self::Constant) -> Self {
        val
    }
}

impl<'a, 'ir, T: ConstantTy<'ir>> ConstantAs<'a, 'ir> for &'a T {
    fn read_as(val: &'a Self::Constant) -> Self {
        val
    }
}

impl<'a, 'ir, T> ConstantAs<'a, 'ir> for &'a [T]
where
    [T]: BorrowConstant<'ir>,
    Self::Constant: Borrow<[T]>,
{
    fn read_as(val: &'a Self::Constant) -> Self {
        val.borrow()
    }
}

pub struct Constant<'ir, T: ?Sized>(NonZeroUsize, PhantomData<&'ir T>, PhantomIrMarker<'ir>);

impl<'ir, T: ?Sized> Copy for Constant<'ir, T> {}
impl<'ir, T: ?Sized> Clone for Constant<'ir, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'ir, T: ?Sized + PartialEq<U>, U: ?Sized> PartialEq<Constant<'ir, U>> for Constant<'ir, T> {
    fn eq(&self, other: &Constant<'ir, U>) -> bool {
        self.0 == other.0
    }
}

impl<'ir, T: ?Sized + Eq> Eq for Constant<'ir, T> {}

impl<'ir, T: ?Sized> core::hash::Hash for Constant<'ir, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<'ir, T: ?Sized + BorrowConstant<'ir> + DebugWithConstants<'ir>> DebugWithConstants<'ir>
    for Constant<'ir, T>
where
    for<'a> &'a T: ConstantAs<'a, 'ir, Constant = T::Constant>,
{
    fn fmt(&self, f: &mut core::fmt::Formatter, pool: &ConstantPool<'ir>) -> core::fmt::Result {
        f.write_fmt(format_args!("${}(", self.0))?;
        let val = self.get(pool);
        <T as DebugWithConstants<'ir>>::fmt(val, f, pool)?;
        f.write_str(")")
    }
}

impl<'ir, T: ?Sized> Constant<'ir, T> {
    pub const fn borrow<R: ?Sized>(&self) -> Constant<'ir, R>
    where
        T: Borrow<R>,
    {
        Constant(self.0, PhantomData, self.2)
    }
}

impl<'ir, T: BorrowConstant<'ir> + ?Sized> Constant<'ir, T> {
    fn raw_get<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> &'a T::Constant {
        // SAFETY: the `'ir` bound guarantees this is in bounds of the pool, because it belongs to `ir`
        let ctx = unsafe { pool.inner.get(self.0) };

        // SAFETY: the `'ir` bound, plus the invariants of `Constant`
        unsafe { <T::Constant as ConstantTy>::borrow_entry_unchecked(ctx) }
    }

    pub fn read<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> T
    where
        T: ConstantAs<'a, 'ir> + Sized,
    {
        let constant = self.raw_get(pool);

        T::read_as(constant)
    }

    // I've managed to confuse r-a
    pub fn get<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> &'a T
    where
        &'a T: ConstantAs<'a, 'ir, Constant = T::Constant>,
    {
        let constant = self.raw_get(pool);

        <&'a T as ConstantAs<'a, 'ir>>::read_as(constant)
    }
}

impl<'ir, T: BorrowConstant<'ir> + ?Sized> Decode<&ConstantPool<'ir>> for Constant<'ir, T> {
    fn decode<R: crate::binfmt::dec::Reader>(
        decoder: &mut crate::binfmt::dec::Decoder<R, &ConstantPool<'ir>>,
    ) -> crate::binfmt::dec::Result<Self> {
        let n = NonZeroUsize::decode(decoder)?;
        let ctx = decoder.context();
        ctx.try_bind(n).map_err(|e| match e {
            TryBindError::NotInMap(n) => {
                crate::binfmt::dec::Error::OutOfRangeUnsigned(n.get() as u128)
            }
            TryBindError::WrongType(_, ty) => crate::binfmt::dec::Error::BadType(ty),
        })
    }
}

impl<'a, 'ir, T: BorrowConstant<'ir> + ?Sized> Decode<UnsafeCpInit<'a, 'ir>> for Constant<'ir, T> {
    fn decode<R: crate::binfmt::dec::Reader>(
        decoder: &mut crate::binfmt::dec::Decoder<R, UnsafeCpInit<'a, 'ir>>,
    ) -> crate::binfmt::dec::Result<Self> {
        let n = NonZeroUsize::decode(decoder)?;
        let ctx = decoder.context();

        let idx = n.get() - 1;
        let Some(ent) = ctx.0.get(idx) else {
            return Err(crate::binfmt::dec::Error::OutOfRangeUnsigned(
                n.get() as u128
            ));
        };

        if ent.discrim() != <T::Constant as ConstantTy<'ir>>::DISCRIM {
            return Err(crate::binfmt::dec::Error::BadType(ent.name()));
        }

        Ok(Self(n, PhantomData, PhantomIrMarker(PhantomData)))
    }
}

pub(crate) struct UnsafeCpInit<'a, 'ir>(&'a Vec<ConstantPoolEntry<'ir>>);

macro_rules! def_constant_pool_enum {
    {
        $vis:vis enum $name:ident <$life:lifetime> {
            $($variant:ident ($ty:ty)),*
            $(,)?
        }
    } => {
        #[derive(Clone, PartialEq, Eq, lxca_derive::DebugWithConstants)]
        #[non_exhaustive]
        $vis enum $name <$life> {
            $($variant ($ty),)*

            #[doc(hidden)]
            __NonExhaustive(PhantomIrMarker<$life>)
        }

        impl<'ir> core::hash::Hash for $name <'ir> {
            fn hash<S: core::hash::Hasher>(&self, state: &mut S) {
                match self {
                    $($name :: $variant (n) => <$ty as BorrowConstant<'ir>>::hash(n, state),)*
                    Self::__NonExhaustive(_) => {}
                }
            }
        }

        impl<'ir> $name <'ir> {

            fn discrim(&self) -> usize {
                match self {
                    $($name :: $variant (_) => ${index()},)*
                    Self::__NonExhaustive(_) => !0,
                }
            }

            fn name(&self) -> &'static str {
                match self {
                    $($name :: $variant (_) => ::core::stringify!($variant),)*
                    Self::__NonExhaustive(_) => "!!UNKNOWN!!",
                }
            }

            fn eq_borrowed<R: BorrowConstant<'ir>+ ?Sized>(&self, other: &R) -> bool where R::Constant: PartialEq<R> {
                match self {
                    Self::__NonExhaustive(_) => false,
                    $($name :: $variant(n) => {
                        if const { ${index()} == <R::Constant as ConstantTy>::DISCRIM } {
                            // SAFETY:
                            // This is a type identity transmute to inform the type system of the known equivalence
                            let borrowed: &R::Constant = unsafe { core::mem::transmute(n) };

                            *borrowed == *other
                        } else {
                            false
                        }
                    }),*
                }
            }


        }

        $(
            impl<$life> PartialEq<$ty> for $name <$life> {
                fn eq(&self, other: &$ty) -> bool {
                    match self {
                        $name :: $variant (n) => n == other,
                        _ => false
                    }
                }
            }
            unsafe impl<$life> ConstantTy<$life> for $ty {
                const DISCRIM: usize = ${index()};

                fn into_entry(self) -> $name <$life> {
                    $name :: $variant (self)
                }

                unsafe fn borrow_entry_unchecked<'__a>(entry: &'__a $name <$life>) -> &'__a Self {
                    match entry {
                        $name :: $variant (val) => val,
                        _ => unsafe { core::hint::unreachable_unchecked() }
                    }
                }
            }

            impl<$life> BorrowConstant<$life> for $ty {
                type Constant = Self;
            }

            impl<$life> From<$ty> for ConstantPoolEntry<$life> {
                fn from(value: $ty) -> Self {
                    value.into_entry()
                }
            }

            impl<$life> From<Box<$ty>> for ConstantPoolEntry<$life> {
                fn from(value: Box<$ty>) -> Self {
                    (*value).into_entry()
                }
            }
        )*
    }
}

macro_rules! impl_from_integer_types {
    ($($int_ty:ty),* $(,)?) => {
        $(

            impl<'ir> DebugWithConstants<'ir> for Constant<'ir, $int_ty> {
                fn fmt(&self, f: &mut core::fmt::Formatter, pool: &ConstantPool<'ir>) -> core::fmt::Result {
                    let val = self.read(pool);

                    <$int_ty as core::fmt::Debug>::fmt(&val, f)
                }
            }
            impl<'ir> From<$int_ty> for ConstantPoolEntry<'ir> {
                fn from(value: $int_ty) -> ConstantPoolEntry<'ir> {
                    ConstantPoolEntry::Integer(value as u128)
                }
            }

            impl<'a, 'ir> ConstantAs<'a, 'ir> for $int_ty {
                fn read_as(this: &u128) -> Self {
                    (*this) as $int_ty
                }
            }

            impl<'ir> BorrowConstant<'ir> for $int_ty {
                type Constant = u128;

                fn hash<S: core::hash::Hasher>(&self, hasher: &mut S)
                where
                    Self: core::hash::Hash,
                {
                    <str as core::hash::Hash>::hash("CPE", hasher);
                    hasher.write_usize(<Self::Constant as ConstantTy>::DISCRIM);
                    <u128 as core::hash::Hash>::hash(&(*self as u128), hasher);
                }
            }

            impl<'ir> From<Box<$int_ty>> for ConstantPoolEntry<'ir> {
                fn from(value: Box<$int_ty>) -> Self {
                    ConstantPoolEntry::Integer(*value as u128)
                }
            }
        )*
    }
}

impl_from_integer_types!(u8, i8, u16, i16, u32, i32, u64, i64, i128);

pub trait BorrowConstant<'ir> {
    type Constant: ConstantTy<'ir> + 'ir;
    /// Convience Method that hashes both the value and the discriminant of the constant pool entry
    ///
    /// Must not be overriden (else may result in arbitrary misbehaviour, but not undefined behaviour)
    ///
    /// [`Self::hash`][core::hash::Hash::hash] must also be the same as [`Self::Constant::hash`][core::hash::Hash::hash] for equal values
    fn hash<S: core::hash::Hasher>(&self, hasher: &mut S)
    where
        Self: core::hash::Hash,
    {
        <str as core::hash::Hash>::hash("CPE", hasher);
        hasher.write_usize(<Self::Constant as ConstantTy>::DISCRIM);
        <Self as core::hash::Hash>::hash(self, hasher);
    }
}

impl<'ir, 'a, C: ?Sized + BorrowConstant<'ir>> BorrowConstant<'ir> for &'a C {
    type Constant = C::Constant;
}

impl<'ir, 'a, C: ?Sized + BorrowConstant<'ir> + ToOwned> BorrowConstant<'ir> for Cow<'a, C> {
    type Constant = C::Constant;
}

impl<'ir, C: ?Sized + BorrowConstant<'ir>> BorrowConstant<'ir> for Box<C> {
    type Constant = C::Constant;
}

impl<'ir> BorrowConstant<'ir> for str {
    type Constant = String;
}

impl<'ir> From<&str> for ConstantPoolEntry<'ir> {
    fn from(value: &str) -> Self {
        ConstantPoolEntry::String(value.into())
    }
}

impl<'ir> From<Cow<'_, str>> for ConstantPoolEntry<'ir> {
    fn from(value: Cow<str>) -> Self {
        ConstantPoolEntry::String(value.into())
    }
}

impl<'ir> From<Box<str>> for ConstantPoolEntry<'ir> {
    fn from(value: Box<str>) -> Self {
        ConstantPoolEntry::String(value.into_string())
    }
}

impl<'ir, C: Clone> From<Box<[C]>> for ConstantPoolEntry<'ir>
where
    Vec<C>: ConstantTy<'ir>,
{
    fn from(value: Box<[C]>) -> Self {
        value.to_vec().into_entry()
    }
}

/// # Safety
/// This trait cannot be implemented outside the crate. The full safety constraints that must be upheld by an implementor are not defined.
/// External unsafe code may rely on the following:
/// * [`ConstantTy::into_entry`] and [`ConstantTy::borrow_entry_unchecked`] are consistent.
///  That is [`ConstantTy::into_entry`] returns a [`ConstantPoolEntry`] such that it can be passed to [`ConstantTy::borrow_entry_unchecked`] and satisfy the preconditions.
pub unsafe trait ConstantTy<'ir>:
    BorrowConstant<'ir, Constant = Self> + Into<ConstantPoolEntry<'ir>>
{
    #[doc(hidden)]
    const DISCRIM: usize;

    fn into_entry(self) -> ConstantPoolEntry<'ir>;

    unsafe fn borrow_entry_unchecked<'a>(entry: &'a ConstantPoolEntry<'ir>) -> &'a Self;
}

struct ConstantPoolInner<'ir> {
    constants_array: Vec<ConstantPoolEntry<'ir>>,
    hasher: RandomState,
}

impl<'ir> ConstantPoolInner<'ir> {
    unsafe fn get(&self, n: NonZeroUsize) -> &ConstantPoolEntry<'ir> {
        unsafe { self.constants_array.get_unchecked(n.get() - 1) }
    }

    fn hash_elem<R: BorrowConstant<'ir> + core::hash::Hash + ?Sized>(&self, val: &R) -> u64 {
        let mut hasher = self.hasher.build_hasher();

        <R as BorrowConstant>::hash(val, &mut hasher);

        hasher.finish()
    }

    unsafe fn rehash(&self, n: NonZeroUsize) -> u64 {
        // SAFETY: Internal method, only called with elements in the table
        let ent = unsafe { self.constants_array.get_unchecked(n.get() - 1) };

        let mut hasher = self.hasher.build_hasher();

        <ConstantPoolEntry as core::hash::Hash>::hash(ent, &mut hasher);

        hasher.finish()
    }

    fn len(&self) -> usize {
        self.constants_array.len()
    }

    fn insert_raw(&mut self, val: ConstantPoolEntry<'ir>) -> NonZeroUsize {
        self.constants_array.push(val);
        // SAFETY: This will always be > 0 because the array will always have at least one element
        unsafe { NonZero::new_unchecked(self.len()) }
    }
}

pub struct ConstantPool<'ir> {
    _marker: PhantomIrMarker<'ir>,
    inner: ConstantPoolInner<'ir>,
    tbl: HashTable<NonZeroUsize>,
}

impl<'ir> HasIr<'ir> for ConstantPool<'ir> {
    fn marker(&self) -> PhantomIrMarker<'ir> {
        self._marker
    }
}

impl<'ir> core::fmt::Debug for ConstantPool<'ir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct CpId(usize);
        impl core::fmt::Debug for CpId {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_fmt(format_args!("${}", self.0))
            }
        }
        struct WrapEntry<'a, 'ir>(&'a ConstantPoolEntry<'ir>, &'a ConstantPool<'ir>);

        impl<'a, 'ir> core::fmt::Debug for WrapEntry<'a, 'ir> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                <ConstantPoolEntry<'ir> as DebugWithConstants<'ir>>::fmt(self.0, f, self.1)
            }
        }

        f.debug_map()
            .entries(
                self.inner
                    .constants_array
                    .iter()
                    .enumerate()
                    .map(|(i, e)| (CpId(i + 1), WrapEntry(e, self))),
            )
            .finish()
    }
}

// This is a hack, but it's fine because we're local.
//
delegate_to_debug!(ConstantPool<'ir>);

impl<'ir> ConstantPool<'ir> {
    fn get_entry_index<R: BorrowConstant<'ir> + Eq + core::hash::Hash + ?Sized>(
        &self,
        val: &R,
    ) -> Option<NonZeroUsize>
    where
        R::Constant: PartialEq<R>,
    {
        let hash = self.inner.hash_elem(val);
        self.tbl
            .find(hash, |&v| unsafe { self.inner.get(v) }.eq_borrowed(val))
            .copied()
    }

    fn insert_if_eq<R: Insertable<'ir>>(&mut self, val: R) -> NonZeroUsize {
        match val.get_entry(self) {
            Ok(ent) => ent,
            Err(frag) => {
                let (hash, entry) = R::do_insert(frag);
                let idx = self.inner.insert_raw(entry);

                self.tbl
                    .insert_unique(hash, idx, |k| unsafe { self.inner.rehash(*k) });
                idx
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum TryBindError {
    NotInMap(NonZeroUsize),
    WrongType(NonZeroUsize, &'static str),
}

impl<'ir> ConstantPool<'ir> {
    /// # Safety
    /// This is a root method, and can only be called when you have an ir context
    pub(crate) unsafe fn new() -> Self {
        Self {
            _marker: PhantomIrMarker(PhantomData),
            inner: ConstantPoolInner {
                constants_array: Vec::new(),
                hasher: RandomState::new(),
            },
            tbl: HashTable::new(),
        }
    }

    pub fn intern<C: BorrowConstant<'ir> + ?Sized, V: Internalizable<'ir, C>>(
        &mut self,
        val: V,
    ) -> Constant<'ir, C> {
        let n = self.insert_if_eq(val);

        Constant(n, PhantomData, self._marker)
    }

    pub fn get<V: BorrowConstant<'ir> + core::hash::Hash + ?Sized + Eq>(
        &self,
        val: &V,
    ) -> Option<Constant<'ir, V::Constant>>
    where
        V::Constant: PartialEq<V>,
    {
        self.get_entry_index(val)
            .map(|n| Constant(n, PhantomData, self._marker))
    }

    pub fn try_bind<R: BorrowConstant<'ir> + ?Sized>(
        &self,
        n: NonZeroUsize,
    ) -> Result<Constant<'ir, R>, TryBindError> {
        let raw_idx = n.get() - 1;

        let Some(ent) = self.inner.constants_array.get(raw_idx) else {
            return Err(TryBindError::NotInMap(n));
        };

        if ent.discrim() != <R::Constant as ConstantTy>::DISCRIM {
            return Err(TryBindError::WrongType(n, ent.name()));
        }

        Ok(Constant(n, PhantomData, self._marker))
    }
}

mod private {
    use std::num::NonZero;

    use crate::ir::constant::{
        BorrowConstant, BoxOrConstant, Constant, ConstantPool, ConstantPoolEntry,
    };

    pub unsafe trait Insertable<'ir>: Sized {
        type Frag;
        type Constant: 'ir;
        fn get_entry(self, pool: &ConstantPool<'ir>) -> Result<NonZero<usize>, Self::Frag>;
        fn do_insert(efrag: Self::Frag) -> (u64, ConstantPoolEntry<'ir>);
    }

    unsafe impl<
        'ir,
        I: BorrowConstant<'ir, Constant: PartialEq<Self>>
            + core::hash::Hash
            + Eq
            + Into<ConstantPoolEntry<'ir>>,
    > Insertable<'ir> for I
    {
        type Frag = (u64, Self);
        type Constant = I::Constant;
        fn get_entry(self, pool: &ConstantPool<'ir>) -> Result<NonZero<usize>, Self::Frag> {
            pool.get_entry_index(&self).ok_or_else(move || {
                let hash = pool.inner.hash_elem(&self);
                (hash, self)
            })
        }
        fn do_insert((hash, val): Self::Frag) -> (u64, ConstantPoolEntry<'ir>) {
            (hash, val.into())
        }
    }

    unsafe impl<'ir, C: BorrowConstant<'ir> + ?Sized> Insertable<'ir> for Constant<'ir, C> {
        type Frag = core::convert::Infallible;
        type Constant = C::Constant;
        fn get_entry(self, _: &ConstantPool<'ir>) -> Result<NonZero<usize>, Self::Frag> {
            Ok(self.0)
        }
        fn do_insert(efrag: Self::Frag) -> (u64, ConstantPoolEntry<'ir>) {
            match efrag {}
        }
    }

    unsafe impl<'ir, C: BorrowConstant<'ir>> Insertable<'ir> for BoxOrConstant<'ir, C>
    where
        Box<C>: Insertable<'ir, Frag = (u64, Box<C>), Constant = C::Constant>,
    {
        type Frag = (u64, Box<C>);

        type Constant = C::Constant;

        fn get_entry(self, pool: &ConstantPool<'ir>) -> Result<NonZero<usize>, Self::Frag> {
            match self {
                BoxOrConstant::Interned(constant) => Ok(constant.0),
                BoxOrConstant::Boxed(boxed) => boxed.get_entry(pool),
            }
        }

        fn do_insert(efrag: Self::Frag) -> (u64, ConstantPoolEntry<'ir>) {
            Box::<C>::do_insert(efrag)
        }
    }
}

use private::Insertable;

pub trait Internalizable<'ir, C: BorrowConstant<'ir> + ?Sized>:
    Insertable<'ir, Constant = C::Constant>
{
}

impl<'ir, C: Insertable<'ir, Constant = T::Constant>, T: BorrowConstant<'ir> + ?Sized>
    Internalizable<'ir, T> for C
{
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Void;

delegate_to_debug!(Void);

def_constant_pool_enum! {
    pub enum ConstantPoolEntry <'ir> {
        Void(Void),
        Integer(u128),
        String(String),
        Bytes(Vec<u8>),
        Uuid(Uuid),
        MetadataList(MetadataList<'ir>),
        Symbol(SymbolDef),
        Type(Type<'ir>),
        Value(Value<'ir>),
        Expr(Expr<'ir>),
        Signature(Signature<'ir>)
    }
}

impl<'ir> BorrowConstant<'ir> for [u8] {
    type Constant = Vec<u8>;
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum BoxOrConstant<'ir, T: ?Sized> {
    Interned(Constant<'ir, T>),
    Boxed(Box<T>),
}

impl<'ir, T: ?Sized + DebugWithConstants<'ir>> DebugWithConstants<'ir> for BoxOrConstant<'ir, T>
where
    Constant<'ir, T>: DebugWithConstants<'ir>,
{
    fn fmt(&self, f: &mut core::fmt::Formatter, pool: &ConstantPool<'ir>) -> core::fmt::Result {
        match self {
            Self::Boxed(b) => b.fmt(f, pool),
            Self::Interned(c) => c.fmt(f, pool),
        }
    }
}
