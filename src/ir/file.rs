use std::{cmp::Ordering, marker::PhantomData};

use lxca_derive::DebugWithConstants;

use crate::{
    binfmt::dec::Decode,
    delegate_to_debug,
    fmt_helpers::WithConstants,
    ir::{
        decls::{DeclBuilder, Declaration},
        metadata::{Metadata, MetadataBuilder},
        types::{Signature, SignatureBuilder, TypeBuilder},
    },
};

use super::{
    PhantomIrMarker,
    constant::{BorrowConstant, Constant, ConstantPool, Internalizable},
    metadata::MetadataList,
    types::Type,
};

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version {
    pub major: u16,
    pub minor: u16,
}

impl core::fmt::Debug for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}.{}", self.major, self.minor))
    }
}

impl core::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}.{}", self.major, self.minor))
    }
}

impl<C> Decode<C> for Version {
    fn decode<R: crate::binfmt::dec::Reader>(
        decoder: &mut crate::binfmt::dec::Decoder<R, C>,
    ) -> crate::binfmt::dec::Result<Self> {
        let [major, minor] = u16::decode_array(decoder)?;

        Ok(Version { major, minor })
    }
}

impl Version {
    pub const CURRENT: Version = Version { major: 0, minor: 0 };

    pub const fn compatible_with(&self, expected: &Version) -> bool {
        self.major == expected.major && expected.minor <= self.minor
    }

    pub const fn supported(&self) -> bool {
        Self::CURRENT.compatible_with(self)
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Magic;

const MAGIC: [u8; 6] = *b"\xFFLXCA\xBC";

impl<C> Decode<C> for Magic {
    fn decode<R: crate::binfmt::dec::Reader>(
        decoder: &mut crate::binfmt::dec::Decoder<R, C>,
    ) -> crate::binfmt::dec::Result<Self> {
        let arr = <[u8; 6]>::decode(decoder)?;

        if arr != MAGIC {
            Err(crate::binfmt::dec::Error::FormatError("Bad magic"))
        } else {
            Ok(Magic)
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OrderMarker;

impl<C> Decode<C> for OrderMarker {
    fn decode<R: crate::binfmt::dec::Reader>(
        decoder: &mut crate::binfmt::dec::Decoder<R, C>,
    ) -> crate::binfmt::dec::Result<Self> {
        let a: [u8; 2] = u8::decode_array(decoder)?;

        match a {
            [0xAA, 0xBB] => decoder.config_mut().order = crate::binfmt::ByteOrder::Big,
            [0xBB, 0xAA] => decoder.config_mut().order = crate::binfmt::ByteOrder::Little,
            _ => {
                return Err(crate::binfmt::dec::Error::FormatError(
                    "Bad Byte Order Marker",
                ));
            }
        }
        Ok(OrderMarker)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FileMetadata {
    pub magic: Magic,
    pub order: OrderMarker,
    pub version: Version,
}

delegate_to_debug!(Version, FileMetadata);

#[derive(DebugWithConstants)]
pub struct File<'ir> {
    _marker: PhantomIrMarker<'ir>,
    metadata: FileMetadata,
    constant_pool: ConstantPool<'ir>,
    source_metadata: SourceMetadata<'ir>,
    decls: Vec<Declaration<'ir>>,
}

impl<'ir> core::fmt::Debug for File<'ir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        WithConstants(self, &self.constant_pool).fmt(f)
    }
}

impl<'ir> File<'ir> {
    pub fn pool(&self) -> &ConstantPool<'ir> {
        &self.constant_pool
    }
}

#[derive(DebugWithConstants)]
pub struct SourceMetadata<'ir> {
    _marker: PhantomIrMarker<'ir>,
    target: Constant<'ir, str>,
    metadata_items: MetadataList<'ir>,
}

pub struct FileBuilder<'ir> {
    constant_pool: Option<ConstantPool<'ir>>,
    metadata: Vec<Metadata<'ir>>,
    decls: Vec<Declaration<'ir>>,
}

impl<'ir> FileBuilder<'ir> {
    pub(crate) fn new() -> Self {
        Self {
            constant_pool: Some(unsafe { ConstantPool::new() }),
            metadata: Vec::new(),
            decls: Vec::new(),
        }
    }

    pub fn constant_pool(&mut self) -> &mut ConstantPool<'ir> {
        self.constant_pool
            .as_mut()
            .expect("A finished FileBuilder cannot be used")
    }

    pub fn build_constant<
        C: BorrowConstant<'ir> + ?Sized,
        R: Internalizable<'ir, C>,
        F: for<'b> FnOnce(&mut ConstantBuilder<'ir, 'b>) -> R,
    >(
        &mut self,
        f: F,
    ) -> Constant<'ir, C> {
        let val = f(&mut ConstantBuilder::new(self.constant_pool()));

        self.constant_pool().intern(val)
    }

    pub fn declare<F: for<'b> FnOnce(&mut DeclBuilder<'ir, 'b>) -> Declaration<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let decl = f(&mut DeclBuilder::new(self.constant_pool()));
        self.decls.push(decl);
        self
    }

    pub fn with_metadata<F: for<'b> FnOnce(&mut MetadataBuilder<'ir, 'b>) -> Metadata<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let meta = f(&mut MetadataBuilder::new(self.constant_pool()));
        self.metadata.push(meta);
        self
    }

    pub fn finish<T: Internalizable<'ir, str>>(&mut self, targ: T) -> File<'ir> {
        let mut constant_pool = self
            .constant_pool
            .take()
            .expect("A finished FileBuilder cannot be used");
        let target = constant_pool.intern(targ);
        File {
            _marker: PhantomIrMarker(PhantomData),
            metadata: FileMetadata {
                magic: Magic,
                order: OrderMarker,
                version: Version::CURRENT,
            },
            constant_pool,
            source_metadata: SourceMetadata {
                _marker: PhantomIrMarker(PhantomData),
                target,
                metadata_items: MetadataList(core::mem::take(&mut self.metadata)),
            },
            decls: core::mem::take(&mut self.decls),
        }
    }
}

pub struct ConstantBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
}

impl<'ir, 'a> ConstantBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>) -> Self {
        Self { pool }
    }

    pub fn type_with<
        R: Internalizable<'ir, Type<'ir>>,
        T: for<'b> FnOnce(&mut TypeBuilder<'ir, 'b>) -> R,
    >(
        &mut self,
        f: T,
    ) -> R {
        f(&mut TypeBuilder::new(self.pool))
    }

    pub fn signature_with<
        R: Internalizable<'ir, Signature<'ir>>,
        S: for<'b> FnOnce(&mut SignatureBuilder<'ir, 'b>) -> R,
    >(
        &mut self,
        f: S,
    ) -> R {
        f(&mut SignatureBuilder::new(self.pool))
    }
}
