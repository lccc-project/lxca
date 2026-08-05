use lxca_derive::DebugWithConstants;

use crate::{delegate_to_debug, ir::{constant::{Constant, ConstantPool, Internalizable}, decls::DeclScopeId, expr::{AccessClass, Value, ValueBuilder}, metadata::{Metadata, MetadataBuilder, MetadataIter, MetadataList, NestedMetadata}, pretty::{self, delegate_to_display, pretty_print_list}, symbol::{LabelSym, Symbol}}};


#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[bitfield_struct::bitenum]
#[repr(u32)]
pub enum MemoryAccessMode {
    NoAccess,
    ReadOnly,
    #[fallback]
    ReadWrite,
}

impl core::fmt::Display for MemoryAccessMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryAccessMode::NoAccess => f.write_str("none"),
            MemoryAccessMode::ReadOnly => f.write_str("readonly"),
            MemoryAccessMode::ReadWrite => f.write_str("readwrite"),
        }
    }
}


#[bitfield_struct::bitfield(u32, hash = true)]
#[derive(PartialEq, Eq)]
pub struct AssemblyFlags {
    #[bits(2)]
    pub global_memory_mode: MemoryAccessMode,
    #[bits(2)]
    pub operand_memory_mode: MemoryAccessMode,
    pub deterministic: bool,
    pub volatile: bool,
    pub transparent: bool,
    pub do_not_substitute: bool,
    #[bits(8)]
    __pad: u8,
    #[bits(16)]
    pub synchronize_access_class: AccessClass,

}

impl core::fmt::Display for AssemblyFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        
        f.write_str("globals ")?;
        self.global_memory_mode().fmt(f)?;
        f.write_str(" operands ")?;
        self.operand_memory_mode().fmt(f)?;
        
        if self.deterministic() {
            f.write_str(" deterministic")?;
        }

        if self.volatile() {
            f.write_str(" volatile")?;
        }

        if self.transparent() {
            f.write_str(" transparent")?;
        }

        if self.do_not_substitute() {
            f.write_str(" rawasm")?;
        }

        let cl = self.synchronize_access_class();
        if cl.atomic() {
            f.write_str(" synchronize")?;
            cl.fmt(f)?;
        }

        Ok(())
    }
}

delegate_to_debug!(AssemblyFlags);
delegate_to_display!(AssemblyFlags);

#[derive(Clone, Hash, PartialEq, Eq, DebugWithConstants)]
pub struct InlineAssembly<'ir> {
    metadata: MetadataList<'ir>,
    subst_string: Constant<'ir, str>,
    syntax_form: Option<Constant<'ir, Symbol>>,
    global_flags: AssemblyFlags,
    const_operands: Vec<AsmConstOperand<'ir>>,
}

impl<'ir> pretty::PrettyPrint<'ir> for InlineAssembly<'ir> {
    fn fmt(&self, f: &mut pretty::PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
        self.metadata.fmt(f)?;
        f.write_str("asm ")?;
        if let Some(syntax) = self.syntax_form {
            syntax.fmt(f)?;
            f.write_str(" ")?;
        }
        self.global_flags.fmt(f)?;

        f.write_str("\n")?;

        let st = self.subst_string.get(f.constants()).to_string();

        let mut pretty = f.nest();

        
        for r in st.split('\n').map(|r| r.trim_ascii()) {
            use core::fmt::Display as _;
            pretty.write_tabs()?;
            pretty.write_str("\"")?;
            r.escape_default().fmt(&mut pretty)?;
            pretty.write_str("\"\n")?;
        }

        f.write_str("consts [")?;

        pretty_print_list(&self.const_operands, f, ", ")?;
        f.write_str("]\n")
    }
}

impl<'ir> NestedMetadata<'ir> for InlineAssembly<'ir> {
    fn list_metadata(&self) -> &MetadataList<'ir> {
        &self.metadata
    }

    fn next<'a>(&'a self, _: &'a ConstantPool<'ir>) -> Option<&'a Self> {
        None
    }
}

impl<'ir> InlineAssembly<'ir> {
    pub fn global_flags(&self) -> AssemblyFlags {
        self.global_flags
    }

    pub fn const_operands(&self) -> &[AsmConstOperand<'ir>] {
        &self.const_operands
    }

    pub fn asm_string(&self) -> Constant<'ir, str> {
        self.subst_string
    }

    pub fn syntax_form(&self) -> Option<Constant<'ir, Symbol>> {
        self.syntax_form
    }

    pub fn metadata<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> MetadataIter<'ir, 'a, Self> {
        MetadataIter::new(self, pool)
    }
}

#[derive(Clone, Hash, PartialEq, Eq, DebugWithConstants)]
#[non_exhaustive]
pub enum AsmConstOperandInner<'ir> {
    Symbol(Constant<'ir, Symbol>),
    Constant(Value<'ir>),
    Label(Constant<'ir, LabelSym>, DeclScopeId<'ir>),
}

#[derive(Clone, Hash, PartialEq, Eq, DebugWithConstants)]
pub struct AsmConstOperand<'ir> {
    metadata: MetadataList<'ir>,
    opr: AsmConstOperandInner<'ir>
}

impl<'ir> pretty::PrettyPrint<'ir> for AsmConstOperand<'ir> {
    fn fmt(&self, f: &mut pretty::PrettyPrinter<'_, '_, 'ir>) -> core::fmt::Result {
        self.metadata.fmt(f)?;
        match &self.opr {
            AsmConstOperandInner::Symbol(sym) => {
                f.write_str("symbol ")?;
                sym.fmt(f)
            },
            AsmConstOperandInner::Constant(value) => {
                value.fmt(f)
            },
            AsmConstOperandInner::Label(label, _) => label.fmt(f),
        }
    }
}

impl<'ir> NestedMetadata<'ir> for AsmConstOperand<'ir> {
    fn list_metadata(&self) -> &MetadataList<'ir> {
        &self.metadata
    }

    fn next<'a>(&'a self, _: &'a super::constant::ConstantPool<'ir>) -> Option<&'a Self> {
        None
    }
}

impl<'ir> AsmConstOperand<'ir> {
    pub fn metadata<'a>(&'a self, pool: &'a ConstantPool<'ir>) -> MetadataIter<'ir, 'a, Self> {
        MetadataIter::new(self, pool)
    }

    pub fn body(&self) -> &AsmConstOperandInner<'ir> {
        &self.opr
    }
}


pub struct InlineAssemblyBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    metadata: Vec<Metadata<'ir>>,
    global_flags: AssemblyFlags,
    syntax: Option<Constant<'ir, Symbol>>,
    oprs: Vec<AsmConstOperand<'ir>>,
    scope_id: Option<DeclScopeId<'ir>>,
}

impl<'ir, 'a> InlineAssemblyBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>, scope_id: Option<DeclScopeId<'ir>>) -> Self {
        Self {
            pool,
            metadata: Vec::new(),
            global_flags: AssemblyFlags::new().with_global_memory_mode(MemoryAccessMode::ReadWrite).with_operand_memory_mode(MemoryAccessMode::ReadWrite).with_volatile(true).with_synchronize_access_class(
                    AccessClass::new().with_atomic(true).with_seq_cst(true).with_acquire(true).with_release(true)
                ),
            syntax: None,
            oprs: Vec::new(),
            scope_id
        }
    }

    pub fn with_metadata<F: for<'b> FnOnce(&mut MetadataBuilder<'ir, 'b>) -> Metadata<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let meta = f(&mut MetadataBuilder::new(self.pool, self.scope_id));
        self.metadata.push(meta);
        self
    }

    pub fn with_syntax<I: Internalizable<'ir, Symbol>>(&mut self, syntax: I) -> &mut Self {
        let sym = self.pool.intern(syntax);
        self.syntax = Some(sym);
        self
    }

    pub fn with_global_flags(&mut self, flags: AssemblyFlags) -> &mut Self {
        self.global_flags = flags;
        self
    }

    pub fn with_global_memory_mode(&mut self, mode: MemoryAccessMode) -> &mut Self {
        self.global_flags.set_global_memory_mode(mode);
        self
    }

    pub fn with_operand_memory_mode(&mut self, mode: MemoryAccessMode) -> &mut Self {
        self.global_flags.set_operand_memory_mode(mode);
        self
    }

    pub fn with_access_class(&mut self, access_class: AccessClass) -> &mut Self {
        self.global_flags.set_synchronize_access_class(access_class);
        self
    }

    pub fn with_deterministic(&mut self, deterministic: bool) -> &mut Self {
        self.global_flags.set_deterministic(deterministic);
        self
    }

    pub fn with_volatile(&mut self, volatile: bool) -> &mut Self {
        self.global_flags.set_volatile(volatile);
        self
    }

    pub fn with_do_not_substitute(&mut self, do_not_substitute: bool) -> &mut Self {
        self.global_flags.set_do_not_substitute(do_not_substitute);
        self
    }

    pub fn with_operand<F: for<'b> FnOnce(&mut AsmConstOperandBuilder<'ir, 'b>) -> AsmConstOperand<'ir>>(&mut self, f: F) -> &mut Self {
        let opr = f(&mut AsmConstOperandBuilder::new(self.pool, self.scope_id));
        self.oprs.push(opr);
        self
    }

    pub fn build<S: Internalizable<'ir, str>>(&mut self, subst_str: S) -> InlineAssembly<'ir> {
        let subst = self.pool.intern(subst_str);
        InlineAssembly { metadata: MetadataList(core::mem::take(&mut self.metadata)), subst_string: subst, syntax_form: self.syntax, global_flags: self.global_flags, const_operands: core::mem::take(&mut self.oprs) }
    }
}


pub struct AsmConstOperandBuilder<'ir, 'a> {
    pool: &'a mut ConstantPool<'ir>,
    metadata: Vec<Metadata<'ir>>,
    scope: Option<DeclScopeId<'ir>>,
}

impl<'ir, 'a> AsmConstOperandBuilder<'ir, 'a> {
    pub(crate) fn new(pool: &'a mut ConstantPool<'ir>, scope: Option<DeclScopeId<'ir>>) -> Self {
        Self {pool, metadata: Vec::new(), scope}
    }

    pub fn with_metadata<F: for<'b> FnOnce(&mut MetadataBuilder<'ir, 'b>) -> Metadata<'ir>>(
        &mut self,
        f: F,
    ) -> &mut Self {
        let meta = f(&mut MetadataBuilder::new(self.pool, self.scope));
        self.metadata.push(meta);
        self
    }

    pub fn build(&mut self, body: AsmConstOperandInner<'ir>) -> AsmConstOperand<'ir> {
        AsmConstOperand { metadata: MetadataList(core::mem::take(&mut self.metadata)), opr: body }
    }

    pub fn symbol<S: Internalizable<'ir, Symbol>>(&mut self, sym: S) -> AsmConstOperand<'ir> {
        let sym = self.pool.intern(sym);
        self.build(AsmConstOperandInner::Symbol(sym))
    }

    pub fn value_with<F: for<'b> FnOnce(&mut ValueBuilder<'ir, 'b>) -> Value<'ir>>(&mut self, f: F) -> AsmConstOperand<'ir> {
        let val = f(&mut ValueBuilder::new(self.pool, self.scope));
        self.build(AsmConstOperandInner::Constant(val))
    }

    pub fn label<S: Internalizable<'ir, LabelSym>>(&mut self, sym: S) -> AsmConstOperand<'ir> {
        let scope = self.scope.expect("Cannot reference a label at global scope");
        let sym = self.pool.intern(sym);
        self.build(AsmConstOperandInner::Label(sym, scope))
    }
}