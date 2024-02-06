use arcstr::ArcStr;

use crate::core::types::Type;

/// A type variable.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct TypeVar {
    name: ArcStr,
}

impl TypeVar {
    /// Creates a new type variable.
    pub(super) fn new(name: impl Into<ArcStr>) -> Self { Self { name: name.into() } }

    /// Returns the name of the type variable.
    pub fn name(&self) -> &ArcStr { &self.name }
}

/// A compound type.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Compound {
    op: ArcStr,
    args: Box<[Type]>,
}

impl Compound {
    /// Creates a new constant type (a compound type with no arguments).
    pub(super) fn constant(name: ArcStr) -> Self {
        let args = Box::new([]);
        Self { op: name, args }
    }

    /// Creates a new compound type.
    pub(super) fn new(op: ArcStr, args: impl Into<Box<[Type]>>) -> Self {
        let args = args.into();
        Self { op, args }
    }

    /// Returns the operator of the compound type.
    pub fn op(&self) -> &ArcStr { &self.op }

    /// Returns the arguments of the compound type.
    pub fn args(&self) -> &[Type] { &self.args }
}

/// A function type.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Function {
    arg: Type,
    ret: Type,
}

impl Function {
    /// Creates a new function type.
    pub(super) fn new(arg: Type, ret: Type) -> Self { Self { arg, ret } }

    /// Returns the argument type of the function type.
    pub fn arg(&self) -> &Type { &self.arg }

    /// Returns the return type of the function type.
    pub fn ret(&self) -> &Type { &self.ret }
}
