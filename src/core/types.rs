use std::sync::Arc;

use arcstr::ArcStr;
use eyre::{ensure, Result};

/// A type variable.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct TypeVar(pub ArcStr);

/// A compound type operator.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct CompoundOp(pub ArcStr);

/// A compound type.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Compound {
    pub op: CompoundOp,
    pub args: Box<[Type]>,
}

/// A function type.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Function {
    pub arg: Type,
    pub ret: Type,
}

/// A type.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Type(pub Arc<TypeImpl>);

/// Implementation of [`Type`].
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum TypeImpl {
    TypeVar(TypeVar),
    Constant(CompoundOp),
    Compound(Compound),
    Function(Function),
}

impl Type {
    /// Creates a new type variable.
    pub fn new_var(name: impl Into<ArcStr>) -> Self {
        Self(Arc::new(TypeImpl::TypeVar(TypeVar(name.into()))))
    }

    /// Creates a new constant type (a compound type with no arguments).
    pub fn new_constant(name: impl Into<CompoundOp>) -> Self {
        Self(Arc::new(TypeImpl::Constant(name.into())))
    }

    /// Creates a new compound type.
    pub fn new_compound(
        op: impl Into<CompoundOp>,
        args: impl IntoIterator<Item = impl Into<Type>>,
    ) -> Result<Self> {
        let op = op.into();
        let args: Box<[Type]> = args.into_iter().map(Into::into).collect();
        ensure!(
            !args.is_empty(),
            "compound type must have at least one argument"
        );
        Ok(Self(Arc::new(TypeImpl::Compound(Compound { op, args }))))
    }

    /// Creates a new function type.
    pub fn new_function(arg: impl Into<Type>, ret: impl Into<Type>) -> Self {
        Self(Arc::new(TypeImpl::Function(Function {
            arg: arg.into(),
            ret: ret.into(),
        })))
    }

    /// Returns the type variable name, if this is a type variable.
    pub fn as_var(&self) -> Option<&TypeVar> {
        match &*self.0 {
            TypeImpl::TypeVar(var) => Some(var),
            _ => None,
        }
    }

    /// Returns the constant type name, if this is a constant type.
    pub fn as_constant(&self) -> Option<&CompoundOp> {
        match &*self.0 {
            TypeImpl::Constant(op) => Some(op),
            _ => None,
        }
    }

    /// Returns the compound type operator and arguments, if this is a compound type.
    pub fn as_compound(&self) -> Option<&Compound> {
        match &*self.0 {
            TypeImpl::Compound(compound) => Some(compound),
            _ => None,
        }
    }

    /// Returns the function type argument and return type, if this is a function type.
    pub fn as_function(&self) -> Option<&Function> {
        match &*self.0 {
            TypeImpl::Function(function) => Some(function),
            _ => None,
        }
    }
}
