use std::{collections::HashMap, sync::Arc};

use arcstr::ArcStr;

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
    pub args: Vec<Type>,
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
        Self(Arc::new(TypeImpl::Compound(Compound {
            op: name.into(),
            args: vec![],
        })))
    }

    /// Creates a new compound type.
    pub fn new_compound(
        op: impl Into<CompoundOp>,
        args: impl IntoIterator<Item = impl Into<Type>>,
    ) -> Self {
        let op = op.into();
        let args = args.into_iter().map(Into::into).collect();
        Self(Arc::new(TypeImpl::Compound(Compound { op, args })))
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

    /// Instantiates this type with the given type substitution.
    pub fn instantiate(&self, subst: &HashMap<TypeVar, Type>) -> Self {
        match &*self.0 {
            TypeImpl::TypeVar(var) => subst.get(var).cloned().unwrap_or_else(|| self.clone()),
            TypeImpl::Compound(compound) => Self::new_compound(
                compound.op.clone(),
                compound.args.iter().map(|arg| arg.instantiate(subst)),
            ),
            TypeImpl::Function(function) => Self::new_function(
                function.arg.instantiate(subst),
                function.ret.instantiate(subst),
            ),
        }
    }
}
