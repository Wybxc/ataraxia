use std::{cell::OnceCell, rc::Rc};

use arcstr::ArcStr;

mod variants;

pub use variants::*;

/// A type.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Type(pub Rc<TypeImpl>);

/// Implementation of [`Type`].
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum TypeImpl {
    TypeVar(TypeVar),
    Compound(Compound),
    Function(Function),
    Bool,
}

impl Type {
    /// Creates a new type variable.
    pub fn var(name: impl Into<ArcStr>) -> Self {
        Self(Rc::new(TypeImpl::TypeVar(TypeVar::new(name))))
    }

    /// Creates a new constant type (a compound type with no arguments).
    pub fn constant(name: ArcStr) -> Self {
        Self(Rc::new(TypeImpl::Compound(Compound::constant(name))))
    }

    /// Creates a new compound type.
    pub fn compound(op: ArcStr, args: impl Into<Box<[Type]>>) -> Self {
        Self(Rc::new(TypeImpl::Compound(Compound::new(op, args))))
    }

    /// Creates a new function type.
    pub fn function(arg: Type, ret: Type) -> Self {
        Self(Rc::new(TypeImpl::Function(Function::new(arg, ret))))
    }

    /// Creates a new boolean type.
    pub fn bool() -> Self {
        thread_local! {
            static BOOL: OnceCell<Type> = OnceCell::new();
        }
        BOOL.with(|bool| bool.get_or_init(|| Self(Rc::new(TypeImpl::Bool))).clone())
    }

    /// Checks if this is a type variable.
    pub fn is_var(&self) -> bool { matches!(&*self.0, TypeImpl::TypeVar(_)) }

    /// Checks if this is a compound type.
    pub fn is_compound(&self) -> bool { matches!(&*self.0, TypeImpl::Compound(_)) }

    /// Checks if this is a function type.
    pub fn is_function(&self) -> bool { matches!(&*self.0, TypeImpl::Function(_)) }

    /// Checks if this is a boolean type.
    pub fn is_bool(&self) -> bool { matches!(&*self.0, TypeImpl::Bool) }

    /// Returns the type variable name, if this is a type variable.
    pub fn as_var(&self) -> Option<&TypeVar> {
        match &*self.0 {
            TypeImpl::TypeVar(var) => Some(var),
            _ => None,
        }
    }

    /// Returns the compound type operator and arguments, if this is a compound
    /// type.
    pub fn as_compound(&self) -> Option<&Compound> {
        match &*self.0 {
            TypeImpl::Compound(compound) => Some(compound),
            _ => None,
        }
    }

    /// Returns the function type argument and return type, if this is a
    /// function type.
    pub fn as_function(&self) -> Option<&Function> {
        match &*self.0 {
            TypeImpl::Function(function) => Some(function),
            _ => None,
        }
    }
}

/// Trait for objects that have a type.
pub trait HasType {
    /// Returns the type of this object.
    fn ty(&self) -> Type;
}
