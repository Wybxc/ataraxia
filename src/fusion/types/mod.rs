use std::{borrow::Cow, collections::HashSet, rc::Rc, sync::atomic::AtomicI32};

use smol_str::SmolStr;

pub mod builder;
pub mod unify;
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
}

impl Type {
    /// Creates a new type variable.
    pub fn var(name: impl Into<SmolStr>) -> Self {
        Self(Rc::new(TypeImpl::TypeVar(TypeVar::new(name))))
    }

    /// Create an anonymous type variable.
    pub fn anon() -> Self {
        static COUNTER: AtomicI32 = AtomicI32::new(1);
        let id = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        Self::var(format!("?{}", id))
    }

    /// Creates a new constant type (a compound type with no arguments).
    pub fn constant(name: impl Into<SmolStr>) -> Self {
        Self(Rc::new(TypeImpl::Compound(Compound::constant(name))))
    }

    /// Creates a new compound type.
    pub fn compound(op: impl Into<SmolStr>, args: impl Into<Box<[Type]>>) -> Self {
        Self(Rc::new(TypeImpl::Compound(Compound::new(op, args))))
    }

    /// Creates a new function type.
    pub fn function(arg: Type, ret: Type) -> Self {
        Self(Rc::new(TypeImpl::Function(Function::new(arg, ret))))
    }

    /// Checks if this is a type variable.
    pub fn is_var(&self) -> bool { matches!(&*self.0, TypeImpl::TypeVar(_)) }

    /// Checks if this is a compound type.
    pub fn is_compound(&self) -> bool { matches!(&*self.0, TypeImpl::Compound(_)) }

    /// Checks if this is a function type.
    pub fn is_function(&self) -> bool { matches!(&*self.0, TypeImpl::Function(_)) }

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

    /// Returns whether this type is a clone of another type.
    pub fn is(&self, other: &Self) -> bool { Rc::ptr_eq(&self.0, &other.0) }

    /// Returns free type variables in this type.
    pub fn free_vars(&self) -> HashSet<&TypeVar> {
        let mut set = HashSet::new();
        self.free_vars_impl(&mut set);
        set
    }

    fn free_vars_impl(&self, set: &mut HashSet<&TypeVar>) {
        match &*self.0 {
            TypeImpl::TypeVar(var) => {
                set.insert(var);
            }
            TypeImpl::Compound(compound) => {
                for arg in compound.args() {
                    arg.free_vars_impl(set);
                }
            }
            TypeImpl::Function(function) => {
                function.arg().free_vars_impl(set);
                function.ret().free_vars_impl(set);
            }
        }
    }

    /// Instantiates type variables in this type.
    pub fn subst(&self, var: &TypeVar, ty: Type) -> Type {
        match &*self.0 {
            TypeImpl::TypeVar(var2) if var == var2 => ty,
            TypeImpl::TypeVar(_) => self.clone(),
            TypeImpl::Compound(compound) => {
                let mut new_args = Cow::from(compound.args());
                for (i, arg) in compound.args().iter().enumerate() {
                    let new_arg = arg.subst(var, ty.clone());
                    if !new_arg.is(arg) {
                        new_args.to_mut()[i] = new_arg;
                    }
                }
                if matches!(new_args, Cow::Borrowed(_)) {
                    self.clone()
                } else {
                    Type::compound(compound.op().clone(), new_args.into_owned())
                }
            }
            TypeImpl::Function(function) => {
                let arg = function.arg().subst(var, ty.clone());
                let ret = function.ret().subst(var, ty);
                if arg.is(function.arg()) && ret.is(function.ret()) {
                    self.clone()
                } else {
                    Type::function(arg, ret)
                }
            }
        }
    }
}

/// Trait for objects that have a type.
pub trait HasType {
    /// Returns the type of this object.
    fn ty(&self) -> Type;
}
