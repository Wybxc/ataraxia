use std::rc::Rc;

use eyre::Result;
use smol_str::SmolStr;

// mod compute;
pub(crate) mod builder;
mod variants;

pub use variants::*;

use crate::fusion::types::{
    unify::{alpha, unify},
    HasType, Type,
};

/// A term.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Term(pub Rc<TermImpl>);

/// Implementation of [`Term`].
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum TermImpl {
    Var(Var),
    Constant(Constant),
    Application(Application),
    Matching(Matching),
}

impl Term {
    /// Creates a new variable.
    pub fn var(name: impl Into<SmolStr>, ty: Type) -> Self {
        Self(Rc::new(TermImpl::Var(Var::new(name, ty))))
    }

    /// Creates a new constant.
    pub fn constant(name: impl Into<SmolStr>, ty: Type) -> Self {
        Self(Rc::new(TermImpl::Constant(Constant::new(name, ty))))
    }

    /// Creates a new application.
    pub fn application(func: Term, arg: Term) -> Result<Self> {
        Ok(Self(Rc::new(TermImpl::Application(Application::new(func, arg)?))))
    }

    /// Creates a new match.
    pub fn matching(cases: impl Into<Vec<Case>>) -> Result<Self> {
        Ok(Self(Rc::new(TermImpl::Matching(Matching::new(cases)?))))
    }

    /// Checks if the term is a variable.
    pub fn is_var(&self) -> bool { matches!(&*self.0, TermImpl::Var(_)) }

    /// Checks if the term is a constant.
    pub fn is_constant(&self) -> bool { matches!(&*self.0, TermImpl::Constant(_)) }

    /// Checks if the term is an application.
    pub fn is_application(&self) -> bool { matches!(&*self.0, TermImpl::Application(_)) }

    /// Checks if the term is a match.
    pub fn is_matching(&self) -> bool { matches!(&*self.0, TermImpl::Matching(_)) }

    /// Returns a reference to the variable, if this is a variable.
    pub fn as_var(&self) -> Option<&Var> {
        match &*self.0 {
            TermImpl::Var(var) => Some(var),
            _ => None,
        }
    }

    /// Returns a reference to the constant, if this is a constant.
    pub fn as_constant(&self) -> Option<&Constant> {
        match &*self.0 {
            TermImpl::Constant(constant) => Some(constant),
            _ => None,
        }
    }

    /// Returns a reference to the application, if this is an application.
    pub fn as_application(&self) -> Option<&Application> {
        match &*self.0 {
            TermImpl::Application(application) => Some(application),
            _ => None,
        }
    }

    /// Returns a reference to the match, if this is a match.
    pub fn as_matching(&self) -> Option<&Matching> {
        match &*self.0 {
            TermImpl::Matching(matching) => Some(matching),
            _ => None,
        }
    }

    /// Returns whether this term is a clone of another term.
    pub fn is(&self, other: &Self) -> bool { Rc::ptr_eq(&self.0, &other.0) }

    /// Instantiates type variables in this term.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Self {
        match &*self.0 {
            TermImpl::Var(var) => var
                .instantiate(inst)
                .map(|var| Term(Rc::new(TermImpl::Var(var)))),
            TermImpl::Constant(constant) => constant
                .instantiate(inst)
                .map(|constant| Term(Rc::new(TermImpl::Constant(constant)))),
            TermImpl::Application(application) => application
                .instantiate(inst)
                .map(|application| Term(Rc::new(TermImpl::Application(application)))),
            TermImpl::Matching(matching) => matching
                .instantiate(inst)
                .map(|matching| Term(Rc::new(TermImpl::Matching(matching)))),
        }
        .unwrap_or_else(|| self.clone())
    }

    /// Coerces this term to a given type.
    pub fn coercion(&self, ty: Type) -> Result<Self> {
        let result = self.instantiate(&alpha(self.ty(), &ty.free_vars()));
        let inst = unify([result.ty(), ty])?;
        Ok(result.instantiate(&inst))
    }
}

impl HasType for Term {
    fn ty(&self) -> Type {
        match &*self.0 {
            TermImpl::Var(var) => var.ty(),
            TermImpl::Constant(constant) => constant.ty(),
            TermImpl::Application(application) => application.ty(),
            TermImpl::Matching(matching) => matching.ty(),
        }
    }
}
