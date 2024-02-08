use std::rc::Rc;

use arcstr::ArcStr;
use eyre::Result;

// mod compute;
mod variants;

pub use variants::*;

use crate::core::types::{HasType, Type};

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
    Equality(Equality),
    Implication(Implication),
}

impl Term {
    /// Creates a new variable.
    pub fn var(name: impl Into<ArcStr>, ty: Type) -> Self {
        Self(Rc::new(TermImpl::Var(Var::new(name, ty))))
    }

    /// Creates a new constant.
    pub fn constant(name: impl Into<ArcStr>, ty: Type) -> Self {
        Self(Rc::new(TermImpl::Constant(Constant::new(name, ty))))
    }

    /// Creates a new application.
    pub fn application(func: Term, arg: Term) -> Result<Self> {
        Ok(Self(Rc::new(TermImpl::Application(Application::new(func, arg)?))))
    }

    /// Creates a new match.
    pub fn matching(cases: impl Into<Box<[Case]>>) -> Result<Self> {
        Ok(Self(Rc::new(TermImpl::Matching(Matching::new(cases)?))))
    }

    /// Creates a new equality.
    pub fn equality(left: Term, right: Term) -> Result<Self> {
        Ok(Self(Rc::new(TermImpl::Equality(Equality::new(left, right)?))))
    }

    /// Creates a new implication.
    pub fn implication(antecedent: Term, consequent: Term) -> Result<Self> {
        Ok(Self(Rc::new(TermImpl::Implication(Implication::new(antecedent, consequent)?))))
    }

    /// Checks if the term is a variable.
    pub fn is_var(&self) -> bool { matches!(&*self.0, TermImpl::Var(_)) }

    /// Checks if the term is a constant.
    pub fn is_constant(&self) -> bool { matches!(&*self.0, TermImpl::Constant(_)) }

    /// Checks if the term is an application.
    pub fn is_application(&self) -> bool { matches!(&*self.0, TermImpl::Application(_)) }

    /// Checks if the term is a match.
    pub fn is_matching(&self) -> bool { matches!(&*self.0, TermImpl::Matching(_)) }

    /// Checks if the term is an equality.
    pub fn is_equality(&self) -> bool { matches!(&*self.0, TermImpl::Equality(_)) }

    /// Checks if the term is an implication.
    pub fn is_implication(&self) -> bool { matches!(&*self.0, TermImpl::Implication(_)) }

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

    /// Returns a reference to the equality, if this is an equality.
    pub fn as_equality(&self) -> Option<&Equality> {
        match &*self.0 {
            TermImpl::Equality(equality) => Some(equality),
            _ => None,
        }
    }

    /// Returns a reference to the implication, if this is an implication.
    pub fn as_implication(&self) -> Option<&Implication> {
        match &*self.0 {
            TermImpl::Implication(implication) => Some(implication),
            _ => None,
        }
    }

    /// Instantiates type variables in this term.
    ///
    /// TODO: Copy on write
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Self {
        match &*self.0 {
            TermImpl::Var(var) => Term(Rc::new(TermImpl::Var(var.instantiate(inst)))),
            TermImpl::Constant(constant) => {
                Term(Rc::new(TermImpl::Constant(constant.instantiate(inst))))
            }
            TermImpl::Application(application) => {
                Term(Rc::new(TermImpl::Application(application.instantiate(inst))))
            }
            TermImpl::Matching(matching) => {
                Term(Rc::new(TermImpl::Matching(matching.instantiate(inst))))
            }
            TermImpl::Equality(equality) => {
                Term(Rc::new(TermImpl::Equality(equality.instantiate(inst))))
            }
            TermImpl::Implication(implication) => {
                Term(Rc::new(TermImpl::Implication(implication.instantiate(inst))))
            }
        }
    }
}

impl HasType for Term {
    fn ty(&self) -> Type {
        match &*self.0 {
            TermImpl::Var(var) => var.ty(),
            TermImpl::Constant(constant) => constant.ty(),
            TermImpl::Application(application) => application.ty(),
            TermImpl::Matching(matching) => matching.ty(),
            TermImpl::Equality(equality) => equality.ty(),
            TermImpl::Implication(implication) => implication.ty(),
        }
    }
}
