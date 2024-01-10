use std::sync::Arc;

use arcstr::ArcStr;
use eyre::{ensure, Result};

use crate::core::types::Type;

/// A variable.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Var {
    pub name: ArcStr,
    pub ty: Type,
}

/// A constant.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Constant {
    pub name: ArcStr,
    pub ty: Type,
}

/// A function application.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Application {
    pub func: Term,
    pub arg: Term,
}

/// A lambda abstraction.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Abstraction {
    pub param: Var,
    pub body: Term,
}

/// Equality of terms.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Equality {
    pub left: Term,
    pub right: Term,
}

/// Implication of terms.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Implication {
    pub antecedent: Term,
    pub consequent: Term,
}

/// A term.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Term(pub Arc<TermImpl>);

/// Implementation of [`Term`].
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum TermImpl {
    Var(Var),
    Constant(Constant),
    Application(Application),
    Abstraction(Abstraction),
    Equality(Equality),
    Implication(Implication),
}

impl Term {
    /// Creates a new variable.
    pub fn var(name: impl Into<ArcStr>, ty: Type) -> Self {
        let name = name.into();
        Self(Arc::new(TermImpl::Var(Var { name, ty })))
    }

    /// Creates a new constant.
    pub fn constant(name: impl Into<ArcStr>, ty: Type) -> Self {
        let name = name.into();
        Self(Arc::new(TermImpl::Constant(Constant { name, ty })))
    }

    /// Creates a new application.
    pub fn application(func: Term, arg: Term) -> Result<Self> {
        ensure!(func.ty().as_function().is_some(), "`func` is not a function");
        ensure!(func.ty().as_function().unwrap().arg == arg.ty(), "argument type mismatch");
        Ok(Self(Arc::new(TermImpl::Application(Application { func, arg }))))
    }

    /// Creates a new abstraction.
    pub fn abstraction(name: impl Into<ArcStr>, ty: Type, body: Term) -> Self {
        let name = name.into();
        Self(Arc::new(TermImpl::Abstraction(Abstraction {
            param: Var { name, ty },
            body,
        })))
    }

    /// Creates a new equality.
    pub fn equality(left: Term, right: Term) -> Result<Self> {
        ensure!(left.ty() == right.ty(), "type mismatch");
        Ok(Self(Arc::new(TermImpl::Equality(Equality { left, right }))))
    }

    /// Creates a new implication.
    pub fn implication(antecedent: Term, consequent: Term) -> Result<Self> {
        ensure!(antecedent.ty() == Type::bool(), "antecedent is not a boolean");
        ensure!(consequent.ty() == Type::bool(), "consequent is not a boolean");
        Ok(Self(Arc::new(TermImpl::Implication(Implication {
            antecedent,
            consequent,
        }))))
    }

    /// Returns the type of this term.
    pub fn ty(&self) -> Type {
        match &*self.0 {
            TermImpl::Var(var) => var.ty.clone(),
            TermImpl::Constant(constant) => constant.ty.clone(),
            TermImpl::Application(application) => {
                application.func.ty().as_function().unwrap().ret.clone()
            }
            TermImpl::Abstraction(abstraction) => {
                Type::function(abstraction.param.ty.clone(), abstraction.body.ty().clone())
            }
            TermImpl::Equality(_) => Type::bool(),
            TermImpl::Implication(_) => Type::bool(),
        }
    }
}
