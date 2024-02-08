use std::collections::HashSet;

use arcstr::ArcStr;
use eyre::{bail, ensure, Result};

use crate::core::{
    terms::{Term, TermImpl},
    types::{unify::unify, HasType, Type},
};

/// A variable.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Var {
    name: ArcStr,
    ty: Type,
}

impl Var {
    /// Creates a new variable.
    pub(super) fn new(name: impl Into<ArcStr>, ty: Type) -> Self {
        let name = name.into();
        Self { name, ty }
    }

    /// Returns the name of this variable.
    pub fn name(&self) -> &ArcStr { &self.name }

    /// Instantiates type variables in this variable.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Self {
        Self {
            name: self.name.clone(),
            ty: inst(self.ty.clone()),
        }
    }
}

impl HasType for Var {
    fn ty(&self) -> Type { self.ty.clone() }
}

/// A constant.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Constant {
    name: ArcStr,
    ty: Type,
}

impl Constant {
    /// Creates a new constant.
    pub(super) fn new(name: impl Into<ArcStr>, ty: Type) -> Self {
        let name = name.into();
        Self { name, ty }
    }

    /// Returns the name of this constant.
    pub fn name(&self) -> &ArcStr { &self.name }

    /// Instantiates type variables in this constant.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Self {
        Self {
            name: self.name.clone(),
            ty: inst(self.ty.clone()),
        }
    }
}

impl HasType for Constant {
    fn ty(&self) -> Type { self.ty.clone() }
}

/// A function application.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Application {
    func: Term,
    arg: Term,
}

impl Application {
    /// Creates a new application.
    pub(super) fn new(mut func: Term, mut arg: Term) -> Result<Self> {
        let func_ty = func.ty();
        let Some(func_ty) = func_ty.as_function() else {
            bail!("`func` is not a function")
        };
        if func_ty.arg() != &arg.ty() {
            let inst = unify(&[func_ty.arg().clone(), arg.ty()])?;
            func = func.instantiate(&inst);
            arg = arg.instantiate(&inst);
        }
        Ok(Self { func, arg })
    }

    /// Returns the function of this application.
    pub fn func(&self) -> &Term { &self.func }

    /// Returns the argument of this application.
    pub fn arg(&self) -> &Term { &self.arg }

    /// Instantiates type variables in this application.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Self {
        Self {
            func: self.func.instantiate(inst),
            arg: self.arg.instantiate(inst),
        }
    }
}

impl HasType for Application {
    fn ty(&self) -> Type { self.func.ty().as_function().unwrap().ret().clone() }
}

/// Pattern matching on terms.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Matching {
    cases: Box<[Case]>,
}

impl Matching {
    /// Creates a new match.
    pub(super) fn new(cases: impl Into<Box<[Case]>>) -> Result<Self> {
        let cases = cases.into();
        ensure!(!cases.is_empty(), "empty match");

        let pat_ty: Box<[Type]> = cases.iter().map(|case| case.pat().ty()).collect();
        let inst = unify(&pat_ty)?;
        let cases: Box<[Case]> = cases.iter().map(|case| case.instantiate(&inst)).collect();

        let ret_ty: Box<[Type]> = cases.iter().map(|case| case.body().ty()).collect();
        let inst = unify(&ret_ty)?;
        let cases: Box<[Case]> = cases.iter().map(|case| case.instantiate(&inst)).collect();

        Ok(Self { cases })
    }

    /// Returns the cases of this matching.
    #[allow(clippy::borrowed_box)]
    pub fn cases(&self) -> &Box<[Case]> { &self.cases }

    /// Instantiates type variables in this matching.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Self {
        Self {
            cases: self
                .cases
                .iter()
                .map(|case| case.instantiate(inst))
                .collect(),
        }
    }
}

impl HasType for Matching {
    fn ty(&self) -> Type { self.cases[0].ty() }
}

/// Match case.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Case {
    pat: Term,
    body: Term,
}

impl Term {
    fn is_pattern(&self, vars: &mut HashSet<Var>) -> bool {
        match &*self.0 {
            TermImpl::Var(var) => vars.insert(var.clone()),
            TermImpl::Constant(_) => true,
            TermImpl::Matching(_) | TermImpl::Equality(_) | TermImpl::Implication(_) => false,
            TermImpl::Application(app) => {
                app.func().is_pattern(vars)
                    && app
                        .arg()
                        .as_var()
                        .is_some_and(|var| vars.insert(var.clone()))
            }
        }
    }
}

impl Case {
    /// Creates a new case.
    pub fn new(pat: Term, body: Term) -> Result<Self> {
        ensure!(pat.is_pattern(&mut HashSet::new()), "non-pattern in case");
        Ok(Self { pat, body })
    }

    /// Returns the pattern of this case.
    pub fn pat(&self) -> &Term { &self.pat }

    /// Returns the body of this case.
    pub fn body(&self) -> &Term { &self.body }

    /// Instantiates type variables in this case.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Self {
        Self {
            pat: self.pat.instantiate(inst),
            body: self.body.instantiate(inst),
        }
    }
}

impl HasType for Case {
    fn ty(&self) -> Type { Type::function(self.pat.ty(), self.body.ty()) }
}

/// Equality of terms.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Equality {
    left: Term,
    right: Term,
}

impl Equality {
    /// Creates a new equality.
    pub(super) fn new(mut left: Term, mut right: Term) -> Result<Self> {
        if left.ty() != right.ty() {
            let inst = unify(&[left.ty(), right.ty()])?;
            left = left.instantiate(&inst);
            right = right.instantiate(&inst);
        }
        Ok(Self { left, right })
    }

    /// Returns the left-hand side of this equality.
    pub fn left(&self) -> &Term { &self.left }

    /// Returns the right-hand side of this equality.
    pub fn right(&self) -> &Term { &self.right }

    /// Instantiates type variables in this equality.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Self {
        Self {
            left: self.left.instantiate(inst),
            right: self.right.instantiate(inst),
        }
    }
}

impl HasType for Equality {
    fn ty(&self) -> Type { Type::bool() }
}

/// Implication of terms.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Implication {
    antecedent: Term,
    consequent: Term,
}

impl Implication {
    /// Creates a new implication.
    pub(super) fn new(mut antecedent: Term, mut consequent: Term) -> Result<Self> {
        if antecedent.ty() != Type::bool() {
            antecedent = antecedent.instantiate(&unify(&[antecedent.ty(), Type::bool()])?);
        }
        if consequent.ty() != Type::bool() {
            consequent = consequent.instantiate(&unify(&[consequent.ty(), Type::bool()])?);
        }
        Ok(Self {
            antecedent,
            consequent,
        })
    }

    /// Returns the antecedent of this implication.
    pub fn antecedent(&self) -> &Term { &self.antecedent }

    /// Returns the consequent of this implication.
    pub fn consequent(&self) -> &Term { &self.consequent }

    /// Instantiates type variables in this implication.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Self {
        Self {
            antecedent: self.antecedent.instantiate(inst),
            consequent: self.consequent.instantiate(inst),
        }
    }
}

impl HasType for Implication {
    fn ty(&self) -> Type { Type::bool() }
}
