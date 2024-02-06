use std::collections::HashSet;

use arcstr::ArcStr;
use eyre::{bail, ensure, Result};

use crate::core::{
    terms::{Term, TermImpl},
    types::{HasType, Type},
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
    pub(super) fn new(func: Term, arg: Term) -> Result<Self> {
        let func_ty = func.ty();
        let Some(func_ty) = func_ty.as_function() else {
            bail!("`func` is not a function")
        };
        ensure!(func_ty.arg() == &arg.ty(), "argument type mismatch");
        Ok(Self { func, arg })
    }

    /// Returns the function of this application.
    pub fn func(&self) -> &Term { &self.func }

    /// Returns the argument of this application.
    pub fn arg(&self) -> &Term { &self.arg }
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
        let pat_ty = cases[0].pat().ty();
        let ret_ty = cases[0].body().ty();
        for case in cases.iter().skip(1) {
            ensure!(case.pat().ty() == pat_ty, "pattern type mismatch");
            ensure!(case.body().ty() == ret_ty, "case type mismatch");
        }

        Ok(Self { cases })
    }

    /// Returns the cases of this matching.
    #[allow(clippy::borrowed_box)]
    pub fn cases(&self) -> &Box<[Case]> { &self.cases }
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
            TermImpl::Matching(_) | TermImpl::Implication(_) => false,
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
    pub(super) fn new(pat: Term, body: Term) -> Result<Self> {
        ensure!(pat.is_pattern(&mut HashSet::new()), "non-pattern in case");
        Ok(Self { pat, body })
    }

    /// Returns the pattern of this case.
    pub fn pat(&self) -> &Term { &self.pat }

    /// Returns the body of this case.
    pub fn body(&self) -> &Term { &self.body }
}

impl HasType for Case {
    fn ty(&self) -> Type { Type::function(self.pat.ty(), self.body.ty()) }
}

/// Implication of terms.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Implication {
    antecedent: Term,
    consequent: Term,
}

impl Implication {
    /// Creates a new implication.
    pub(super) fn new(antecedent: Term, consequent: Term) -> Result<Self> {
        ensure!(antecedent.ty() == Type::bool(), "antecedent type mismatch");
        ensure!(consequent.ty() == Type::bool(), "consequent type mismatch");
        Ok(Self {
            antecedent,
            consequent,
        })
    }

    /// Returns the antecedent of this implication.
    pub fn antecedent(&self) -> &Term { &self.antecedent }

    /// Returns the consequent of this implication.
    pub fn consequent(&self) -> &Term { &self.consequent }
}

impl HasType for Implication {
    fn ty(&self) -> Type { Type::bool() }
}
