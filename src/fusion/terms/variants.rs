use std::{borrow::Cow, collections::HashSet, ops::Not};

use eyre::{bail, ensure, Result};
use smol_str::SmolStr;

use crate::fusion::{
    terms::{Term, TermImpl},
    types::{
        unify::{alpha, unify},
        HasType, Type,
    },
};

/// A variable.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Var {
    name: SmolStr,
    ty: Type,
}

impl Var {
    /// Creates a new variable.
    pub(super) fn new(name: impl Into<SmolStr>, ty: Type) -> Self {
        let name = name.into();
        Self { name, ty }
    }

    /// Returns the name of this variable.
    pub fn name(&self) -> &SmolStr { &self.name }

    /// Instantiates type variables in this variable.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Option<Self> {
        let ty = inst(self.ty.clone());
        ty.is(&self.ty).not().then(|| Var {
            name: self.name.clone(),
            ty,
        })
    }
}

impl HasType for Var {
    fn ty(&self) -> Type { self.ty.clone() }
}

/// A constant.
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Constant {
    name: SmolStr,
    ty: Type,
}

impl Constant {
    /// Creates a new constant.
    pub(super) fn new(name: impl Into<SmolStr>, ty: Type) -> Self {
        let name = name.into();
        Self { name, ty }
    }

    /// Returns the name of this constant.
    pub fn name(&self) -> &SmolStr { &self.name }

    /// Instantiates type variables in this constant.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Option<Self> {
        let ty = inst(self.ty.clone());
        ty.is(&self.ty).not().then(|| Constant {
            name: self.name.clone(),
            ty,
        })
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
            func = func.instantiate(&alpha(func.ty(), &arg.ty().free_vars()));
            let inst = unify([func.ty().as_function().unwrap().arg().clone(), arg.ty()])?;
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
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Option<Self> {
        let func = self.func.instantiate(inst);
        let arg = self.arg.instantiate(inst);
        (func.is(&self.func) && arg.is(&self.arg))
            .not()
            .then_some(Application { func, arg })
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
    pub(super) fn new(cases: impl Into<Vec<Case>>) -> Result<Self> {
        let mut cases = cases.into();
        ensure!(!cases.is_empty(), "empty match");

        let inst = unify(cases.iter().map(|case| case.pat().ty()))?;
        for case in cases.iter_mut() {
            if let Some(new_case) = case.instantiate(&inst) {
                *case = new_case;
            }
        }

        let inst = unify(cases.iter().map(|case| case.body().ty()))?;
        for case in cases.iter_mut() {
            if let Some(new_case) = case.instantiate(&inst) {
                *case = new_case;
            }
        }

        let cases = cases.into_boxed_slice();
        Ok(Self { cases })
    }

    /// Returns the cases of this matching.
    pub fn cases(&self) -> &[Case] { &self.cases }

    /// Instantiates type variables in this matching.
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Option<Self> {
        let mut cases = Cow::from(&self.cases[..]);
        for (i, case) in self.cases().iter().enumerate() {
            if let Some(case) = case.instantiate(inst) {
                cases.to_mut()[i] = case;
            }
        }
        if matches!(cases, Cow::Borrowed(_)) {
            None
        } else {
            Some(Matching {
                cases: Box::from(cases.into_owned()),
            })
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
            TermImpl::Matching(_) => false,
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
    pub fn instantiate(&self, inst: &impl Fn(Type) -> Type) -> Option<Self> {
        let pat = self.pat.instantiate(inst);
        let body = self.body.instantiate(inst);
        (pat.is(&self.pat) && body.is(&self.body))
            .not()
            .then_some(Case { pat, body })
    }
}

impl HasType for Case {
    fn ty(&self) -> Type { Type::function(self.pat.ty(), self.body.ty()) }
}
