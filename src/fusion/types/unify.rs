use std::collections::HashSet;

use eyre::{bail, ensure, Result};

use crate::fusion::types::{Type, TypeImpl, TypeVar};

/// Alpha-renames a type.
///
/// Rename type variables in `ty` so that they do not conflict with `ref_ty`.
pub fn alpha(ty: Type, ref_ty: &HashSet<&TypeVar>) -> impl Fn(Type) -> Type + 'static {
    let vars = ty
        .free_vars()
        .intersection(ref_ty)
        .map(|&var| (var.clone(), Type::anon()))
        .collect::<Vec<_>>();
    move |mut ty| {
        for (var, anon) in vars.iter() {
            ty = ty.subst(var, anon.clone());
        }
        ty
    }
}

/// Unifies two types.
///
/// Try instantiating type variables in `a` and `b` so that they become equal.
/// When successful, returns a function that can be used to instantiate other
/// types with the same substitutions.
pub fn unify<T>(types: T) -> Result<impl Fn(Type) -> Type + 'static>
where
    T: IntoIterator<Item = Type>,
    <T as IntoIterator>::IntoIter: Clone,
{
    let mut inferred = vec![];
    let mut constraints = vec![];
    let types = types.into_iter();
    for (a, b) in types.clone().zip(types.skip(1)) {
        debug_assert!(a.free_vars().intersection(&b.free_vars()).count() == 0);
        constraints.push((a, b));
    }

    while let Some((a, b)) = constraints.pop() {
        let top = match (&*a.0, &*b.0) {
            (TypeImpl::TypeVar(a), TypeImpl::TypeVar(b)) if a.name() == b.name() => None,
            (TypeImpl::TypeVar(a), _) if !a.occurs_in(&b) => Some((a.clone(), b.clone())),
            (_, TypeImpl::TypeVar(b)) if !b.occurs_in(&a) => Some((b.clone(), a.clone())),
            (TypeImpl::Compound(a), TypeImpl::Compound(b)) => {
                ensure!(a.op() == b.op(), "cannot unify {:?} and {:?}", a, b);
                let a = a.args();
                let b = b.args();
                ensure!(a.len() == b.len(), "cannot unify {:?} and {:?}", a, b);
                constraints.extend(a.iter().cloned().zip(b.iter().cloned()));
                None
            }
            (TypeImpl::Function(a), TypeImpl::Function(b)) => {
                constraints.push((a.arg().clone(), b.arg().clone()));
                constraints.push((a.ret().clone(), b.ret().clone()));
                None
            }
            _ => bail!("cannot unify {:?} and {:?}", a, b),
        };
        if let Some((name, ty)) = top {
            for (a, b) in constraints.iter_mut() {
                *a = a.subst(&name, ty.clone());
                *b = b.subst(&name, ty.clone());
            }
            inferred.push((name, ty));
        }
    }

    Ok(move |mut rty: Type| {
        for (name, ty) in inferred.iter() {
            rty = rty.subst(name, ty.clone());
        }
        rty
    })
}

impl TypeVar {
    fn occurs_in(&self, ty: &Type) -> bool {
        match &*ty.0 {
            TypeImpl::TypeVar(var) => self.name() == var.name(),
            TypeImpl::Compound(compound) => compound.args().iter().any(|arg| self.occurs_in(arg)),
            TypeImpl::Function(function) => {
                self.occurs_in(function.arg()) || self.occurs_in(function.ret())
            }
            TypeImpl::Bool => false,
        }
    }
}
