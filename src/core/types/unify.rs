use eyre::{bail, ensure, Result};

use crate::core::types::{Type, TypeImpl, TypeVar};

/// Unifies two types.
///
/// Try instantiating type variables in `a` and `b` so that they become equal.
/// When successful, returns a function that can be used to instantiate other
/// types with the same substitutions.
pub fn unify(types: &[Type]) -> Result<impl Fn(Type) -> Type> {
    let mut inferred = vec![];

    if types.len() >= 2 {
        let mut constraints = vec![];
        for (a, b) in types.iter().zip(types.iter().skip(1)) {
            constraints.push((a.clone(), b.clone()));
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
                (TypeImpl::Bool, TypeImpl::Bool) => None,
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
    }

    Ok(move |mut rty: Type| {
        if inferred.is_empty() {
            return rty;
        }
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
