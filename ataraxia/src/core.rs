//! Core logic for LCF calculus.

use crate::term::{Term, Type};

pub fn bool() -> Type {
    Type::basic("bool")
}

pub fn tt() -> Term {
    Term::var("tt", bool())
}

pub fn ff() -> Term {
    Term::var("ff", bool())
}

pub fn uu(ty: Type) -> Term {
    Term::var("uu", ty)
}

pub fn ite(ty: Type) -> Term {
    let ty = Type::func(bool(), Type::func(ty.clone(), Type::func(ty.clone(), ty)));
    Term::var("ite", ty)
}

pub fn fix(ty: Type) -> Term {
    let ty = Type::func(Type::func(ty.clone(), ty.clone()), ty);
    Term::var("fix", ty)
}