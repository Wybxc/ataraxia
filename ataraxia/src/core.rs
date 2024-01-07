//! Core logic for LCF calculus.

use crate::term::{Term, Type};

impl Type {
    /// Boolean type.
    ///
    /// # Semantics
    /// There are three values of type `bool`: `tt`, `ff`, and `uu`.
    /// `tt` and `ff` are the usual true and false values, while `uu` is
    /// an unknown value.
    pub fn bool() -> Type {
        Type::basic("bool")
    }

    /// Check if the type is the boolean type.
    pub fn is_bool(&self) -> bool {
        self == &Self::bool()
    }
}

impl Term {
    /// The true value.
    ///
    /// # Semantics
    /// `tt` is the true value of type `bool`.
    pub fn tt() -> Term {
        Term::var("tt", Type::bool())
    }

    /// The false value.
    ///
    /// # Semantics
    /// `ff` is the false value of type `bool`.
    pub fn ff() -> Term {
        Term::var("ff", Type::bool())
    }

    /// The unknown value.
    ///
    /// # Semantics
    /// `uu` is the bottom value of the given type, usually used to represent
    /// the unknown value.
    pub fn uu(ty: Type) -> Term {
        Term::var("uu", ty)
    }

    /// The if-then-else function.
    ///
    /// # Semantics
    /// `ite` is the if-then-else function, which takes a condition, a then
    /// branch, and an else branch.
    ///
    /// It is defined as follows:
    /// - If the condition is `tt`, the result is the then branch.
    /// - If the condition is `ff`, the result is the else branch.
    /// - If the condition is `uu`, the result is `uu`.
    pub fn ite(ty: Type) -> Term {
        let ty = Type::func(
            Type::bool(),
            Type::func(ty.clone(), Type::func(ty.clone(), ty)),
        );
        Term::var("ite", ty)
    }

    /// The fixpoint combinator.
    ///
    /// # Semantics
    /// `fix` is the fixpoint combinator, which takes a function and returns
    /// its fixed point.
    ///
    /// `ℱx.t` is shorthand for `fix(λx.t)`.
    pub fn fix(ty: Type) -> Term {
        let ty = Type::func(Type::func(ty.clone(), ty.clone()), ty);
        Term::var("fix", ty)
    }

    /// Check if the term is the true value.
    pub fn is_tt(&self) -> bool {
        self == &Self::tt()
    }

    /// Check if the term is the false value.
    pub fn is_ff(&self) -> bool {
        self == &Self::ff()
    }

    /// Check if the term is the `uu` of the given type.
    pub fn is_uu(&self, ty: Type) -> bool {
        self == &Self::uu(ty)
    }

    /// Check if the term is the if-then-else function.
    pub fn is_ite(&self, ty: Type) -> bool {
        self == &Self::ite(ty)
    }

    /// Check if the term is the fixpoint combinator.
    pub fn is_fix(&self, ty: Type) -> bool {
        self == &Self::fix(ty)
    }
}
