//! Theorem in the LCF calculus.

use std::fmt::Display;

use crate::term::{Formula, FormulaSet, Sentence, Term, Type};

use arcstr::ArcStr;
use eyre::{ensure, Context, OptionExt, Result};

/// A theorem in the LCF calculus.
///
/// # Semantics
/// A theorem means that it is provable that the left-hand side implies
/// the right-hand side.
///
/// Theorems can only be created by its constructor functions, which
/// ensures that theorems are always valid.
#[derive(Debug, Clone)]
pub struct Theorem(Sentence);

/// Create a theorem.
macro_rules! theorem {
    (=> $right: expr) => {
        Theorem::new(FormulaSet::empty(), $right)
    };
    ($left: expr => $right: expr) => {
        Theorem::new($left, $right)
    };
}

/// Create a formula. Assumes that the formula is well-typed.
macro_rules! uformula {
    ($left: expr, $right: expr) => {
        Formula::new($left, $right).expect("internal error")
    };
}

/// Create a application term. Assumes that the term is well-typed.
macro_rules! uapp {
    ($left: expr, $right: expr) => {
        Term::app($left, $right).expect("internal error")
    };
}

/// Create a equivalence formula set. Assumes that the formulas are well-typed.
macro_rules! uequiv {
    ($left: expr, $right: expr) => {
        FormulaSet::equiv($left, $right).expect("internal error")
    };
}

impl Theorem {
    fn new(left: FormulaSet, right: FormulaSet) -> Self {
        Self(Sentence::new(left, right))
    }

    /// Get the sentence of the theorem.
    pub fn sentence(&self) -> &Sentence {
        &self.0
    }

    /// Display the theorem.
    pub fn display(&self, show_type: bool) -> impl Display + '_ {
        self.0.display(show_type)
    }

    /// Declare an axiom.
    ///
    /// # Safety
    /// Axioms are part of the trusted base of the LCF calculus. New axioms should
    /// only be added when extending the logic, and should be carefully checked for
    /// consistency.
    pub unsafe fn axiom(sentence: Sentence) -> Self {
        Self(sentence)
    }

    /// Include rule.
    ///
    /// # Semantics
    /// ```text
    /// P |- Q, R
    /// ----------
    ///   P |- Q
    /// ```
    pub fn include(self, p: FormulaSet) -> Result<Self> {
        ensure!(
            p.is_subset(&self.0.right),
            "include: formula not in right side of `p`"
        );
        Ok(theorem!(self.0.left.union(p) => self.0.right))
    }

    /// Cut rule.
    ///
    /// # Semantics
    /// ```text
    /// P |- Q    Q |- R
    /// ----------------
    ///      P |- R
    /// ```
    pub fn cut(self, other: Self) -> Result<Self> {
        ensure!(self.0.right == other.0.left, "cut: formulas do not match");
        Ok(theorem!(self.0.left => other.0.right))
    }

    /// Conjuction rule.
    ///
    /// # Semantics
    /// ```text
    /// P |- Q    P |- R
    /// ----------------
    ///    P |- Q, R
    pub fn conj(self, other: Self) -> Result<Self> {
        ensure!(self.0.left == other.0.left, "conj: formulas do not match");
        Ok(theorem!(self.0.left => self.0.right.union(other.0.right)))
    }

    /// Apply rule.
    ///
    /// # Semantics
    /// ```text
    ///    P |- s ⊑ t
    /// ------------------
    ///  P |- u(s) ⊑ u(t)
    /// ```
    pub fn apply(self, u: Term) -> Result<Self> {
        let formula = self
            .0
            .right
            .single()
            .ok_or_eyre("apply: more than one formula on right")?;
        let right = FormulaSet::unit({
            let left = Term::app(u.clone(), formula.left).wrap_err("apply: apply to left")?;
            let right = Term::app(u, formula.right).wrap_err("apply: apply to right")?;
            Formula::new(left, right).wrap_err("apply: combine left and right")?
        });
        Ok(theorem!(self.0.left => right))
    }

    /// Reflexivity rule.
    ///
    /// # Semantics
    /// ```text
    /// |- s ⊑ s
    /// ```
    pub fn refl(s: Term) -> Self {
        let formula = uformula!(s.clone(), s);
        theorem!(=> FormulaSet::unit(formula))
    }

    /// Transitivity rule.
    ///
    /// # Semantics
    /// ```text
    /// P |- s ⊑ t    P |- t ⊑ u
    /// -------------------------
    ///         P |- s ⊑ u
    /// ```
    pub fn trans(self, other: Self) -> Result<Self> {
        ensure!(
            self.0.left == other.0.left,
            "trans: left formulas do not match"
        );
        let f1 = self
            .0
            .right
            .single()
            .ok_or_eyre("trans: more than one formula on right")?;
        let f2 = other
            .0
            .right
            .single()
            .ok_or_eyre("trans: more than one formula on right")?;
        ensure!(f1.right == f2.left, "trans: right formulas do not match");
        let right = FormulaSet::unit(
            Formula::new(f1.left, f2.right).wrap_err("trans: combine left and right")?,
        );
        Ok(theorem!(self.0.left => right))
    }

    /// Min rule.
    ///
    /// # Semantics
    /// ```text
    /// |- UU ⊑ s
    /// ```
    pub fn min(s: Term) -> Result<Self> {
        let uu = Term::uu(s.ty().clone());
        let formula = uformula!(uu, s);
        Ok(theorem!(=> FormulaSet::unit(formula)))
    }

    /// Min rule for applications.
    ///
    /// # Semantics
    /// ```text
    /// |- UU(s) ⊑ UU
    /// ```
    pub fn min_app(s: Term, ret_ty: Type) -> Result<Self> {
        let uu_fun = Term::uu(Type::func(s.ty().clone(), ret_ty.clone()));
        let uu_ret = Term::uu(ret_ty);
        let formula = uformula!(uapp!(uu_fun, s), uu_ret);
        Ok(theorem!(=> FormulaSet::unit(formula)))
    }

    /// Conditional true rule.
    ///
    /// # Semantics
    /// ```text
    /// |- ite(tt, s, t) ≡ s
    /// ```
    pub fn cond_true(s: Term, t: Term) -> Result<Self> {
        ensure!(
            s.ty() == t.ty(),
            "cond_true: terms do not have the same type"
        );
        let ite = Term::ite(s.ty().clone());
        let term = uapp!(uapp!(uapp!(ite, Term::tt()), s.clone()), t);
        Ok(theorem!(=> uequiv!(term, s)))
    }

    /// Conditional false rule.
    ///
    /// # Semantics
    /// ```text
    /// |- ite(ff, s, t) ≡ t
    /// ```
    pub fn cond_false(s: Term, t: Term) -> Result<Self> {
        ensure!(
            s.ty() == t.ty(),
            "cond_false: terms do not have the same type"
        );
        let ite = Term::ite(s.ty().clone());
        let term = uapp!(uapp!(uapp!(ite, Term::ff()), s), t.clone());
        Ok(theorem!(=> uequiv!(term, t)))
    }

    /// Conditional unknown rule.
    ///
    /// # Semantics
    /// ```text
    /// |- ite(uu, s, t) ≡ uu
    /// ```
    pub fn cond_unknown(s: Term, t: Term) -> Result<Self> {
        ensure!(
            s.ty() == t.ty(),
            "cond_unknown: terms do not have the same type"
        );
        let ite = Term::ite(s.ty().clone());
        let term = uapp!(uapp!(uapp!(ite, Term::uu(Type::bool())), s.clone()), t);
        Ok(theorem!(=> uequiv!(term, Term::uu(s.ty().clone()))))
    }

    /// Abstraction rule.
    ///
    /// # Semantics
    /// ```text
    ///    P |- s ⊑ t
    /// ----------------
    /// P |- λx.s ⊑ λx.t
    /// ```
    pub fn abstr(self, x: impl Into<ArcStr>, ty: Type) -> Result<Self> {
        let x = x.into();
        let formula = self
            .0
            .right
            .single()
            .ok_or_eyre("abstr: more than one formula on right")?;
        let right = FormulaSet::unit({
            let left = Term::abs(x.clone(), ty.clone(), formula.left);
            let right = Term::abs(x, ty, formula.right);
            Formula::new(left, right).wrap_err("abstr: combine left and right")?
        });
        Ok(theorem!(self.0.left => right))
    }

    /// Conversion rule.
    ///
    /// # Semantics
    /// ```text
    /// |- (λx.s)(t) ≡ s[t/x]
    /// ```
    pub fn conv(func: Term, t: Term) -> Result<Self> {
        let arg = func.arg().ok_or_eyre("conv: not a function")?;
        let right = func
            .body()
            .expect("internal error")
            .clone()
            .subst(arg, t.clone());
        let left = Term::app(func, t).wrap_err("conv: apply")?;
        Ok(theorem!(=> uequiv!(left, right)))
    }

    /// Function rule.
    ///
    /// # Semantics
    /// ```text
    /// |- λx.f(x) ≡ f
    pub fn func(f: Term, x: impl Into<ArcStr>) -> Result<Self> {
        let x = x.into();
        let left = Term::abs(
            x.clone(),
            f.ty().clone(),
            Term::app(f.clone(), Term::var(x, f.ty().clone())).wrap_err("func: apply")?,
        );
        let right = f;
        Ok(theorem!(=> uequiv!(left, right)))
    }

    // TODO: finish implementing the rest of the rules
}
