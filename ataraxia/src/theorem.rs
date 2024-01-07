//! Theorem in the LCF calculus.

use std::fmt::Display;

use crate::term::{Formula, FormulaSet, Sentence, Term, Type};

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

macro_rules! theorem {
    (=> $right: expr) => {
        Theorem::new(FormulaSet::empty(), $right)
    };
    ($left: expr => $right: expr) => {
        Theorem::new($left, $right)
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
        let formula = Formula::new(s.clone(), s).expect("internal error");
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
        let formula = Formula::new(uu, s).expect("internal error");
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
        let formula = Formula::new(Term::app(uu_fun, s).expect("internal error"), uu_ret)
            .expect("internal error");
        Ok(theorem!(=> FormulaSet::unit(formula)))
    }

    // TODO: finish implementing the rest of the rules
}
