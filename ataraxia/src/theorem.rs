//! Theorem in the LCF calculus.

use std::fmt::Display;

use crate::term::{FormulaSet, Sentence};

use eyre::{ensure, Result};

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
            "include: formula not in theorem"
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

    // TODO: finish implementing the rest of the rules
}
