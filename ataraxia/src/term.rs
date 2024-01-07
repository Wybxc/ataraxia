//! Terms and formulas of the LCF calculus.

use std::{collections::HashMap, fmt::Display, sync::Arc};

use arcstr::ArcStr;
use eyre::{ensure, OptionExt, Result};
use imbl::OrdSet;
use once_cell::sync::Lazy;
use parking_lot::Mutex;

/// A type in the LCF calculus.
///
/// # Semantics
/// - `basic(name)` is a basic type with the given name.
///     - A special case is `bool`, which is built in the core logic.
/// - `func(arg, ret)` is a function type from `arg` to `ret`.
#[derive(Debug, Clone, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub struct Type(Arc<TypeImpl>);

#[derive(Debug, Clone, Hash, PartialOrd, Ord, PartialEq, Eq)]
enum TypeImpl {
    Basic { name: ArcStr },
    Func { arg: Type, ret: Type },
}

impl From<Arc<TypeImpl>> for Type {
    fn from(arc: Arc<TypeImpl>) -> Self {
        Self(arc)
    }
}

impl AsRef<TypeImpl> for Type {
    fn as_ref(&self) -> &TypeImpl {
        &self.0
    }
}

impl Type {
    /// Create or retrieve a basic type.
    pub fn basic(name: impl Into<ArcStr>) -> Self {
        static STORE: Lazy<Mutex<HashMap<ArcStr, Type>>> = Lazy::new(|| Mutex::new(HashMap::new()));
        let name = name.into();
        let mut store = STORE.lock();
        store
            .entry(name.clone())
            .or_insert_with(|| Arc::new(TypeImpl::Basic { name }).into())
            .clone()
    }

    /// Create a function type.
    pub fn func(arg: Self, ret: Self) -> Self {
        Arc::new(TypeImpl::Func { arg, ret }).into()
    }

    /// Get the name of a basic type. If the type is not basic, return `None`.
    pub fn name(&self) -> Option<&ArcStr> {
        match self.as_ref() {
            TypeImpl::Basic { name } => Some(name),
            _ => None,
        }
    }

    /// Get the argument type of a function type. If the type is not a function,
    /// return `None`.
    pub fn arg(&self) -> Option<&Type> {
        match self.as_ref() {
            TypeImpl::Func { arg, .. } => Some(arg),
            _ => None,
        }
    }

    /// Get the return type of a function type. If the type is not a function,
    /// return `None`.
    pub fn ret(&self) -> Option<&Type> {
        match self.as_ref() {
            TypeImpl::Func { ret, .. } => Some(ret),
            _ => None,
        }
    }
}

impl Display for TypeImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeImpl::Basic { name } => write!(f, "{}", name),
            TypeImpl::Func { arg, ret } => write!(f, "({} -> {})", arg, ret),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

/// A term in the LCF calculus.
///
/// # Semantics
/// - `var(name, ty)` is a variable with the given name and type.
/// - `abs(arg, arg_ty, body)` is an abstraction over the given argument with the
///   given type, with the given body.
/// - `app(func, arg)` is an application of the given function to the given
///   argument.
///
/// There are several built-in terms:
/// - `tt` of type `bool` is the true value.
/// - `ff` of type `bool` is the false value.
/// - `uu(ty)` of type `ty` is the bottom value, usually used to represent
///   undefined.
/// - `ite(ty)` of type `bool -> ty -> ty -> ty` is the if-then-else function.
/// - `fix(ty)` of type `(ty -> ty) -> ty` is the fixed-point combinator.
///
/// # Notes
/// This type derives [`PartialOrd`] and [`Ord`] for convenience, but the ordering
/// is not meaningful. It is only used to give formulas a display order.
#[derive(Debug, Clone, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub struct Term(Arc<TermImpl>);

#[derive(Debug, Clone, Hash, PartialOrd, Ord, PartialEq, Eq)]
enum TermImpl {
    Var {
        name: ArcStr,
        ty: Type,
    },
    Abstract {
        arg: ArcStr,
        arg_ty: Type,
        body: Term,
    },
    Apply {
        func: Term,
        arg: Term,
    },
}

impl From<Arc<TermImpl>> for Term {
    fn from(arc: Arc<TermImpl>) -> Self {
        Self(arc)
    }
}

impl AsRef<TermImpl> for Term {
    fn as_ref(&self) -> &TermImpl {
        &self.0
    }
}

impl Term {
    /// Create or retrieve a variable.
    pub fn var(name: impl Into<ArcStr>, ty: Type) -> Self {
        static STORE: Lazy<Mutex<HashMap<(ArcStr, Type), Term>>> =
            Lazy::new(|| Mutex::new(HashMap::new()));
        let name = name.into();
        let mut store = STORE.lock();
        store
            .entry((name.clone(), ty.clone()))
            .or_insert_with(|| Arc::new(TermImpl::Var { name, ty }).into())
            .clone()
    }

    /// Create an abstraction.
    pub fn abs(arg: impl Into<ArcStr>, arg_ty: Type, body: Self) -> Self {
        let arg = arg.into();
        Arc::new(TermImpl::Abstract { arg, arg_ty, body }).into()
    }

    /// Create an application.
    pub fn app(func: Self, arg: Self) -> Result<Self> {
        let arg_ty = func.ty().arg().ok_or_eyre("expected function type")?;
        ensure!(arg.ty() == arg_ty, "argument type mismatch");
        Ok(Arc::new(TermImpl::Apply { func, arg }).into())
    }

    /// Get the name of the variable. If the term is not a variable, return `None`.
    pub fn name(&self) -> Option<&ArcStr> {
        match self.as_ref() {
            TermImpl::Var { name, .. } => Some(name),
            _ => None,
        }
    }

    /// Get the type of the term.
    pub fn ty(&self) -> &Type {
        match self.as_ref() {
            TermImpl::Var { ty, .. } => ty,
            TermImpl::Abstract { arg_ty, .. } => arg_ty,
            TermImpl::Apply { func, .. } => match func.ty().as_ref() {
                TypeImpl::Func { ret, .. } => ret,
                _ => panic!("expected function type"),
            },
        }
    }

    /// Display the term.
    pub fn display(&self, show_types: bool) -> impl Display + '_ {
        struct DisplayTerm<'a>(&'a Term, bool);

        impl Display for DisplayTerm<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self.0.as_ref() {
                    TermImpl::Var { name, ty } => {
                        if self.1 {
                            write!(f, "({}:{})", name, ty)
                        } else {
                            write!(f, "{}", name)
                        }
                    }
                    TermImpl::Abstract { arg, arg_ty, body } => {
                        if self.1 {
                            write!(f, "(λ{}:{}.{})", arg, arg_ty, DisplayTerm(body, self.1))
                        } else {
                            write!(f, "(λ{}.{})", arg, DisplayTerm(body, self.1))
                        }
                    }
                    TermImpl::Apply { func, arg } => {
                        write!(
                            f,
                            "({} {})",
                            DisplayTerm(func, self.1),
                            DisplayTerm(arg, self.1)
                        )
                    }
                }
            }
        }

        DisplayTerm(self, show_types)
    }
}

/// A formula in the LCF calculus.
///
/// # Semantics
/// A formula consisting of `(left, right)` means the value of `left` is less than
/// or equal to the value of `right`. More precisely, they are related by the
/// partial order defined by the domain chosen for the calculus.
///
/// The type of `left` and `right` must be the same.
#[derive(Debug, Clone, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub struct Formula {
    pub left: Term,
    pub right: Term,
}

impl Formula {
    /// Create a formula.
    pub fn new(left: Term, right: Term) -> Result<Self> {
        ensure!(left.ty() == right.ty(), "type mismatch");
        Ok(Self { left, right })
    }

    /// Display the formula.
    pub fn display(&self, show_types: bool) -> impl Display + '_ {
        struct DisplayFormula<'a>(&'a Formula, bool);

        impl Display for DisplayFormula<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{} ⊑ {}",
                    self.0.left.display(self.1),
                    self.0.right.display(self.1)
                )
            }
        }

        DisplayFormula(self, show_types)
    }
}

/// A set of formulas.
///
/// # Semantics
/// A set of formulas means a "conjunction" of the formulas in the set.
/// The word "conjunction" is in quotes because the core logic does not
/// have conjunctions, but the semantics of a set of formulas is the same
/// as the semantics of a conjunction.
///
/// Empty sets are allowed, and are considered equivalent to `tt`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormulaSet {
    pub formlas: OrdSet<Formula>,
}

impl FormulaSet {
    /// Create a formula set.
    pub fn new(formlas: OrdSet<Formula>) -> Self {
        Self { formlas }
    }

    /// A formula that represents equality between two terms, that is, the
    /// terms are related by the partial order in both directions.
    pub fn equiv(t1: Term, t2: Term) -> Result<Self> {
        let f1 = Formula::new(t1.clone(), t2.clone())?;
        let f2 = Formula::new(t2, t1)?;
        Ok(Self::from_iter([f1, f2]))
    }

    /// An empty formula set.
    pub fn empty() -> Self {
        Self::new(OrdSet::new())
    }

    /// A formula set that contains a single formula.
    pub fn unit(f: Formula) -> Self {
        Self::new(OrdSet::unit(f))
    }

    /// Check if the formula set contains the given formula.
    pub fn has(&self, formula: &Formula) -> bool {
        self.formlas.contains(formula)
    }

    /// Return the union of two formula sets.
    pub fn union(self, other: Self) -> Self {
        Self {
            formlas: self.formlas.union(other.formlas),
        }
    }

    /// Check if the formula set is a subset of another formula set.
    pub fn is_subset(&self, super_set: &Self) -> bool {
        self.formlas.is_subset(&super_set.formlas)
    }

    /// Return the single formula in the set, if there is exactly one.
    /// Otherwise, return `None`.
    pub fn single(self) -> Option<Formula> {
        if self.formlas.len() == 1 {
            self.formlas.into_iter().next()
        } else {
            None
        }
    }

    /// Display the formula set.
    pub fn display(&self, show_types: bool) -> impl Display + '_ {
        struct DisplayFormulaSet<'a>(&'a FormulaSet, bool);

        impl Display for DisplayFormulaSet<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut first = true;
                for formula in &self.0.formlas {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", formula.display(self.1))?;
                }
                Ok(())
            }
        }

        DisplayFormulaSet(self, show_types)
    }
}

impl From<OrdSet<Formula>> for FormulaSet {
    fn from(formlas: OrdSet<Formula>) -> Self {
        Self { formlas }
    }
}

impl FromIterator<Formula> for FormulaSet {
    fn from_iter<T: IntoIterator<Item = Formula>>(iter: T) -> Self {
        Self::new(OrdSet::from_iter(iter))
    }
}

/// A sentence in the LCF calculus.
///
/// # Semantics
/// A sentence consisting of `(left, right)` means the value of `left` implies
/// the value of `right`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Sentence {
    pub left: FormulaSet,
    pub right: FormulaSet,
}

impl Sentence {
    /// Create a sentence.
    pub fn new(left: FormulaSet, right: FormulaSet) -> Self {
        Self { left, right }
    }

    /// Display the sentence.
    pub fn display(&self, show_types: bool) -> impl Display + '_ {
        struct DisplaySentence<'a>(&'a Sentence, bool);

        impl Display for DisplaySentence<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{} ⊢ {}",
                    self.0.left.display(self.1),
                    self.0.right.display(self.1)
                )
            }
        }

        DisplaySentence(self, show_types)
    }
}
