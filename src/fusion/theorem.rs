use once_cell::unsync::Lazy;

use crate::fusion::{terms::Term, types::Type};

/// A theorem.
pub struct Theorem {
    term: Term,
    proof: Proof,
}

impl Theorem {
    /// The term of the theorem.
    pub fn term(&self) -> &Term { &self.term }

    /// The proof of the theorem.
    pub fn proof(&self) -> &Proof { &self.proof }
}

/// A proof.
pub struct Proof {
    term: Term,
}

impl Proof {
    /// Creates a new proof.
    pub fn new(term: Term) -> Self { Self { term } }

    /// The term of the proof.
    pub fn term(&self) -> &Term { &self.term }
}

/// Logic rules
pub struct Rule;

fn eq_refl() -> Term {
    thread_local! {
        static EQ_REFL: Lazy<Term> = Lazy::new(|| {
            let tyvar = Type::anon_var();
            Term::constant(
                "$eq_refl",
                Type::function(tyvar.clone(), Type::function(tyvar, Type::bool())),
            )
        });
    }
    EQ_REFL.with(|eq_refl| Lazy::force(eq_refl).clone())
}

impl Rule {
    /// Reflexivity.
    pub fn reflexivity(term: Term) -> Theorem {
        let thm = Term::equality(term.clone(), term.clone()).unwrap();
        let proof = Term::application(eq_refl(), term).unwrap();
        let proof = Proof::new(proof);
        Theorem { term: thm, proof }
    }
}
