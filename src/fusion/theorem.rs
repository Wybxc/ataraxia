use crate::fusion::terms::Term;

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

impl Rule {
    /// Reflexivity.
    pub fn reflexivity(term: Term) -> Theorem {
        // let thm = Term::equality(term.clone(), term.clone()).unwrap();
        // let thm = Term::application(Term::constant(""))
        let proof = builtin::eq(term.clone(), term.clone()).unwrap();
        let proof = Proof::new(proof);
        Theorem { term: thm, proof }
    }
}

mod builtin {
    use eyre::Result;

    use crate::fusion::{
        terms::{builder::TermBuilder, Term},
        types::{builder::TypeBuilder, Type},
    };

    pub fn bool() -> Type { Type::constant("bool") }

    pub fn eq_type() -> Type {
        let ty_var = Type::anon();
        ty_var.to(ty_var.to(bool()))
    }

    pub fn eq(left: Term, right: Term) -> Result<Term> {
        Term::constant("=", eq_type()).app(left)?.app(right)
    }
    pub fn eq_refl(left: Term, right: Term) -> Result<Term> {
        Term::constant("$eq_refl", eq_type()).app(left)?.app(right)
    }
}
