use eyre::Result;

use crate::fusion::terms::Term;

pub trait TermBuilder {
    fn app(&self, arg: Term) -> Result<Term>;
}

impl TermBuilder for Term {
    fn app(&self, arg: Term) -> Result<Term> { Term::application(self.clone(), arg) }
}
