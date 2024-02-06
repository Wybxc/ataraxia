use crate::core::terms::{Case, Term, TermImpl, Var};

impl Term {
    /// Small-step evaluation, performing a single reduction step.
    ///
    /// If evaluation is not possible, returns `None`.
    pub fn compute_step(&self) -> Option<Term> {
        match &*self.0 {
            TermImpl::Var(_) => None,
            TermImpl::Constant(_) => None,
            TermImpl::Application(app) => {
                if let Some(func) = app.func().compute_step() {
                    return Some(Term::application(func, app.arg().clone()).unwrap());
                } else if let Some(arg) = app.arg().compute_step() {
                    return Some(Term::application(app.func().clone(), arg).unwrap());
                }
                if let Some(func) = app.func().as_matching() {
                    for case in func.cases().iter() {
                        if let Some(term) = case.rewrite(app.arg()) {
                            return Some(term);
                        }
                    }
                }
                None
            }
            TermImpl::Matching(matching) => {
                for (i, case) in matching.cases().iter().enumerate() {
                    if let Some(body) = case.body().compute_step() {
                        let mut cases = matching.cases().clone();
                        cases[i] = Case::new(case.pat().clone(), body).unwrap();
                        return Some(Term::matching(cases).unwrap());
                    }
                }
                None
            }
            TermImpl::Implication(imply) => {
                if let Some(antecedent) = imply.antecedent().compute_step() {
                    return Some(
                        Term::implication(antecedent, imply.consequent().clone()).unwrap(),
                    );
                }
                if let Some(consequent) = imply.consequent().compute_step() {
                    return Some(
                        Term::implication(imply.antecedent().clone(), consequent).unwrap(),
                    );
                }
                None
            }
        }
    }

    /// Full evaluation, performing all possible reduction steps.
    ///
    /// TODO: improve performance
    pub fn compute_full(&self) -> Term {
        let mut term = self.clone();
        while let Some(next) = term.compute_step() {
            term = next;
        }
        term
    }

    /// Substitutes all free occurrences of the given variable with the given
    /// term.
    ///
    /// TODO: substitute multiple variables at once
    fn substitute(&self, var: &Var, term: &Term) -> Term {
        match &*self.0 {
            TermImpl::Var(v) if v == var => term.clone(),
            TermImpl::Var(_) => self.clone(),
            TermImpl::Constant(_) => self.clone(),
            TermImpl::Application(app) => {
                let func = app.func().substitute(var, term);
                let arg = app.arg().substitute(var, term);
                Term::application(func, arg).unwrap()
            }
            TermImpl::Matching(matching) => {
                let mut cases = matching.cases().clone();
                for case in cases.iter_mut() {
                    *case = case.substitute(var, term);
                }
                Term::matching(cases).unwrap()
            }
            TermImpl::Implication(imply) => {
                let antecedent = imply.antecedent().substitute(var, term);
                let consequent = imply.consequent().substitute(var, term);
                Term::implication(antecedent, consequent).unwrap()
            }
        }
    }

    /// Checks if the given variable is free in the pattern.
    /// Assumes that `self` is a pattern.
    fn is_free_var(&self, var: &Var) -> bool {
        match &*self.0 {
            TermImpl::Var(v) if v == var => false,
            TermImpl::Var(_) => true,
            TermImpl::Constant(_) => false,
            TermImpl::Application(app) => app.func().is_free_var(var) || app.arg().is_free_var(var),
            TermImpl::Matching(_) | TermImpl::Implication(_) => unreachable!(),
        }
    }

    /// Matches the term against the given pattern.
    /// Assumes that `self` is a pattern.
    ///
    /// Returns if the term matches the pattern, and if so, adds the variable
    /// bindings to `result`.
    fn matches(&self, term: &Term, result: &mut Vec<(Var, Term)>) -> bool {
        match &*self.0 {
            TermImpl::Var(var) => {
                result.push((var.clone(), term.clone()));
                true
            }
            TermImpl::Constant(c) => c == term.as_constant().unwrap(),
            TermImpl::Application(app) => {
                if let Some(term) = term.as_application() {
                    app.func().matches(term.func(), result) && app.arg().matches(term.arg(), result)
                } else {
                    false
                }
            }
            TermImpl::Matching(_) | TermImpl::Implication(_) => false,
        }
    }
}

impl Case {
    /// Rewrites the case via pattern matching.
    fn rewrite(&self, term: &Term) -> Option<Term> {
        let mut matches = vec![];
        if self.pat().matches(term, &mut matches) {
            let mut result = self.body().clone();
            for (var, term) in matches {
                result = result.substitute(&var, &term);
            }
            Some(result)
        } else {
            None
        }
    }

    /// Substitutes all free occurrences of the given variable with the given
    /// term.
    fn substitute(&self, var: &Var, term: &Term) -> Case {
        if self.pat().is_free_var(var) {
            Case::new(self.pat().clone(), self.body().substitute(var, term)).unwrap()
        } else {
            self.clone()
        }
    }
}
