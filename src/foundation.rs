use std::collections::HashMap;

use smol_str::SmolStr;

use crate::{fusion::terms::Term, parser::Affix};

pub struct Foundation {
    operators: HashMap<SmolStr, Affix>,
    constants: HashMap<SmolStr, Term>,
}

impl Foundation {
    pub fn new() -> Self {
        Self {
            operators: HashMap::new(),
            constants: HashMap::new(),
        }
    }

    pub fn affix(&self, op: &str) -> Option<Affix> { self.operators.get(op).copied() }

    pub fn is_constant(&self, name: &str) -> bool { self.constants.contains_key(name) }
}
