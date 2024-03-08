//! From https://github.com/segeljakt/pratt

use std::fmt::Debug;

use eyre::{bail, Result};

#[derive(Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
    Neither,
}

#[derive(PartialEq, Eq, PartialOrd, Copy, Clone)]
pub struct Precedence(pub u32);

impl Precedence {
    const fn raise(mut self) -> Precedence {
        self.0 = self.0.saturating_add(1);
        self
    }
    const fn lower(mut self) -> Precedence {
        self.0 = self.0.saturating_sub(1);
        self
    }
    const fn normalize(mut self) -> Precedence {
        self.0 = self.0.saturating_mul(10);
        self
    }
    const fn min() -> Precedence { Precedence(u32::MIN) }
    const fn max() -> Precedence { Precedence(u32::MAX) }
}

#[derive(Copy, Clone)]
pub enum Affix {
    Primary,
    Infix(Precedence, Associativity),
    Prefix(Precedence),
    Postfix(Precedence),
}

pub trait PrattParser<Inputs>
where
    Inputs: Iterator<Item = Self::Input>,
{
    type Input: Debug;
    type Output: Sized;

    fn query(&mut self, input: &Self::Input) -> Result<Affix>;

    fn primary(&mut self, input: Self::Input) -> Result<Self::Output>;

    fn infix(
        &mut self,
        lhs: Self::Output,
        op: Self::Input,
        rhs: Self::Output,
    ) -> Result<Self::Output>;

    fn prefix(&mut self, op: Self::Input, rhs: Self::Output) -> Result<Self::Output>;

    fn postfix(&mut self, lhs: Self::Output, op: Self::Input) -> Result<Self::Output>;

    fn parse(&mut self, inputs: Inputs) -> Result<Self::Output> {
        self.parse_input(&mut inputs.peekable(), Precedence::min())
    }

    fn parse_peekable(
        &mut self,
        inputs: &mut core::iter::Peekable<Inputs>,
    ) -> Result<Self::Output> {
        self.parse_input(inputs, Precedence::min())
    }

    fn parse_input(
        &mut self,
        tail: &mut core::iter::Peekable<Inputs>,
        rbp: Precedence,
    ) -> Result<Self::Output> {
        let Some(head) = tail.next() else {
            bail!("unexpected end of input");
        };
        let info = self.query(&head)?;
        let mut nbp = self.nbp(info);
        let mut node = self.nud(head, tail, info);
        while let Some(head) = tail.peek() {
            let info = self.query(head)?;
            let lbp = self.lbp(info);
            if rbp < lbp && lbp < nbp {
                let head = tail.next().unwrap();
                nbp = self.nbp(info);
                node = self.led(head, tail, info, node?);
            } else {
                break;
            }
        }
        node
    }

    /// Null-Denotation
    fn nud(
        &mut self,
        head: Self::Input,
        tail: &mut core::iter::Peekable<Inputs>,
        info: Affix,
    ) -> Result<Self::Output> {
        match info {
            Affix::Prefix(precedence) => {
                let rhs = self.parse_input(tail, precedence.normalize().lower());
                self.prefix(head, rhs?)
            }
            Affix::Primary => self.primary(head),
            Affix::Postfix(_) => bail!("unexpected postfix operator: {:?}", head),
            Affix::Infix(_, _) => bail!("unexpected infix operator: {:?}", head),
        }
    }

    /// Left-Denotation
    fn led(
        &mut self,
        head: Self::Input,
        tail: &mut core::iter::Peekable<Inputs>,
        info: Affix,
        lhs: Self::Output,
    ) -> Result<Self::Output> {
        match info {
            Affix::Infix(precedence, associativity) => {
                let precedence = precedence.normalize();
                let rhs = match associativity {
                    Associativity::Left => self.parse_input(tail, precedence),
                    Associativity::Right => self.parse_input(tail, precedence.lower()),
                    Associativity::Neither => self.parse_input(tail, precedence.raise()),
                };
                self.infix(lhs, head, rhs?)
            }
            Affix::Postfix(_) => self.postfix(lhs, head),
            Affix::Primary => bail!("unexpected primary: {:?}", head),
            Affix::Prefix(_) => bail!("unexpected prefix operator: {:?}", head),
        }
    }

    //         <lbp>  <rbp>  <nbp> <kind>
    // Primary:  MIN |  MIN |  MAX | nud
    // Prefix:  MIN |   bp |  MAX | nud
    // Postfix:  bp |  MIN |  MAX | led
    // InfixL:   bp |   bp | bp+1 | led
    // InfixR:   bp | bp-1 | bp+1 | led
    // InfixN:   bp |   bp |   bp | led

    /// Left-Binding-Power
    fn lbp(&mut self, info: Affix) -> Precedence {
        match info {
            Affix::Primary => Precedence::min(),
            Affix::Prefix(_) => Precedence::min(),
            Affix::Postfix(precedence) => precedence.normalize(),
            Affix::Infix(precedence, _) => precedence.normalize(),
        }
    }

    /// Next-Binding-Power
    fn nbp(&mut self, info: Affix) -> Precedence {
        match info {
            Affix::Primary => Precedence::max(),
            Affix::Prefix(_) => Precedence::max(),
            Affix::Postfix(_) => Precedence::max(),
            Affix::Infix(precedence, Associativity::Left) => precedence.normalize().raise(),
            Affix::Infix(precedence, Associativity::Right) => precedence.normalize().raise(),
            Affix::Infix(precedence, Associativity::Neither) => precedence.normalize(),
        }
    }
}
