mod pratt;

use std::rc::Rc;

use eyre::{bail, Result};
pub use pratt::{Affix, PrattParser};
use smol_str::SmolStr;

use crate::{
    foundation::Foundation,
    fusion::{terms::Term, types::Type},
};

#[derive(Debug)]
pub(crate) enum TokenTree {
    Operator(SmolStr),
    Group(Vec<TokenTree>),
    Annotation {
        expr: Box<TokenTree>,
        ty: TypeExpr,
    },
    Name(SmolStr),
    Application {
        func: Box<TokenTree>,
        arg: Vec<TokenTree>,
    },
    Match {
        scrutinee: Box<TokenTree>,
        cases: Vec<(TokenTree, TokenTree)>,
    },
    Function {
        arg: SmolStr,
        body: Box<TokenTree>,
    },
}

#[derive(Debug)]
pub(crate) enum TypeExpr {
    TypeVar(SmolStr),
    Compound(SmolStr, Vec<TypeExpr>),
    Function(Box<TypeExpr>, Box<TypeExpr>),
}

impl TypeExpr {
    fn into_type(self) -> Type {
        match self {
            TypeExpr::TypeVar(name) => Type::var(name),
            TypeExpr::Compound(name, args) => {
                Type::compound(name, args.into_iter().map(Self::into_type).collect::<Vec<_>>())
            }
            TypeExpr::Function(arg, ret) => Type::function(arg.into_type(), ret.into_type()),
        }
    }
}

pub(crate) struct Parser {
    foundation: Rc<Foundation>,
}

impl<I> PrattParser<I> for Parser
where
    I: Iterator<Item = TokenTree>,
{
    type Input = TokenTree;
    type Output = Term;

    fn query(&mut self, input: &TokenTree) -> Result<Affix> {
        let input = match input {
            TokenTree::Operator(op) => op,
            _ => return Ok(Affix::Primary),
        };
        match self.foundation.affix(input) {
            Some(affix) => Ok(affix),
            None => bail!("unknown operator: {:?}", input),
        }
    }

    fn primary(&mut self, input: TokenTree) -> Result<Term> {
        match input {
            TokenTree::Operator(_) => unreachable!(),
            TokenTree::Group(group) => self.parse(group.into_iter()),
            TokenTree::Annotation { expr, ty } => {
                let expr = self.parse(std::iter::once(*expr))?;
                expr.coercion(ty.into_type())
            }
            TokenTree::Name(name) => {
                let ty = Type::anon();
                if self.foundation.is_constant(&name) {
                    Ok(Term::constant(name, ty))
                } else {
                    Ok(Term::var(name, ty))
                }
            }
            TokenTree::Application { .. } => todo!(),
            TokenTree::Match { .. } => todo!(),
            TokenTree::Function { .. } => todo!(),
        }
    }

    fn infix(&mut self, lhs: Term, op: TokenTree, rhs: Term) -> Result<Term> { todo!() }

    fn prefix(&mut self, op: TokenTree, rhs: Term) -> Result<Term> { todo!() }

    fn postfix(&mut self, lhs: Term, op: TokenTree) -> Result<Term> { todo!() }
}

fn append<T>(mut vec: Vec<T>, item: T) -> Vec<T> {
    vec.push(item);
    vec
}

fn concat<T>(mut vec: Vec<T>, other: impl IntoIterator<Item = T>) -> Vec<T> {
    vec.extend(other);
    vec
}
