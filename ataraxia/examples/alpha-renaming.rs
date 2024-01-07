//! Alpha-renaming example.
//!
//! ```text
//! |- λx.t ≡ λy.t[y/x]
//! ```
//!
//! Proof:
//! ```text
//! |- λy.(λx.t)(y) ≡ λx.t          (by func)
//! |- (λx.t)(y) ≡ t[y/x]           (by conv)
//! |- λy.(λx.t)(y) ≡ λy.t[y/x]     (by abstr)
//! |- λx.t ≡ λy.t[y/x]             (by trans)
//! ```

use ataraxia::{
    term::{Term, Type},
    theorem::Theorem,
};
use color_eyre::Result;

macro_rules! ty {
    ($t: ident) => {
        Type::basic(stringify!($t))
    };
}

macro_rules! tm {
    ($x: ident : $t: expr) => {
        Term::var(stringify!($x), $t)
    };
    (λ $x: ident : $t: expr => $body: expr) => {
        Term::abs(stringify!($x), $t, $body)
    };
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let t1 = Theorem::func(tm!(λ x: ty!(S) => tm!(t: ty!(T))), "y", ty!(S))?;
    println!("{}", t1.display(true));
    Ok(())
}
