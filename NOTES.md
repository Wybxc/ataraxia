# Develop notes

## Can we adopt GADTs to give `Term` more type safety?

The answer is no. Consider terms of "any type" or "unknown type", e.g. a single variable `x`.
This is a common case in typed lambda calculus (see details below). We can't give it a type
because we don't know what type it is, and we can't construct a type var that can be converted
into any type later.

<details>
<summary>
More details about the question
</summary>

### Q1: How hard is it to implement GADTs?

GADTs in OCaml (tastes sweet):

```ocaml
type _ term =
  | Int : int -> int term
  | Add : int term * int term -> int term
  | Bool : bool -> bool term
  | If : bool term * 'a term * 'a term -> 'a term
```

GADTs in Rust (tastes spicy):

```rust
trait Term {
  type Type;
}

struct Int(i32);
impl Term for Int {
  type Type = i32;
}

struct Add<T1, T2>(T1, T2);
impl<T1, T2> Term for Add<T1, T2>
where
  T1: Term<Type = i32>,
  T2: Term<Type = i32>,
{
  type Type = i32;
}

struct Bool(bool);
impl Term for Bool {
  type Type = bool;
}

struct If<T1, T2, T3>(T1, T2, T3);
impl<T1, T2, T3> Term for If<T1, T2, T3>
where
  T1: Term<Type = bool>,
  T2: Term,
  T3: Term<Type = T2::Type>,
{
  type Type = T2::Type;
}
```

### Q2: How to design a generic `Term` type?

Usually different logics have different types of terms. For example:

```ocaml
(* Boolean logic *)
type bool_term =
  | BBool : bool -> bool_term
  | BNot : bool_term -> bool_term
  | BAnd : bool_term * bool_term -> bool_term
  | BOr : bool_term * bool_term -> bool_term
  | BImplies : bool_term * bool_term -> bool_term

(* Natural number arithmetic *)
type _ nat_term =
  | NNat : int -> int nat_term
  | NAdd : int nat_term * int nat_term -> int nat_term
  | NBool : bool -> bool nat_term
  | NIf : bool nat_term * 'a nat_term * 'a nat_term -> 'a nat_term
```

If we want to combine them together into a generic form, there is lambda
calculus, while **logic rules are kept outside the kernel**:

```ocaml
type term =
  | Var : string -> term
  | Abs : string * term -> term
  | App : term * term -> term

let add = Abs ("x", Abs ("y", App (App (Var "+", Var "x"), Var "y")))
```

Further more, **typed** lambda calculus:

```ocaml
type _ term = 
  | Var : string -> 'a term
  | Abs : string * 'a term -> ('b -> 'a) term
  | App : ('a -> 'b) term * 'a term -> 'b term
```
</details>