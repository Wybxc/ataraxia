# Furina

A **proof of concept** for an LCF-style proof assistant in system language, e.g. Rust.

The project is currently in a very early stage. The goal is to have a working proof assistant with a small standard
library, and then to add more features and libraries.

## Logic

The logic is based on the simply typed rho-calculus, with a few extensions.

### Types

The type system is a variant of the Higher Order Logic (HOL) system, with minor modifications. 
The recursive definition of the syntax of types is as follows:

1. **Type Variables**: Denoted as `a`, `b`, `c`, etc.
2. **Compound Types**: Represented as `op t1 t2 ...`, where `op` signifies a type operator and `t1`, `t2`, ... are 
types. Notably, compound types without arguments are referred to as **atomic types**, such as `bool`.

The system incorporates several intrinsic type operators:
- `->`: Represents a function type. For instance, `a -> b` corresponds to the type of functions mapping `a` to `b`.
- `bool`: Represents a boolean type.

### Terms

The syntax of terms, defined recursively, includes:

- **Variable**: Represented as `typ x`, where `typ` refers to the type and `x` to the variable name.
- **Constant**: Denoted by `c`.
- **Application**: Expressed as `f x`, indicating the application of `f` to `x`. Here, `f` is a term of type `a -> b` 
and `x` is a term of type `a`.
- **Matching**: Written as `match p1 -> t1 | p2 -> t2 | ...`, it signifies the matching of patterns `p1`, `p2`, ... to 
terms `t1`, `t2`, ... Lambda abstraction, such as `fun x => t`, is a specific instance of matching, equivalent to 
`match x -> t`.

Additionally, there are built-in constants:
- `=>`: Symbolizes material implication and its type is `bool -> bool -> bool`.
- `=`: Represents equality with a type of `a -> a -> bool`.

### Theorem

Theorem types, capable of representing propositions, are defined as follows:
- `bool` qualifies as a theorem type.
- `a -> b` is a theorem type, provided `b` is theorem type.

A theorem is a term of a theorem type that is provable, constructed according to the logic's rules.

Every theorem has a corresponding proof object in Coq, which gives semantics to the theorem. Specifically, the
proof object `[[A]]` of a theorem `A` is given by the recursive rules as follows:
- The proof object of a theorem of type `bool` is given by the primitive logic rules.
- For a theorem `P` with type `a -> b`, `[[P]]` is `fun x => [[P x]]`.

### Rules

#### Assume

```text
|- A -> A
```

#### Reflection

```text
|- A = A
```

#### Abstraction

```text
t1 = t1'  t2 = t2'  ...  tn = tn'
---------------------------------
match p1 -> t1 | p2 -> t2 | ... = match p1 -> t1' | p2 -> t2' | ...
```

#### Application

```text
f = f'  x = x'
--------------
f x = f' x'
```

#### MP

```text
A  A -> B
---------
B
```

#### Rho-reduction

```text
(match p1 -> t1 | p2 -> t2 | ...) x = x[p1/t1; p2/t2; ...]
```
