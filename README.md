# Erlang Code Generator for `aleph-syntax-tree`

This crate provides a code generator that converts an abstract syntax tree (AST) from the [`aleph-syntax-tree`](https://github.com/aleph-lang/aleph-syntax-tree) crate into Erlang source code.

## Overview

The core function `generate(ast: AlephTree) -> String` takes an `AlephTree` and returns equivalent Erlang code.

This generator supports:
- Basic expressions: `Add`, `Sub`, `Not`, `Let`, `If`, etc.
- Data structures: `Tuple`, `Array`, `Bytes`
- Control flow: `Match`, `While`, `Return`, `Stmts`
- Function definitions: `LetRec`, `App`
- COBOL-like constructs: `ProcedureDivision`, `Perform`, `Accept`, `Display`, etc.

Output is properly indented and aims to be readable and idiomatic Erlang.

## Installation

Add the dependency to your `Cargo.toml`:

```toml
[dependencies]
aleph-syntax-tree = "0.1"
```

## Usage

```rust
use aleph_syntax_tree::syntax::AlephTree;
use your_crate::generate;

fn main() {
    let ast: AlephTree = /* build or parse your AST */;
    let code = generate(ast);
    println!("{}", code);
}
```

## Example

```rust
let ast = AlephTree::Let {
    var: "X".into(),
    is_pointer: false,
    value: Box::new(AlephTree::Int { value: "42".into() }),
    expr: Box::new(AlephTree::Return {
        value: Box::new(AlephTree::Var {
            var: "X".into(),
            is_pointer: false,
        }),
    }),
};
```

Produces Erlang code:

```erlang
X = 42,
X
```

## Related

- [`aleph-syntax-tree`](https://github.com/aleph-lang/aleph-syntax-tree) – The AST definition crate
- [`aleph-syntax-tree` on crates.io](https://crates.io/crates/aleph-syntax-tree)

