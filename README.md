# PHiLe
## The Painless High-Level Persistence Engine

[![PHiLe on Travis CI](https://api.travis-ci.org/H2CO3/PHiLe.svg)](https://travis-ci.org/H2CO3/PHiLe)
[![PHiLe on crates.io](https://img.shields.io/crates/v/phile.svg)](https://crates.io/crates/phile)
[![PHiLe on docs.rs](https://docs.rs/phile/badge.svg)](https://docs.rs/phile)
[![PHiLe Download](https://img.shields.io/crates/d/phile.svg)](https://crates.io/crates/phile)
[![PHiLe License](https://img.shields.io/badge/license-BSD-blue.svg)](https://github.com/H2CO3/PHiLe/blob/master/LICENSE.txt)
[![Test Coverage](https://codecov.io/gh/H2CO3/PHiLe/branch/master/graphs/badge.svg)](https://codecov.io/gh/H2CO3/PHiLe)
[![Gitter chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/PHiLe-DB)
[![Lines of Code](https://tokei.rs/b1/github/H2CO3/PHiLe)](https://github.com/Aaronepower/tokei)
[![Twitter](https://img.shields.io/badge/twitter-@H2CO3_iOS-blue.svg?style=flat&colorB=64A5DE&label=Twitter)](http://twitter.com/H2CO3_iOS)

### What's PHiLe?

PHiLe (pronounced *fillet*), the Painless High‑Level Persistence Engine, is a next‑generation database management tool and domain‑specific language. It allows application developers to write what is basically a textual form of a conceptual or entity relationship model, from which it automatically generates a Database Abstraction Layer.

Unlike traditional ORMs, it has a strong, expressive type system. It allows programmers to work with many kinds of values, not only objects, therefore trivially eliminating many of the bottlenecks commonly associated with ORM solutions, such as having to retrieve the transitive closure of an object, only to discard all but a few of its scalar attributes.

PHiLe also supports a wide spectrum of different languages and databases, therefore it cooperates well with many already-existing technology stacks. It includes tools for automatically migrating data of the same conceptual schema between different underlying database engines, and another DSL for refactoring the schema without loss of data or type safety.

### Where can I download it?

* PHiLe is on [crates.io](https://crates.io/crates/phile). You can install the compiler by issuing the following command:

    ```
    cargo install philec
    ```

  If you want to use the `libphile` library programmatically, add this to your `Cargo.toml`:

   ```
   [dependencies]
   phile = "0.1.4"
   ```

* You can clone the source at [the official GitHub page](https://github.com/H2CO3/PHiLe/) of the project using:

    ```
    git clone git://github.com/H2CO3/PHiLe.git
    ```

### Is it documented?

Sure! Depending on how you are willing to use PHiLe, you may be looking for…

 * [The Tutorial](https://h2co3.github.io/phile/tutorial). This gets you
   started quickly and painlessly with writing schemas and queries in
   PHiLe's domain-specific language.
 * [The Examples](https://github.com/H2CO3/PHiLe/tree/master/doc/examples).
   Check out these code snippets if you learn easier by example.
 * [The Reference](https://h2co3.github.io/phile/reference).
   Search through this document if you are already familiar with the basics
   and you are now looking for the details of a specific feature.
 * [API documentation](https://docs.rs/phile/) for the `phile` library,
   useful when you want to embed PHiLe into your own Rust programs.
 * [Manpage-style docs for `philec`](https://docs.rs/crate/philec/),
   if you want to deep dive into the invocation of the PHiLe CLI compiler.

### How does it work?

The philosophy behind the design of PHiLe is explained in detail [on my blog](http://h2co3.org/blog/index.php/2017/04/10/join-considered-harmful/).

The PHiLe DSL is a statically‑typed, object-oriented, functional-declarative language, which is capable of describing schemas (like a DDL) and queries/updates (like a DML). It brings the memory model that programmers are familiar with to the world of persistent storage: relational tables become classes or structs, records and documents become objects, and relationships become arrays and pointers.

As rightfully expected from a modern functional language, the PHiLe DSL supports advanced and convenient features such as higher‑order functions, pattern matching, and algebraic data types (including class, struct, tuple, enum and optional types). Enums, in particular, are a type‑safe alternative to stringly‑typed fields, while proper optional types prevent errors related to `NULL` values.

PHiLe will support the following database flavors and programming languages:

#### Database Engines:

* SQLite3
* MySQL / MariaDB
* MongoDB

#### Programming Languages:

* Rust
* C
* C++
* Objective‑C
* Swift
* Go
* JavaScript
* Python
* Ruby
* Java
* C#
* Haskell

Support for other popular storage engines (e.g. Postgres, MS/SQL, Redis, …) and programming languages would be nice and is planned.

### I want to yell at you because it's crap!

* Tweet to me [@H2CO3_iOS](https://twitter.com/H2CO3_iOS)
* Open an issue on [GitHub](https://github.com/H2CO3/PHiLe/)
* Leave a comment [on my blog](http://h2co3.org/blog/)
* Mail me at [h2co3@h2co3.org](h2co3@h2co3.org)
