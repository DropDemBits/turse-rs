# Turse - Turing in Rust

Turse is a new Rust interpreter and compiler for the (forsaken) Turing language, and is
intended to be a drop in replacement for TProlog, the older interpreter.

This project uses information from these repositories:

- [OpenTuring](https://github.com/Open-Turing-Project/OpenTuring)
- [turing-deep-doc](https://github.com/DropDemBits/turing-deep-doc)

Currently, only the compiler (`toc`) is being worked on, but a new interpreter will eventually be focused on.

## Why

- Curiosity. Just pure curiosity
- Learning about compilers and bytecode virtual machines
- Learning about concurrency and parallelism
- Learning about JIT and interpreters
- Big projects like this are "fun" to play with

Do not expect this project to be maintained for a long time.

## Features

Lint mode & the LSP server supports:

- Arrays
- Functions & Procedures
- Pointers
- Sets
- Control flow statements
- Put & get statements

Compilation mode also supports some of thing things linting mode does, with the exception of:

- Arrays
- Set types
- Referencing items not declared in a function or procedure
- Pointers
- String concatenation
- `char(*)` and `string(*)`

Attempting to compile code that does these things _will_ result in a compiler crash.

## Running the Compiler

To run the compiler executable:

```shell
git clone https://github.com/DropDemBits/turse-rs.git
cargo run --bin toc_driver
```

## License

Turse is licensed under the MIT license (see [LICENSE](LICENSE) or <http://opensource.org/licenses/MIT>)
