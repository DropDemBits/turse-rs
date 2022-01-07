# Turse - Turing in Rust

Turse is a new Rust interpreter and compiler for the (forsaken) Turing language, and is
intended to be a drop in replacement for TProlog, the older interpreter.

This project uses information from these repositories:

* [OpenTuring](https://github.com/Open-Turing-Project/OpenTuring)
* [turing-deep-doc](https://github.com/DropDemBits/turing-deep-doc)

Currently, only the compiler (`toc`) is being worked on, but a new interpreter will eventually be focused on.

## Why

* Curiosity. Just pure curiosity
* Learning about compilers and bytecode virtual machines
* Learning about concurrency and parallelism
* Learning about JIT and interpreters
* Big projects like this are "fun" to play with
* The current "situation" gives me time to play with

Do not expect this project to be maintained for a long time.

## Building the Compiler

To build the compiler executable:

```shell
git clone https://github.com/DropDemBits/turse-rs.git
cargo build
```

## License

Turse is licensed under the MIT license (see [LICENSE](LICENSE) or <http://opensource.org/licenses/MIT>)
