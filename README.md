# omniml ඞ

> A simple ML featuring constraint-based type inference with suspended constraints

`omniml` is a work-in-progress specification and implementation of a constraint-based type checker for an ML language with constructor and record overloading. It introduces a novel approach by utilising *suspended constraints* to achieve this functionality.

## Getting Started

> [!NOTE]
> `omniml` is built with Nix, a package manager and system configuration tool that makes building from sources easy! See the [Nix docs](https://nixos.org/download/) for instructions for your system. Additionally, ensure [Nix flakes are enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes).


To build `omniml` from source, follow these steps:
```sh
# Clone the repository
❯ git clone https://github.com/johnyob/omniml.git && cd omniml 
# Enter the Nix development environment
❯ nix develop
# Build 🚀
❯ make
```

We strongly recommend using Nix. Nevertheless, `omniml` can be built using `opam` and `dune` directly.
Proceed by creating a fresh opam switch by running the following:
```sh
# Clone the repository
❯ git clone https://github.com/johnyob/omniml.git && cd omniml 
# Create switch 🎛️
❯ opam switch create . --no-install
❯ eval $(opam config env)
# Install dependencies 📦
❯ opam install -y --deps-only --with-test --with-doc .
# Build 🚀
❯ make
```

## Quick Start

To get started with type checking some examples, run the command below:
```sh
❯ dune exec omniml -- type-check examples/test.ml
```

To capture a constraint-solver trace, enable debug events and select an output file:
```sh
❯ dune exec omniml -- type-check -log-level debug -trace-file trace.json examples/power.ml
```

The trace is written synchronously when the command exits. Open `trace.json` in
`chrome://tracing` or [Perfetto](https://ui.perfetto.dev) to inspect nested solver calls
and their structured S-expression fields.

## Commands

For an overview of commands, run:
```
❯ dune exec omniml -- help
omniml

  omniml SUBCOMMAND

=== subcommands ===

  constraint-gen             . Parses [filename] and prints the generated
                               constraint (formatted as a sexp).
  lex                        . Lexes [filename] and prints the tokens.
  parse                      . Parses [filename] and prints the program
                               (formatted as a sexp).
  type-check                 . Type checks [filename].
  version                    . print version information
  help                       . explain a given subcommand (perhaps recursively)
```

## License

This project is licensed under the GNU GPL v3.0 license.
