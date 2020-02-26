---
description: The handbook for the Sqrrl programming language
---

# Getting Started

## Setting up the compiler

Download the compiler from latest release in the sqrrl-lang repository [https://github.com/Aleman778/sqrrl-lang/releases](https://github.com/Aleman778/sqrrl-lang/releases). There is no setup required the release provides an executable for windows. If you are running another operating system you can build the compiler yourself its very easy. For building the compiler look at the following section, if you run the executable then you can skip to the next section.

### Building the compiler

The only requirement for building the compiler is the `Rust` compiler which can easily be installed using the `rustup` tool. First clone the repository or download source from latest release.

```text
git clone https://github.com/Aleman778/sqrrl-lang
```

For building the compiler run the cargo build system preferrably in release mode.

```text
cargo build --release
```

Now the compiler binary is located inside the target folder under release build.

## Running the compiler

Running the compiler is very easy simply run it in the terminal 

```text
sqrrlc main.sq
```

This will bring up the help screen showing all the commands.



