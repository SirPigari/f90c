# f90c - Single Executable Fortran 90 Compiler in Rust

`f90c` is a single executable Fortran 90 compiler written in Rust. It can compile, link, and run Fortran 90 sources, supporting object files, optimizations, warnings, and multiple subcommands.

---

## Installation

Download the precompiled binary from the [GitHub releases page](https://github.com/SirPigari/f90c/releases) and place it in a directory included in your `PATH`.  

---

## Usage

The general command structure is:

```bash
f90c [subcommand] [links.o (.obj)] [inputs.f90] [options]
```

If no subcommand is provided, all `.f90` inputs are compiled into an executable, optionally linking with `.o` (`.obj`) object files.

---

## Quick Reference

| Subcommand | Description |
|------------|-------------|
| `lex` | Dump tokens of a `.f90` source file |
| `parse` | Dump AST of a `.f90` source file |
| `check` | Run semantic checks only |
| `build` | Compile a `.f90` source into an executable |
| `link` | Link `.o` (`.obj`) object files and `.f90` sources into an executable |
| `emitobj` | Emit a `.f90` source as a `.o` (`.obj`) object file |
| `run` | Parse, check, and execute a `.f90` source file via JIT |
| `help` | Show help for `f90c` or subcommands |

### Options

| Flag | Description |
|------|-------------|
| `-o`, `--out <path>` | Output executable or object path. Defaults derived from the first source |
| `--Wall` | Enable all warnings |
| `--Werror` | Treat warnings as errors |
| `--lto` | Enable Link Time Optimization (LTO) for better performance |
| `-O`, `--opt-level <LEVEL>` | Set optimization level: `0`, `1`, `2`, `3`, `s`, `z`, `S`, `Z` (default `2`) |
| `-q`, `--quiet` | Suppress all output except errors |
| `-h`, `--help` | Show help message |
| `-v`, `--version` | Show version information |

---

## Subcommands Details

### `lex`

```bash
f90c lex input.f90
```

- `input.f90` – Input Fortran 90 source file

---

### `parse`

```bash
f90c parse input.f90
```

- `input.f90` – Input Fortran 90 source file

---

### `check`

```bash
f90c check input.f90
```

- `input.f90` – Input Fortran 90 source file

---

### `build`

Compile a `.f90` source into an **executable**.

```bash
f90c build input.f90 [options]
```

Options:

- `-o`, `--out <path>` – Output path for the executable  
- `--run` – Run after build

---

### `link`

Link `.o` (`.obj`) object files and `.f90` sources into an executable.

```bash
f90c link links.o inputs.f90 [options]
```

Options:

- `-o`, `--out <path>` – Output executable path  

> Sources will be compiled first into `.o` (`.obj`) object files if included

---

### `emitobj`

Emit a `.f90` source as a `.o` (`.obj`) object file to a specific path.

```bash
f90c emitobj input.f90 -o output.o
```

- `input.f90` – Input Fortran 90 source file  
- `output.o` – Output object file path (`.obj` on Windows)

---

### `run`

Parse, check, and execute a `.f90` source file via JIT.

```bash
f90c run input.f90
```

- `input.f90` – Input Fortran 90 source file

---

### `help`

Show help for `f90c` or subcommands.

```bash
f90c help
```

---

## Examples

Compile a single `.f90` source into an executable:

```bash
f90c main.f90
```

This produces an executable derived from the first source name.

---

Compile multiple `.f90` sources directly:

```bash
f90c main.f90 file1.f90 file2.f90
```

Each file is compiled **separately** into its own executable.  
This does **not** link them together.

---

Build reusable object files (modules only, no `program`):

```bash
f90c lib1.f90
f90c utils.f90
```

This produces `lib1.o` (`lib1.obj`) and `utils.o` (`utils.obj`).

---

Link a main program with object files:

```bash
f90c main.f90 lib1.o utils.o
```

This compiles `main.f90` and links it with the object files into a single executable.

---

Build an executable with a custom name:

```bash
f90c build main.f90 -o program
```

This compiles `main.f90` into an executable named `program`.

---

Enable all warnings and treat them as errors:

```bash
f90c main.f90 --Wall --Werror
```

---

Run a `.f90` program with JIT:

```bash
f90c run main.f90
```

---

## License

This project is licensed under the **[GPLv3 License](LICENSE)**.
