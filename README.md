# f90c - Single Executable Fortran 90 Compiler in Rust

`f90c` is a single executable Fortran 90 compiler written in Rust. It can compile, link, and run Fortran 90 sources, supporting object files, optimizations, warnings, and multiple subcommands.

---

## Installation

Download the precompiled binary from the [GitHub releases page](https://github.com/yourusername/f90c/releases) and place it in a directory included in your `PATH`.  

---

## Usage

The general command structure is:

```bash
f90c [subcommand] [links.o (.obj)] [inputs.f90] [options]
```

If no subcommand is provided, all `.f90` inputs are compiled into `.o` (`.obj`) object files. It does **not** run the executable.

---

## Quick Reference

| Subcommand | Description |
|------------|-------------|
| `lex` | Dump tokens of a `.f90` source file |
| `parse` | Dump AST of a `.f90` source file |
| `check` | Run semantic checks only |
| `build` | Compile a `.f90` source into a `.o` (`.obj`) object file |
| `link` | Link `.o` (`.obj`) object files and `.f90` sources into an executable |
| `emitobj` | Emit a `.f90` source as a `.o` (`.obj`) object file to a specific path |
| `run` | Parse, check, and execute a `.f90` source file |
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

```bash
f90c build input.f90 [options]
```

Options:

- `-o`, `--out <path>` – Output object path (defaults next to input with `.o` (`.obj`))  
- `--run` – Run after build

---

### `link`

```bash
f90c link links.o inputs.f90 [options]
```

Options:

- `-o`, `--out <path>` – Output executable path  

> Sources will be compiled first into `.o` (`.obj`) object files if included

---

### `emitobj`

```bash
f90c emitobj input.f90 -o output.o
```

- `input.f90` – Input Fortran 90 source file  
- `output.o` – Output object file path (`.obj` on Windows)

---

### `run`

```bash
f90c run input.f90
```

- `input.f90` – Input Fortran 90 source file

---

### `help`

```bash
f90c help
```

---

## Examples

Compile `.f90` sources into `.o` (`.obj`) object files:

```bash
f90c main.f90 lib1.f90 utils.f90
```

Compile only and generate a `.o` (`.obj`) object file:

```bash
f90c build main.f90 -o main.o
```

Link `.o` (`.obj`) object files into an executable:

```bash
f90c link main.o utils.o -o my_program
```

Enable all warnings and treat them as errors:

```bash
f90c main.f90 --Wall --Werror
```

Run a `.f90` program:

```bash
f90c run main.f90
```

---

## License

This project is licensed under the **[GPLv3 License](LICENSE)**.
