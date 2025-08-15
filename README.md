# f90c — Single Executable Fortran 90 Compiler in Rust

`f90c` is a single executable Fortran 90 compiler written in Rust.  
It lets you parse, check, compile, and run `.f90` programs all in one binary.

Prebuilt single-file executables for Windows are available in the [GitHub releases](https://github.com/SirPigari/f90c/releases).  
If you want, you can also build `f90c` yourself with Cargo.

---

## Quick Start (Windows / PowerShell)

1. **Run a prebuilt executable**  

   ```powershell
   # Run semantic checks
   f90c.exe check ./tests/01_basic_returns.f90

   # Build and run an executable
   f90c.exe build ./tests/10_subroutine_and_args.f90 -o ./tests/tmp_test

   # Compile to an object file
   f90c.exe emit-obj ./tests/03_real_double.f90 ./tests/03_real_double.obj

   # Run a program directly
   f90c.exe run ./tests/01_basic_returns.f90
   ```

2. **Using `USE` with precompiled objects**  

   If your program uses a module compiled to an object file, pass the object first, followed by your main file:  

   ```powershell
   f90c.exe lib.obj main.f90
   ```

3. **Optional: Build from source**  

   ```powershell
   cargo build
   # The compiled binary will be at target/debug/f90c
   ```

---

## CLI Overview

```
check <file>           Run semantic checks and print diagnostics
build <file>           Compile, emit object, and link to executable
emit-obj <in> <out>    Emit compiled object file
lex <file>             Print lexer tokens (debug)
parse <file>           Print parsed AST (debug)
link <files...>        Link sources or prebuilt objects
```

**Warning control:**

```
--wall    Enable extra warnings
--werror  Treat warnings as errors
```

---

## Running the Test Suite

The test harness (`tests/testsuite.rs`) compiles each `.f90` test and compares output against `tests/expectations.json`.

```powershell
# To record expected outputs
$env:TESTSUITE_RECORD = 1
cargo test -- --nocapture

# Modify the compiler...

$env:TESTSUITE_RECORD = 0

# Run the test harness
cargo test -- --nocapture
```

---

## Developer Notes

- **Semantic Analyzer**: `src/sema.rs` checks for unused variables/functions and intent issues. Warnings can be promoted to errors using `--werror` or source directives like `!#error(...)`.
- **Code Generator**: Produces native object files (COFF on Windows) with plain symbol names. Prebuilt objects can be linked directly.
- **Test Harness**: May create temporary executables (`tests/tmp_test`) during runs; cleanup is automatic.

---

## Contributing

Open a PR with a focused change.  
Run `cargo test` locally and update `tests/expectations.json` only when behavior intentionally changed (use record mode to update expected outputs).  
For questions or diagnostics issues, open an issue with a minimal reproducer under `tests/`.
