// extract_obj_exports removed — no longer used by the simplified top-level flow.
use anyhow::Result;
use std::{
    fs,
    path::{Path, PathBuf},
};

pub fn read_file_to_string(path: &Path) -> Result<String> {
    Ok(fs::read_to_string(path)?)
}

pub fn default_object_output_path(input: &Path) -> PathBuf {
    let mut out = input.to_path_buf();
    out.set_extension(if cfg!(target_os = "windows") {
        "obj"
    } else {
        "o"
    });
    out
}

pub fn file_size(path: &Path) -> Result<usize> {
    Ok(fs::metadata(path)?.len() as usize)
}

pub fn format_bytes(bytes: usize) -> String {
    if bytes < 1000 {
        format!("{} B", bytes)
    } else if bytes < 1_000_000 {
        format!("{:.2} KB", bytes as f64 / 1000.0)
    } else if bytes < 1_000_000_000 {
        format!("{:.2} MB", bytes as f64 / 1_000_000.0)
    } else if bytes < 1_000_000_000_000 {
        format!("{:.2} GB", bytes as f64 / 1_000_000_000.0)
    } else {
        format!("{:.2} TB", bytes as f64 / 1_000_000_000_000.0)
    }
}

pub fn print_help() {
    println!("Usage: f90c <COMMAND> [OPTIONS] [ARGS]");
    println!("\nCommands:");
    println!("  lex <input>          Lex the input file and print tokens");
    println!("  parse <input>        Parse the input file and print the AST");
    println!("  check <input>        Check the input file for semantic errors");
    println!("  build <input>        Compile the input file to an object file and link it into an executable");
    println!("  emit-obj <input>     Compile the input file to an object file");
    println!("  run <input>          Parse, check, and run the input file");
    println!("  link <inputs>        Link object files or sources into an executable");
    println!("\nOptions:");
    println!("  --wall               Enable all warnings");
    println!("  --werror             Treat warnings as errors");
    println!("  --lto                Enable link-time optimization");
    println!("  --opt-level <level>  Set optimization level (0, 1, 2, 3, s, z, S, Z)");
    println!("  --quiet              Suppress all output except errors");
    println!("  --help               Show this help message");
}

pub fn default_exe_output_path(input: &Path) -> PathBuf {
    let mut out = input.to_path_buf();
    if cfg!(target_os = "windows") {
        out.set_extension("exe");
    } else {
        if let Some(stem) = input.file_stem() {
            out = input.with_file_name(stem);
        }
    }
    out
}
