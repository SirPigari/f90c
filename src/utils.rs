/// Extracts exported symbol names from a Windows COFF object file (.obj)
pub fn extract_obj_exports(
    path: &std::path::Path,
) -> Result<std::collections::HashSet<String>, std::io::Error> {
    use std::collections::HashSet;
    use std::fs::File;
    use std::io::{BufReader, Read};
    let mut exports = HashSet::new();
    let file = File::open(path)?;
    let mut buf = Vec::new();
    BufReader::new(file).read_to_end(&mut buf)?;
    if buf.len() >= 4 && &buf[0..4] == b"\x7fELF" {
        let mut i = 0;
        while i < buf.len() - 1 {
            if buf[i] >= 0x20 && buf[i] <= 0x7e {
                let mut end = i;
                while end < buf.len() && buf[end] >= 0x20 && buf[end] <= 0x7e && (end - i) < 64 {
                    end += 1;
                }
                if end < buf.len() && buf[end] == 0 && end > i + 1 {
                    let name = &buf[i..end];
                    if let Ok(s) = std::str::from_utf8(name) {
                        if s.chars().all(|c| c.is_ascii_graphic() || c == '_')
                            && !s.starts_with(".text")
                            && !s.starts_with(".data")
                            && !s.starts_with(".bss")
                        {
                            exports.insert(s.to_string());
                        }
                    }
                    i = end;
                }
            }
            i += 1;
        }
    } else if buf.len() >= 20 {
        let num_symbols = u32::from_le_bytes([buf[12], buf[13], buf[14], buf[15]]);
        let sym_table_offset = u32::from_le_bytes([buf[8], buf[9], buf[10], buf[11]]) as usize;
        let sym_size = 18;
        for i in 0..num_symbols {
            let off = sym_table_offset + (i as usize) * sym_size;
            if off + sym_size > buf.len() {
                break;
            }
            let name_bytes = &buf[off..off + 8];
            let name = if let Some(pos) = name_bytes.iter().position(|&b| b == 0) {
                &name_bytes[..pos]
            } else {
                name_bytes
            };
            if let Ok(s) = std::str::from_utf8(name) {
                if s.chars().all(|c| c.is_ascii_graphic() || c == '_')
                    && !s.starts_with(".")
                    && !s.eq_ignore_ascii_case(".file")
                    && !s.eq_ignore_ascii_case("f90c")
                {
                    exports.insert(s.to_string());
                }
            }
        }
    }
    Ok(exports)
}
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
    if bytes < 1024 {
        format!("{} B", bytes)
    } else if bytes < 1024 * 1024 {
        format!("{:.2} KB", bytes as f64 / 1024.0)
    } else if bytes < 1024 * 1024 * 1024 {
        format!("{:.2} MB", bytes as f64 / (1024.0 * 1024.0))
    } else if bytes < 1024 * 1024 * 1024 * 1024 {
        format!("{:.2} GB", bytes as f64 / (1024.0 * 1024.0 * 1024.0))
    } else {
        format!(
            "{:.2} TB",
            bytes as f64 / (1024.0 * 1024.0 * 1024.0 * 1024.0)
        )
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
