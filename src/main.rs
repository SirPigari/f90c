use crate::codegen::Backend;
use anyhow::Result;
use cranelift_codegen::settings::{self, Configurable, Flags};
use structopt::StructOpt;

mod ast;
mod cli;
mod codegen;
mod errors;
mod ir;
mod lexer;
mod linker;
mod parser;
mod runtime;
mod sema;
mod utils;

#[cfg(target_os = "windows")]
const OBJDUMP_BYTES: Option<&[u8]> = Some(include_bytes!("bin/objdump.exe"));
#[cfg(not(target_os = "windows"))]
const OBJDUMP_BYTES: Option<&[u8]> = None;

#[derive(Debug, Clone)]
struct BuildArtifact {
    object: std::path::PathBuf,
    defines_modules: Vec<String>,
    uses_modules: Vec<String>,
    has_program: bool,
    link_deps: Vec<std::path::PathBuf>,
}

fn main() -> Result<()> {
    env_logger::init();
    let args = cli::Cli::from_args();

    if args.help {
        utils::print_help();
        return Ok(());
    }

    if args.version {
        let version = env!("CARGO_PKG_VERSION");
        println!("f90c version: {}", version);
        let linker_version = env!("LINKER_VERSION");
        println!("linker version: {}", linker_version);
        return Ok(());
    }

    let args = {
        let mut a = args.clone();
        if a.cmd.is_none() {
            if let Some(first) = a.inputs.get(0) {
                a.cmd = Some(cli::Command::Build {
                    input: first.clone(),
                    out: a.out.clone(),
                    run: false,
                });
            }
        }
        a
    };

    match args.cmd.clone().unwrap_or(cli::Command::Help) {
        cli::Command::Lex { input } => {
            let src = utils::read_file_to_string(&input)?;
            let tokens = lexer::lex(&src);
            for t in tokens {
                println!("{:?}", t);
            }
        }
        cli::Command::Parse { input } => {
            let src = utils::read_file_to_string(&input)?;
            let tokens = lexer::lex(&src);
            match parser::parse_with_src(&src, &tokens, input.to_str().unwrap_or("<unknown>")) {
                Ok(p) => println!("{:#?}", p),
                Err(errs) => {
                    for e in errs {
                        eprintln!("{}", e);
                    }
                    std::process::exit(1);
                }
            }
        }
        cli::Command::Check { input } => {
            let src = utils::read_file_to_string(&input)?;
            let tokens = lexer::lex(&src);
            let settings_from_src = sema::parse_directives(&src);
            let mut settings = settings_from_src;
            settings.wall |= args.wall;
            settings.werror |= args.werror;
            let mut any_err = false;
            let program = match parser::parse_with_src(
                &src,
                &tokens,
                input.to_str().unwrap_or("<unknown>"),
            ) {
                Ok(p) => p,
                Err(errs) => {
                    any_err = true;
                    for e in &errs {
                        eprintln!("{}", e);
                    }
                    crate::ast::Program {
                        name: "<error>".into(),
                        body: vec![],
                    }
                }
            };
            let empty_obj_exports = std::collections::HashSet::new();
            let sema_errs = sema::analyze_with_src(
                &program,
                &src,
                &tokens,
                input.to_str().unwrap_or("<unknown>"),
                &settings,
                &empty_obj_exports,
            );
            if !sema_errs.is_empty() {
                let mut has_error = false;
                for e in &sema_errs {
                    eprintln!("{}", e);
                    if matches!(e.kind, crate::errors::CompileErrorKind::Semantic) {
                        has_error = true;
                    }
                }
                if has_error || settings.werror {
                    std::process::exit(1);
                }
            }
            if sema_errs.is_empty() && !any_err {
                println!("No problems found.");
            }
        }
        cli::Command::Build { input, out, run } => {
            let output = utils::default_object_output_path(&input);
            println!("Compiling file: {}", input.display());
            if !input.exists() {
                eprintln!("Error: Input file does not exist: {}", input.display());
                std::process::exit(1);
            }
            let art = compile_to_object(&input, &output, args.wall, args.werror, &args)?;
            println!("Wrote object: {}", art.object.display());
            if !art.has_program {
                if !args.quiet {
                    println!("No PROGRAM unit found; emitted object file only.");
                }
                return Ok(());
            }
            let exe_out = out.unwrap_or_else(|| utils::default_exe_output_path(&input));
            link_executable(&[art.object.clone()], &exe_out, args.lto)?;
            println!("Linked: {}", exe_out.display());
            let path = exe_out.canonicalize().unwrap();
            let mut path_str = path.display().to_string();
            if path_str.starts_with(r"\\?\") {
                path_str = path_str[4..].to_string();
            }
            path_str = path_str.replace("\\", "/");
            let file_size = utils::file_size(&exe_out)?;
            println!(
                "Compiled {} to '{}' , [{} BYTES, {}]",
                exe_out.display(),
                path_str,
                file_size,
                utils::format_bytes(file_size)
            );
            if run {
                println!("Running executable: {}", exe_out.display());
                println!("-----------------------------------------");
                let code = runtime::run_executable(&exe_out);
                println!("-----------------------------------------");
                println!("{} exited with code {}", exe_out.display(), code);
            }
        }
        cli::Command::EmitObj { input, out } => {
            let art = compile_to_object(&input, &out, args.wall, args.werror, &args)?;
            if !args.quiet {
                println!("Wrote object: {}", art.object.display());
            }
        }
        cli::Command::Run { input } => {
            let src = utils::read_file_to_string(&input)?;
            let tokens = lexer::lex(&src);
            let settings_from_src = sema::parse_directives(&src);
            let mut settings = settings_from_src;
            settings.wall |= args.wall;
            settings.werror |= args.werror;
            let program = match parser::parse_with_src(
                &src,
                &tokens,
                input.to_str().unwrap_or("<unknown>"),
            ) {
                Ok(p) => p,
                Err(errs) => {
                    for e in &errs {
                        eprintln!("{}", e);
                    }
                    crate::ast::Program {
                        name: "<error>".into(),
                        body: vec![],
                    }
                }
            };
            let empty_obj_exports = std::collections::HashSet::new();
            let sema_errs = sema::analyze_with_src(
                &program,
                &src,
                &tokens,
                input.to_str().unwrap_or("<unknown>"),
                &settings,
                &empty_obj_exports,
            );
            if !sema_errs.is_empty() {
                for e in &sema_errs {
                    eprintln!("{}", e);
                }
            }
            runtime::interpret(&program);
        }
        cli::Command::Link { inputs, out } => {
            let mut artifacts = Vec::new();
            for inp in inputs {
                if inp
                    .extension()
                    .and_then(|e| e.to_str())
                    .map(|e| e.eq_ignore_ascii_case("f90"))
                    .unwrap_or(false)
                {
                    let obj_out = utils::default_object_output_path(&inp);
                    let art = compile_to_object(&inp, &obj_out, args.wall, args.werror, &args)?;
                    artifacts.push(art);
                } else {
                    artifacts.push(BuildArtifact {
                        object: inp,
                        defines_modules: vec![],
                        uses_modules: vec![],
                        has_program: false,
                        link_deps: vec![],
                    });
                }
            }
            compute_link_deps(&mut artifacts);
            let mut objects: Vec<std::path::PathBuf> = Vec::new();
            let mut seen = std::collections::HashSet::new();
            for art in &artifacts {
                if seen.insert(art.object.clone()) {
                    objects.push(art.object.clone());
                }
                for d in &art.link_deps {
                    if seen.insert(d.clone()) {
                        objects.push(d.clone());
                    }
                }
            }
            let exe_out = if let Some(o) = out {
                o
            } else {
                if let Some(prog_art) = artifacts.iter().find(|a| a.has_program) {
                    let mut p = prog_art.object.clone();
                    p.set_extension("exe");
                    p
                } else {
                    let mut p = objects
                        .get(0)
                        .cloned()
                        .unwrap_or_else(|| std::path::PathBuf::from("a.obj"));
                    p.set_extension("exe");
                    p
                }
            };
            link_executable(&objects, &exe_out, args.lto)?;
            if !args.quiet {
                println!("Linked: {}", exe_out.display());
            }
        }
        cli::Command::Help => {
            utils::print_help();
        }
    }
    Ok(())
}

fn compile_to_object(
    input: &std::path::Path,
    out: &std::path::Path,
    wall: bool,
    werror: bool,
    args: &cli::Cli,
) -> Result<BuildArtifact> {
    let src = utils::read_file_to_string(input)?;
    let tokens = lexer::lex(&src);
    let settings_from_src = sema::parse_directives(&src);
    let mut settings = settings_from_src;
    settings.wall |= wall;
    settings.werror |= werror;
    let parse_result = parser::parse_with_src(&src, &tokens, input.to_str().unwrap_or("<unknown>"));
    let program = match parse_result {
        Ok(p) => p,
        Err(errs) => {
            for e in &errs {
                eprintln!("{}", e);
            }
            return Err(anyhow::anyhow!("parse failed"));
        }
    };
    let empty_obj_exports = std::collections::HashSet::new();
    let sema_errs = sema::analyze_with_src(
        &program,
        &src,
        &tokens,
        input.to_str().unwrap_or("<unknown>"),
        &settings,
        &empty_obj_exports,
    );
    if !sema_errs.is_empty() {
        let mut has_error = false;
        for e in &sema_errs {
            eprintln!("{}", e);
            if matches!(e.kind, crate::errors::CompileErrorKind::Semantic) {
                has_error = true;
            }
        }
        if has_error || settings.werror {
            return Err(anyhow::anyhow!("compile failed"));
        }
    }
    let lowered = ir::lower_to_ir_with_debug(&program, args.debug_ir)?;
    let defines_modules = lowered.defines_modules.clone();
    let uses_modules = lowered.uses_modules.clone();
    let has_program = lowered.has_program;
    let module = lowered.module;
    let mut flag_builder = settings::builder();
    match args.opt_level.as_str() {
        "0" => {
            flag_builder.set("opt_level", "none").ok();
        }
        "1" => {
            flag_builder.set("opt_level", "speed").ok();
        }
        "2" => {
            flag_builder.set("opt_level", "speed").ok();
            flag_builder.set("enable_gvn", "true").ok();
            flag_builder.set("enable_licm", "true").ok();
        }
        "3" => {
            flag_builder.set("opt_level", "speed").ok();
            flag_builder.set("enable_verifier", "false").ok();
            flag_builder.set("enable_gvn", "true").ok();
            flag_builder.set("enable_licm", "true").ok();
            flag_builder.set("enable_preopt", "true").ok();
            flag_builder.set("enable_postopt", "true").ok();
        }
        "s" => {
            flag_builder.set("opt_level", "speed_and_size").ok();
            flag_builder.set("enable_gvn", "true").ok();
            flag_builder.set("enable_licm", "true").ok();
            flag_builder.set("preserve_frame_pointers", "false").ok();
            flag_builder.set("unwind_info", "false").ok();
        }
        "S" => {
            flag_builder.set("opt_level", "speed_and_size").ok();
            flag_builder.set("enable_gvn", "true").ok();
            flag_builder.set("enable_licm", "true").ok();
            flag_builder.set("enable_preopt", "true").ok();
            flag_builder.set("enable_postopt", "true").ok();
            flag_builder.set("preserve_frame_pointers", "false").ok();
            flag_builder.set("unwind_info", "false").ok();
            flag_builder.set("machine_code_cfg_info", "false").ok();
        }
        "z" => {
            flag_builder.set("opt_level", "speed_and_size").ok();
            flag_builder.set("enable_gvn", "false").ok();
            flag_builder.set("enable_licm", "false").ok();
            flag_builder.set("enable_preopt", "true").ok();
            flag_builder.set("enable_postopt", "true").ok();
            flag_builder.set("enable_unroll_loops", "false").ok();
            flag_builder.set("preserve_frame_pointers", "false").ok();
            flag_builder.set("unwind_info", "false").ok();
            flag_builder.set("machine_code_cfg_info", "false").ok();
        }
        "Z" => {
            flag_builder.set("opt_level", "speed_and_size").ok();
            flag_builder.set("enable_gvn", "false").ok();
            flag_builder.set("enable_licm", "false").ok();
            flag_builder.set("enable_preopt", "true").ok();
            flag_builder.set("enable_postopt", "true").ok();
            flag_builder.set("enable_unroll_loops", "false").ok();
            flag_builder.set("preserve_frame_pointers", "false").ok();
            flag_builder.set("unwind_info", "false").ok();
            flag_builder.set("machine_code_cfg_info", "false").ok();
            flag_builder.set("regalloc_checker", "false").ok();
        }
        _ => {
            flag_builder.set("opt_level", "speed").ok();
        }
    }
    let flags = Flags::new(flag_builder);
    let backend = codegen::cranelift::CraneliftBackend::with_flags(flags);
    let obj = match backend.compile(&module, Some(&tokens)) {
        Ok(o) => o,
        Err(e) => {
            let msg = e.to_string();

            use codespan_reporting::diagnostic::{Diagnostic, Label};
            use codespan_reporting::files::SimpleFile;
            use codespan_reporting::term::termcolor::StandardStream;
            use codespan_reporting::term::{emit, Config};

            let mut stderr =
                StandardStream::stderr(codespan_reporting::term::termcolor::ColorChoice::Auto);
            let file = SimpleFile::new(input.to_str().unwrap_or("<unknown>"), &src);

            let mut labels: Vec<Label<()>> = Vec::new();
            if let Some(span_idx) = msg.find("@SPAN=") {
                let span_part = &msg[span_idx + 6..];
                if let Some(dots) = span_part.find("..") {
                    if let (Ok(s), Ok(ei)) = (
                        span_part[..dots].parse::<usize>(),
                        span_part[dots + 2..]
                            .split_whitespace()
                            .next()
                            .unwrap_or("")
                            .parse::<usize>(),
                    ) {
                        labels.push(Label::primary((), s..ei));
                    }
                }
            }

            let diag = if labels.is_empty() {
                Diagnostic::error().with_message(msg)
            } else {
                Diagnostic::error().with_message(msg).with_labels(labels)
            };

            let _ = emit(&mut stderr, &Config::default(), &file, &diag);
            return Err(anyhow::anyhow!("compile failed"));
        }
    };
    codegen::emitter::write_object_file(&obj, out)?;
    if args.emit_asm {
        let out_str = out.to_string_lossy().to_string();
        let mut printed = false;

        if let Some(bytes) = OBJDUMP_BYTES {
            let mut path = std::env::temp_dir();
            path.push("objdump.exe");

            if !path.exists() {
                std::fs::write(&path, bytes).expect("failed to write embedded objdump");
            }

            if let Ok(o) = std::process::Command::new(&path)
                .arg("-d")
                .arg(&out_str)
                .output()
            {
                println!("\n=== ASSEMBLY (via objdump) ===\n");
                let s = String::from_utf8_lossy(&o.stdout);
                println!("{}", s);
                printed = true;
            }
        } else {
            let objdump = if cfg!(windows) {
                "objdump.exe"
            } else {
                "objdump"
            };

            if which::which(objdump).is_ok() {
                if let Ok(o) = std::process::Command::new(objdump)
                    .arg("-d")
                    .arg(&out_str)
                    .output()
                {
                    println!("\n=== ASSEMBLY (via {}) ===\n", objdump);
                    let s = String::from_utf8_lossy(&o.stdout);
                    println!("{}", s);
                    printed = true;
                }
            }

            if !printed && cfg!(windows) {
                if which::which("dumpbin.exe").is_ok() {
                    if let Ok(o) = std::process::Command::new("dumpbin.exe")
                        .arg("/DISASM")
                        .arg(&out_str)
                        .output()
                    {
                        println!("\n=== ASSEMBLY (via dumpbin) ===\n");
                        let s = String::from_utf8_lossy(&o.stdout);
                        println!("{}", s);
                        printed = true;
                    }
                }
            }
        }

        if !printed {
            eprintln!("Warning: no disassembler (embedded/system objdump or dumpbin) found; cannot emit assembly");
        }
    }
    Ok(BuildArtifact {
        object: out.to_path_buf(),
        defines_modules,
        uses_modules,
        has_program,
        link_deps: Vec::new(),
    })
}
fn compute_link_deps(arts: &mut [BuildArtifact]) {
    use std::collections::HashMap;
    let mut mod_map: HashMap<String, std::path::PathBuf> = HashMap::new();
    for a in arts.iter() {
        for m in &a.defines_modules {
            mod_map.insert(m.clone(), a.object.clone());
        }
    }
    for a in arts.iter_mut() {
        a.link_deps.clear();
        for u in &a.uses_modules {
            if let Some(p) = mod_map.get(u) {
                if *p != a.object {
                    a.link_deps.push(p.clone());
                }
            }
        }
    }
}

fn link_executable(objects: &[std::path::PathBuf], out: &std::path::Path, lto: bool) -> Result<()> {
    linker::link(objects, out, lto)
}
