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

#[derive(Debug, Clone)]
struct BuildArtifact {
    object: std::path::PathBuf,
    defines_modules: Vec<String>,
    uses_modules: Vec<String>,
    has_program: bool,
    module_only: bool,
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

    if args.cmd.is_none() && !args.inputs.is_empty() {
        if handle_top_level_invocation(&args)? {
            return Ok(());
        }
    }

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
                        module_only: false,
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
fn handle_top_level_invocation(args: &cli::Cli) -> Result<bool> {
    use std::collections::{HashMap, HashSet};
    if args.inputs.is_empty() {
        return Ok(false);
    }
    struct Prescan {
        src: String,
        tokens: Vec<crate::lexer::Token>,
        program: crate::ast::Program,
        path: std::path::PathBuf,
    }
    let mut prescans: Vec<Prescan> = Vec::new();
    for inp in &args.inputs {
        if inp
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| e.eq_ignore_ascii_case("f90"))
            .unwrap_or(false)
        {
            let src = utils::read_file_to_string(inp)?;
            let tokens = lexer::lex(&src);
            match parser::parse_with_src(&src, &tokens, inp.to_str().unwrap_or("<unknown>")) {
                Ok(p) => prescans.push(Prescan {
                    src,
                    tokens,
                    program: p,
                    path: inp.clone(),
                }),
                Err(errs) => {
                    for e in errs {
                        eprintln!("{}", e);
                    }
                    return Ok(true);
                }
            }
        }
    }
    let mut module_exports: HashMap<String, HashSet<String>> = HashMap::new();
    for ps in &prescans {
        for stmt in &ps.program.body {
            if let crate::ast::Stmt::Module { name, body } = stmt {
                let set = module_exports
                    .entry(name.to_ascii_lowercase())
                    .or_insert_with(HashSet::new);
                for inner in body {
                    if let crate::ast::Stmt::Function { name: fname, .. } = inner {
                        set.insert(fname.to_ascii_lowercase());
                    }
                    if let crate::ast::Stmt::Subroutine { name: fname, .. } = inner {
                        set.insert(fname.to_ascii_lowercase());
                    }
                }
            }
        }
    }
    let mut artifacts: Vec<BuildArtifact> = Vec::new();
    let mut linked_obj_exports = HashSet::new();
    for inp in &args.inputs {
        if inp
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| e.eq_ignore_ascii_case("obj"))
            .unwrap_or(false)
        {
            if let Ok(exports) = utils::extract_obj_exports(inp) {
                for sym in &exports {
                    linked_obj_exports.insert(sym.to_ascii_lowercase());
                }
            }
        }
    }
    for ps in prescans {
        let uses: Vec<String> = ps
            .program
            .body
            .iter()
            .filter_map(|s| {
                if let crate::ast::Stmt::Use { module } = s {
                    Some(module.to_ascii_lowercase())
                } else {
                    None
                }
            })
            .collect();
        let mut external_funcs: HashSet<String> = HashSet::new();
        for m in uses {
            if let Some(fset) = module_exports.get(&m) {
                for f in fset {
                    external_funcs.insert(f.clone());
                }
            }
        }
        let settings_from_src = sema::parse_directives(&ps.src);
        let mut settings = settings_from_src;
        settings.wall |= args.wall;
        settings.werror |= args.werror;
        let sema_errs = sema::analyze_with_src_ext(
            &ps.program,
            &ps.src,
            &ps.tokens,
            ps.path.to_str().unwrap_or("<unknown>"),
            &settings,
            &external_funcs,
            &linked_obj_exports,
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
                return Ok(true);
            }
        }
        let lowered = ir::lower_to_ir_with_debug(&ps.program, args.debug_ir)?;
        let defines_modules = lowered.defines_modules.clone();
        let uses_modules = lowered.uses_modules.clone();
        let has_program = lowered.has_program;
        let module_only = lowered.module_only;
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
        let obj_bytes = backend.compile(&module)?;
        let out_obj = utils::default_object_output_path(&ps.path);
        codegen::emitter::write_object_file(&obj_bytes, &out_obj)?;
        if !args.quiet {
            println!("Wrote object: {}", out_obj.display());
        }
        artifacts.push(BuildArtifact {
            object: out_obj,
            defines_modules,
            uses_modules,
            has_program,
            module_only,
            link_deps: Vec::new(),
        });
    }
    for inp in &args.inputs {
        if !(inp
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| e.eq_ignore_ascii_case("f90"))
            .unwrap_or(false))
        {
            artifacts.push(BuildArtifact {
                object: inp.clone(),
                defines_modules: vec![],
                uses_modules: vec![],
                has_program: false,
                module_only: false,
                link_deps: vec![],
            });
        }
    }
    if artifacts.is_empty() {
        eprintln!("No inputs provided.");
        return Ok(true);
    }
    let any_program = artifacts.iter().any(|a| a.has_program);
    compute_link_deps(&mut artifacts);
    if !any_program {
        if !args.quiet {
            println!("No PROGRAM unit found; emitted object file(s) only.");
        }
        return Ok(true);
    }
    let mut link_objects: Vec<std::path::PathBuf> = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for a in &artifacts {
        if !a.module_only || a.has_program {
            if seen.insert(a.object.clone()) {
                link_objects.push(a.object.clone());
            }
        }
    }
    let mut depended: HashSet<std::path::PathBuf> = HashSet::new();
    for a in &artifacts {
        for d in &a.link_deps {
            depended.insert(d.clone());
        }
    }
    for a in &artifacts {
        if a.module_only && depended.contains(&a.object) {
            if seen.insert(a.object.clone()) {
                link_objects.push(a.object.clone());
            }
        }
    }
    for a in &artifacts {
        for d in &a.link_deps {
            if seen.insert(d.clone()) {
                link_objects.push(d.clone());
            }
        }
    }
    if link_objects.is_empty() {
        eprintln!("No objects selected for linking.");
        std::process::exit(1);
    }
    let exe_out = if let Some(o) = args.out.clone() {
        o
    } else {
        if let Some(prog_art) = artifacts.iter().find(|a| a.has_program) {
            let mut p = prog_art.object.clone();
            p.set_extension("exe");
            p
        } else {
            let first = &args.inputs[0];
            if first
                .extension()
                .and_then(|e| e.to_str())
                .map(|e| e.eq_ignore_ascii_case("f90"))
                .unwrap_or(false)
            {
                utils::default_exe_output_path(first)
            } else {
                let mut p = first.clone();
                p.set_extension("exe");
                p
            }
        }
    };
    link_executable(&link_objects, &exe_out, args.lto)?;
    if !args.quiet {
        println!("Linked: {}", exe_out.display());
    }
    let path = exe_out.canonicalize().unwrap_or(exe_out.clone());
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
    Ok(true)
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
    let module_only = lowered.module_only;
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
    // run backend compile and translate certain backend errors into codespan diagnostics
    let obj = match backend.compile(&module) {
        Ok(o) => o,
        Err(e) => {
            let msg = e.to_string();
            // expected backend message when loop var undefined: "Undefined loop variable `{}` in DO"
            if msg.starts_with("Undefined loop variable") {
                // extract identifier between backticks if present
                if let Some(start) = msg.find('`') {
                    if let Some(end) = msg[start + 1..].find('`') {
                        let var = &msg[start + 1..start + 1 + end];
                        // find token span for identifier
                        let span = tokens
                            .iter()
                            .find(|t| match &t.kind {
                                crate::lexer::TokenKind::Ident(s) => s.eq_ignore_ascii_case(var),
                                _ => false,
                            })
                            .map(|t| t.span.clone())
                            .unwrap_or(0..0);

                        // emit codespan diagnostic
                        use codespan_reporting::diagnostic::{Diagnostic, Label};
                        use codespan_reporting::files::SimpleFile;
                        use codespan_reporting::term::termcolor::StandardStream;
                        use codespan_reporting::term::{emit, Config};

                        let mut stderr = StandardStream::stderr(
                            codespan_reporting::term::termcolor::ColorChoice::Auto,
                        );
                        let file = SimpleFile::new(input.to_str().unwrap_or("<unknown>"), &src);
                        let diag = Diagnostic::error()
                            .with_message(msg)
                            .with_labels(vec![Label::primary((), span.clone())]);
                        let _ = emit(&mut stderr, &Config::default(), &file, &diag);
                        return Err(anyhow::anyhow!("compile failed"));
                    }
                }
            }
            return Err(e);
        }
    };
    codegen::emitter::write_object_file(&obj, out)?;
    Ok(BuildArtifact {
        object: out.to_path_buf(),
        defines_modules,
        uses_modules,
        has_program,
        module_only,
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
