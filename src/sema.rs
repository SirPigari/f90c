use std::collections::{HashMap, HashSet};
use std::str::FromStr;

use crate::ast::{CaseItem, Expr, Program, Stmt, TypeSpec};
use crate::errors::{CompileError, CompileErrorKind};
use crate::lexer::{Token, TokenKind};

#[derive(Debug, Clone, Default)]
pub struct SemaSettings {
    pub allow: HashSet<String>,
    pub deny: HashSet<String>,
    pub error: HashSet<String>,
    pub wall: bool,
    pub werror: bool,
}

pub fn parse_directives(src: &str) -> SemaSettings {
    let mut s = SemaSettings::default();
    for line in src.lines() {
        let l = line.trim();
        if let Some(rest) = l.strip_prefix("!#") {
            let r = rest.trim();
            for (kw, set) in [
                ("allow", &mut s.allow),
                ("deny", &mut s.deny),
                ("error", &mut s.error),
            ] {
                let prefix = format!("{}(", kw);
                if r.starts_with(&prefix) && r.ends_with(')') {
                    let inner = &r[prefix.len()..r.len() - 1];
                    let name = inner.trim().to_ascii_lowercase();
                    set.insert(name);
                }
            }
        }
    }
    s
}

fn warn_or_error(
    kind_name: &str,
    message: String,
    span: std::ops::Range<usize>,
    settings: &SemaSettings,
    stderr: &mut codespan_reporting::term::termcolor::StandardStream,
    file: &codespan_reporting::files::SimpleFile<&str, &str>,
    errors: &mut Vec<CompileError>,
) {
    use codespan_reporting::diagnostic::{Diagnostic, Label};
    use codespan_reporting::term::{emit, Config};
    let kn = kind_name.to_ascii_lowercase();
    // allow list: suppress this warning entirely
    if settings.allow.contains(&kn) {
        return;
    }
    // deny or error -> treat as error
    let treat_as_error =
        settings.werror || settings.error.contains(&kn) || settings.deny.contains(&kn);
    if treat_as_error {
        let diag = Diagnostic::error()
            .with_message(&message)
            .with_labels(vec![Label::primary((), span.clone())]);
        let _ = emit(stderr, &Config::default(), file, &diag);
        errors.push(CompileError::new(CompileErrorKind::Semantic, message, span));
    } else {
        let diag = Diagnostic::warning()
            .with_message(&message)
            .with_labels(vec![Label::primary((), span.clone())]);
        let _ = emit(stderr, &Config::default(), file, &diag);
        errors.push(CompileError::new(CompileErrorKind::Warning, message, span));
    }
}
fn report_error(
    message: &str,
    span: std::ops::Range<usize>,
    stderr: &mut codespan_reporting::term::termcolor::StandardStream,
    file: &codespan_reporting::files::SimpleFile<&str, &str>,
    errors: &mut Vec<CompileError>,
) {
    use codespan_reporting::diagnostic::{Diagnostic, Label};
    use codespan_reporting::term::{emit, Config};
    let diag = Diagnostic::error()
        .with_message(message)
        .with_labels(vec![Label::primary((), span.clone())]);
    let _ = emit(stderr, &Config::default(), file, &diag);
    errors.push(CompileError::new(CompileErrorKind::Semantic, message, span));
}

pub fn analyze_with_src(
    program: &Program,
    src: &str,
    tokens: &[Token],
    filename: &str,
    settings: &SemaSettings,
    linked_obj_exports: &HashSet<String>,
) -> Vec<CompileError> {
    let mut sym: HashMap<String, TypeSpec> = HashMap::new();
    let mut errors: Vec<CompileError> = Vec::new();

    fn find_assign_rhs_span(
        tokens: &[Token],
        name: &str,
        rhs: &Expr,
    ) -> Option<std::ops::Range<usize>> {
        let lname = name.to_ascii_lowercase();
        let mut i = 0usize;
        while i + 2 < tokens.len() {
            if let TokenKind::Ident(id) = &tokens[i].kind {
                if id.eq_ignore_ascii_case(&lname) {
                    if matches!(tokens[i + 1].kind, TokenKind::Eq) {
                        match (rhs, &tokens[i + 2].kind) {
                            (Expr::Str(s), TokenKind::Str(ts)) if s == ts => {
                                return Some(tokens[i + 2].span.clone())
                            }
                            (Expr::IntLit(s), TokenKind::Integer(ts)) if s == ts => {
                                return Some(tokens[i + 2].span.clone())
                            }
                            (Expr::RealLit(s), TokenKind::Float(tf)) if s == tf => {
                                return Some(tokens[i + 2].span.clone())
                            }
                            (Expr::Ident(id2), TokenKind::Ident(tid))
                                if id2.eq_ignore_ascii_case(tid) =>
                            {
                                return Some(tokens[i + 2].span.clone())
                            }
                            (Expr::Logical(b), TokenKind::True) if *b => {
                                return Some(tokens[i + 2].span.clone())
                            }
                            (Expr::Logical(b), TokenKind::False) if !*b => {
                                return Some(tokens[i + 2].span.clone())
                            }
                            _ => {}
                        }
                    }
                }
            }
            i += 1;
        }
        None
    }

    use codespan_reporting::files::SimpleFile;
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
    let mut stderr = StandardStream::stderr(ColorChoice::Auto);
    let file = SimpleFile::new(filename, src);
    for stmt in &program.body {
        if let Stmt::VarDecl { kind, names } = stmt {
            if let TypeSpec::Integer(Some(k)) = kind {
                if !matches!(*k, 1 | 2 | 4 | 8 | 16) {
                    let span = tokens
                        .iter()
                        .position(|t| matches!(t.kind, TokenKind::KwInteger))
                        .map(|i| tokens.get(i + 2).map(|t| t.span.clone()).unwrap_or(0..0))
                        .unwrap_or(0..0);
                    let msg = format!("invalid INTEGER kind {} (allowed: 1, 2, 4, 8, 16)", k);
                    warn_or_error(
                        "invalid_integer_kind",
                        msg,
                        span,
                        settings,
                        &mut stderr,
                        &file,
                        &mut errors,
                    );
                }
            }
            for n in names {
                sym.insert(n.to_ascii_lowercase(), kind.clone());
            }
        }
    }
    let mut external_fn_ret: HashMap<String, TypeSpec> = HashMap::new();
    // Pre-populate common Fortran intrinsics so they are treated as external
    // functions by the semantic analyzer. This prevents "undefined function"
    // errors for standard intrinsics and gives them a reasonable type for
    // type checking. The mapping is conservative and may be refined later.
    let mut known_intrinsics: HashMap<String, TypeSpec> = HashMap::new();
    use TypeSpec::*;
    for name in [
        "abs",
        "aimag",
        "aint",
        "anint",
        "ceiling",
        "cmplx",
        "conjg",
        "dble",
        "dim",
        "dprod",
        "floor",
        "sqrt",
        "exp",
        "log",
        "log10",
        "sin",
        "cos",
        "tan",
        "asin",
        "acos",
        "atan",
        "atan2",
        "sinh",
        "cosh",
        "tanh",
        "scale",
        "set_exponent",
        "nearest",
        "fraction",
        "exponent",
        "spacing",
        "rrspacing",
        "precision",
        "radix",
        "range",
        "tiny",
        "epsilon",
        "huge",
        "max",
        "min",
        "sum",
        "product",
        "dot_product",
        "matmul",
        "maxval",
        "minval",
    ] {
        known_intrinsics.insert(name.to_string(), Real);
    }
    for name in [
        "int", "nint", "ichar", "iachar", "len", "len_trim", "count", "size", "lbound", "ubound",
        "maxloc", "minloc", "index", "scan", "verify",
    ] {
        known_intrinsics.insert(name.to_string(), Integer(None));
    }
    for name in ["all", "any", "allocated", "lge", "lgt", "lle", "llt"] {
        known_intrinsics.insert(name.to_string(), Logical);
    }
    for name in [
        "achar",
        "char",
        "repeat",
        "trim",
        "adjustl",
        "adjustr",
        "reshape",
        "transpose",
        "spread",
        "pack",
        "unpack",
        "merge",
    ] {
        known_intrinsics.insert(name.to_string(), Character(None));
    }
    for name in [
        "btest", "iand", "ibclr", "ibits", "ibset", "ieor", "ior", "ishft", "ishftc", "not", "mod",
        "modulo",
    ] {
        known_intrinsics.insert(name.to_string(), Integer(None));
    }
    // merge into external_fn_ret without overwriting any user-declared ones
    for (k, v) in known_intrinsics {
        external_fn_ret.entry(k).or_insert(v);
    }
    let mut fn_name_set: HashSet<String> = HashSet::new();
    for stmt in &program.body {
        match stmt {
            Stmt::Function { name, .. } | Stmt::Subroutine { name, .. } => {
                fn_name_set.insert(name.to_ascii_lowercase());
            }
            _ => {}
        }
    }
    for stmt in &program.body {
        if let Stmt::VarDecl { kind, names } = stmt {
            for n in names {
                let ln = n.to_ascii_lowercase();
                if fn_name_set.contains(&ln) {
                    external_fn_ret.insert(ln, kind.clone());
                }
            }
        }
    }
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum ParamIntent {
        None,
        In,
        Out,
        InOut,
    }

    #[derive(Clone)]
    struct FnInfo {
        return_type: Option<TypeSpec>,
        is_function: bool,
        params: Vec<String>,
        body: Vec<Stmt>,
    }
    let mut fn_map: HashMap<String, FnInfo> = HashMap::new();
    let mut module_fn_names: HashSet<String> = HashSet::new();
    let mut param_intent_map: HashMap<String, ParamIntent> = HashMap::new();
    let mut program_level_reads: HashSet<String> = HashSet::new();
    fn walk_reads_for_program(e: &Expr, out: &mut HashSet<String>) {
        match e {
            Expr::Ident(id) => {
                out.insert(id.to_ascii_lowercase());
            }
            Expr::Call(_, args) => {
                for a in args {
                    walk_reads_for_program(a, out);
                }
            }
            Expr::Bin(_, l, r) => {
                walk_reads_for_program(l, out);
                walk_reads_for_program(r, out);
            }
            Expr::Un(_, inner) => walk_reads_for_program(inner, out),
            _ => {}
        }
    }
    fn walk_stmt_program_reads(s: &Stmt, out: &mut HashSet<String>) {
        match s {
            Stmt::Assign { value, .. } => walk_reads_for_program(value, out),
            Stmt::Print { items } => {
                for it in items {
                    walk_reads_for_program(it, out);
                }
            }
            Stmt::CallSub { name: _, args } => {
                for a in args {
                    walk_reads_for_program(a, out);
                }
            }
            Stmt::SelectCase {
                expr,
                cases,
                default,
            } => {
                walk_reads_for_program(expr, out);
                for c in cases {
                    for item in &c.items {
                        match item {
                            CaseItem::Range(l, r) => {
                                walk_reads_for_program(l, out);
                                walk_reads_for_program(r, out);
                            }
                            CaseItem::Single(e) => walk_reads_for_program(e, out),
                        }
                    }
                    for st in &c.body {
                        walk_stmt_program_reads(st, out);
                    }
                }
                if let Some(d) = default {
                    for st in d {
                        walk_stmt_program_reads(st, out);
                    }
                }
            }
            Stmt::If {
                cond,
                then_body,
                else_body,
            } => {
                walk_reads_for_program(cond, out);
                for st in then_body {
                    walk_stmt_program_reads(st, out);
                }
                if let Some(eb) = else_body {
                    for st in eb {
                        walk_stmt_program_reads(st, out);
                    }
                }
            }
            Stmt::Do {
                start, end, body, ..
            } => {
                walk_reads_for_program(start, out);
                walk_reads_for_program(end, out);
                for st in body {
                    walk_stmt_program_reads(st, out);
                }
            }
            Stmt::Function { body, .. }
            | Stmt::Subroutine { body, .. }
            | Stmt::Module { body, .. } => {
                for st in body {
                    walk_stmt_program_reads(st, out);
                }
            }
            _ => {}
        }
    }
    for stmt in &program.body {
        walk_stmt_program_reads(stmt, &mut program_level_reads);
    }

    let find_ident_span = |name: &str| -> std::ops::Range<usize> {
        if let Some(t) = tokens.iter().find(|t| match &t.kind {
            TokenKind::Ident(s) => s.eq_ignore_ascii_case(name),
            _ => false,
        }) {
            t.span.clone()
        } else {
            0..0
        }
    };
    let mut type_span_map: HashMap<String, std::ops::Range<usize>> = HashMap::new();
    let find_type_keyword_before = |ident_index: usize| -> Option<std::ops::Range<usize>> {
        let mut j = ident_index;
        while j > 0 {
            j -= 1;
            match tokens[j].kind {
                TokenKind::KwInteger
                | TokenKind::KwReal
                | TokenKind::KwDouble
                | TokenKind::KwCharacter
                | TokenKind::KwLogical => {
                    return Some(tokens[j].span.clone());
                }
                _ => {
                    if matches!(
                        tokens[j].kind,
                        TokenKind::KwProgram
                            | TokenKind::KwFunction
                            | TokenKind::KwSubroutine
                            | TokenKind::KwIf
                            | TokenKind::KwDo
                            | TokenKind::KwEnd
                    ) {
                        break;
                    }
                }
            }
        }
        None
    };
    for stmt in &program.body {
        if let Stmt::VarDecl { kind: _, names } = stmt {
            for n in names {
                let lname = n.to_ascii_lowercase();
                if !type_span_map.contains_key(&lname) {
                    if let Some(tok_idx) = tokens.iter().position(
                        |t| matches!(&t.kind, TokenKind::Ident(s) if s.eq_ignore_ascii_case(n)),
                    ) {
                        if let Some(ts) = find_type_keyword_before(tok_idx) {
                            type_span_map.insert(lname.clone(), ts.clone());
                            if let Some(type_tok_idx) = tokens.iter().position(|t| t.span == ts) {
                                let mut intent_found = ParamIntent::None;
                                let mut i = type_tok_idx + 1;
                                while i < tokens.len() {
                                    match &tokens[i].kind {
                                        TokenKind::DColon => break,
                                        TokenKind::Ident(s) if s.eq_ignore_ascii_case("intent") => {
                                            if i + 3 < tokens.len() {
                                                if matches!(tokens[i + 1].kind, TokenKind::LParen) {
                                                    if let TokenKind::Ident(arg) =
                                                        &tokens[i + 2].kind
                                                    {
                                                        let lower = arg.to_ascii_lowercase();
                                                        if lower == "in" {
                                                            intent_found = ParamIntent::In;
                                                        } else if lower == "out" {
                                                            intent_found = ParamIntent::Out;
                                                        } else if lower == "inout" {
                                                            intent_found = ParamIntent::InOut;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                    i += 1;
                                }
                                for nm in names {
                                    param_intent_map.insert(nm.to_ascii_lowercase(), intent_found);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    for stmt in &program.body {
        if let Stmt::Function {
            name, return_type, ..
        } = stmt
        {
            if return_type.is_some() {
                let lname = name.to_ascii_lowercase();
                if !type_span_map.contains_key(&lname) {
                    if let Some(tok_idx) = tokens.iter().position(
                        |t| matches!(&t.kind, TokenKind::Ident(s) if s.eq_ignore_ascii_case(name)),
                    ) {
                        if let Some(ts) = find_type_keyword_before(tok_idx) {
                            type_span_map.insert(lname, ts);
                        }
                    }
                }
            }
        }
    }

    for stmt in &program.body {
        match stmt {
            Stmt::Function {
                name,
                params,
                return_type,
                body,
                ..
            } => {
                let lname = name.to_ascii_lowercase();
                if fn_map.contains_key(&lname) {
                    let span = type_span_map
                        .get(&lname)
                        .cloned()
                        .unwrap_or(find_ident_span(name));
                    report_error(
                        &format!("duplicate function `{}`", name),
                        span,
                        &mut stderr,
                        &file,
                        &mut errors,
                    );
                } else {
                    fn_map.insert(
                        lname,
                        FnInfo {
                            return_type: return_type.clone(),
                            is_function: true,
                            params: params.clone(),
                            body: body.clone(),
                        },
                    );
                }
            }
            Stmt::Subroutine {
                name, params, body, ..
            } => {
                let lname = name.to_ascii_lowercase();
                if fn_map.contains_key(&lname) {
                    let span = type_span_map
                        .get(&lname)
                        .cloned()
                        .unwrap_or(find_ident_span(name));
                    report_error(
                        &format!("duplicate subroutine `{}`", name),
                        span,
                        &mut stderr,
                        &file,
                        &mut errors,
                    );
                } else {
                    fn_map.insert(
                        lname,
                        FnInfo {
                            return_type: None,
                            is_function: false,
                            params: params.clone(),
                            body: body.clone(),
                        },
                    );
                }
            }
            Stmt::Module { name: _, body } => {
                for inner in body {
                    match inner {
                        Stmt::Function {
                            name,
                            params,
                            return_type,
                            body,
                            ..
                        } => {
                            let lname = name.to_ascii_lowercase();
                            module_fn_names.insert(lname.clone());
                            if fn_map.contains_key(&lname) {
                                let span = type_span_map
                                    .get(&lname)
                                    .cloned()
                                    .unwrap_or(find_ident_span(name));
                                report_error(
                                    &format!("duplicate function `{}`", name),
                                    span,
                                    &mut stderr,
                                    &file,
                                    &mut errors,
                                );
                            } else {
                                fn_map.insert(
                                    lname,
                                    FnInfo {
                                        return_type: return_type.clone(),
                                        is_function: true,
                                        params: params.clone(),
                                        body: body.clone(),
                                    },
                                );
                            }
                        }
                        Stmt::Subroutine {
                            name, params, body, ..
                        } => {
                            let lname = name.to_ascii_lowercase();
                            if fn_map.contains_key(&lname) {
                                let span = type_span_map
                                    .get(&lname)
                                    .cloned()
                                    .unwrap_or(find_ident_span(name));
                                report_error(
                                    &format!("duplicate subroutine `{}`", name),
                                    span,
                                    &mut stderr,
                                    &file,
                                    &mut errors,
                                );
                            } else {
                                fn_map.insert(
                                    lname,
                                    FnInfo {
                                        return_type: None,
                                        is_function: false,
                                        params: params.clone(),
                                        body: body.clone(),
                                    },
                                );
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    fn infer_expr_type(
        e: &Expr,
        sym: &HashMap<String, TypeSpec>,
        fn_map: &HashMap<String, FnInfo>,
        external_fn_ret: &HashMap<String, TypeSpec>,
    ) -> Option<TypeSpec> {
        match e {
            Expr::IntLit(_) => Some(TypeSpec::Integer(None)),
            Expr::RealLit(_) => Some(TypeSpec::Real),
            Expr::Logical(_) => Some(TypeSpec::Logical),
            Expr::Str(_) => Some(TypeSpec::Character(None)),
            Expr::Ident(id) => sym.get(&id.to_ascii_lowercase()).cloned(),
            Expr::Un(_, inner) => infer_expr_type(inner, sym, fn_map, external_fn_ret),
            Expr::Bin(op, l, r) => {
                use crate::ast::BinOp::*;
                match op {
                    Add | Sub | Mul | Div | Pow => {
                        let lt = infer_expr_type(l, sym, fn_map, external_fn_ret);
                        let rt = infer_expr_type(r, sym, fn_map, external_fn_ret);
                        match (lt, rt) {
                            (Some(TypeSpec::Integer(_)), Some(TypeSpec::Integer(_))) => {
                                Some(TypeSpec::Integer(None))
                            }
                            (Some(TypeSpec::DoublePrecision), _)
                            | (_, Some(TypeSpec::DoublePrecision)) => {
                                Some(TypeSpec::DoublePrecision)
                            }
                            (Some(TypeSpec::Real), _) | (_, Some(TypeSpec::Real)) => {
                                Some(TypeSpec::Real)
                            }
                            _ => Some(TypeSpec::Real),
                        }
                    }
                    Eq | Ne | Lt | Gt | Le | Ge | And | Or | Eqv | Neqv => Some(TypeSpec::Logical),
                    Concat => Some(TypeSpec::Character(None)),
                }
            }
            Expr::Call(name, _) => {
                let lname = name.to_ascii_lowercase();
                if let Some(fi) = fn_map.get(&lname) {
                    fi.return_type
                        .clone()
                        .or_else(|| external_fn_ret.get(&lname).cloned())
                        .or(Some(TypeSpec::Integer(None)))
                } else {
                    external_fn_ret
                        .get(&lname)
                        .cloned()
                        .or(Some(TypeSpec::Integer(None)))
                }
            }
        }
    }
    fn match_types(decl: &TypeSpec, found: &TypeSpec) -> bool {
        use TypeSpec::*;
        match (decl, found) {
            (Integer(_), Integer(_)) => true,
            (Real, Integer(_)) => true,
            (DoublePrecision, Integer(_)) | (DoublePrecision, Real) => true,
            (Real, Real) => true,
            (DoublePrecision, DoublePrecision) => true,
            (Logical, Logical) => true,
            (Character(_), Character(_)) => true,
            _ => false,
        }
    }
    fn analyze_fn_body(
        fname: &str,
        is_function: bool,
        declared_ret: &Option<TypeSpec>,
        params: &Vec<String>,
        body: &[Stmt],
        global_sym: &HashMap<String, TypeSpec>,
        fn_map: &HashMap<String, FnInfo>,
        external_fn_ret: &HashMap<String, TypeSpec>,
        param_intent_map: &HashMap<String, ParamIntent>,
        find_ident_span: &dyn Fn(&str) -> std::ops::Range<usize>,
        find_type_keyword_before: &dyn Fn(usize) -> Option<std::ops::Range<usize>>,
        tokens: &[Token],
        errors: &mut Vec<CompileError>,
        stderr: &mut codespan_reporting::term::termcolor::StandardStream,
        file: &codespan_reporting::files::SimpleFile<&str, &str>,
        type_span_map: &HashMap<String, std::ops::Range<usize>>,
        settings: &SemaSettings,
        program_level_reads: &HashSet<String>,
    ) {
        let mut local = global_sym.clone();
        let mut local_declared_names: HashSet<String> = HashSet::new();
        let mut local_param_intent: HashMap<String, ParamIntent> = HashMap::new();
        for p in params.iter() {
            local_param_intent.insert(
                p.to_ascii_lowercase(),
                *param_intent_map
                    .get(&p.to_ascii_lowercase())
                    .unwrap_or(&ParamIntent::None),
            );
        }
        let mut assigned: HashSet<String> = HashSet::new();
        for (p, intent) in &local_param_intent {
            if *intent == ParamIntent::In || *intent == ParamIntent::InOut {
                assigned.insert(p.clone());
            }
        }
        if is_function {
            if let Some(rt) = declared_ret {
                local.insert(fname.to_ascii_lowercase(), rt.clone());
            } else if let Some(rt) = external_fn_ret.get(&fname.to_ascii_lowercase()) {
                local.insert(fname.to_ascii_lowercase(), rt.clone());
            }
        }
        for s in body {
            if let Stmt::VarDecl { kind, names } = s {
                for n in names {
                    let lname = n.to_ascii_lowercase();
                    local.insert(lname.clone(), kind.clone());
                    local_declared_names.insert(lname);
                }
            }
        }
        for s in body {
            if let Stmt::VarDecl { kind: _, names } = s {
                for n in names {
                    if let Some(tok_idx) = tokens.iter().position(|t| match &t.kind {
                        TokenKind::Ident(sid) => sid.eq_ignore_ascii_case(n),
                        _ => false,
                    }) {
                        if let Some(ts) = find_type_keyword_before(tok_idx) {
                            if let Some(type_tok_idx) = tokens.iter().position(|t| t.span == ts) {
                                let mut i = type_tok_idx + 1;
                                while i < tokens.len() {
                                    match &tokens[i].kind {
                                        TokenKind::DColon => break,
                                        TokenKind::Ident(sid)
                                            if sid.eq_ignore_ascii_case("intent") =>
                                        {
                                            let span = type_span_map
                                                .get(&n.to_ascii_lowercase())
                                                .cloned()
                                                .unwrap_or(find_ident_span(n));
                                            warn_or_error("intent_on_local", format!("`intent` attribute on non-dummy/local variable `{}`", n), span, settings, stderr, file, errors);
                                            break;
                                        }
                                        _ => {}
                                    }
                                    i += 1;
                                }
                            }
                        }
                    }
                }
            }
        }
        for (name, intent) in param_intent_map.iter() {
            if *intent != ParamIntent::None {
                let lname = name.to_ascii_lowercase();
                if !params.iter().any(|p| p.eq_ignore_ascii_case(name))
                    && local.contains_key(&lname)
                {
                    let span = type_span_map
                        .get(&lname)
                        .cloned()
                        .unwrap_or(find_ident_span(name));
                    warn_or_error(
                        "intent_on_local",
                        format!("`intent` attribute on non-dummy/local variable `{}`", name),
                        span,
                        settings,
                        stderr,
                        file,
                        errors,
                    );
                }
            }
        }
        let mut has_path = false;
        let mut read_vars: HashSet<String> = HashSet::new();
        for r in program_level_reads.iter() {
            read_vars.insert(r.clone());
        }
        fn inner(
            stmts: &[Stmt],
            fname: &str,
            is_function: bool,
            declared_ret: &Option<TypeSpec>,
            local: &HashMap<String, TypeSpec>,
            fn_map: &HashMap<String, FnInfo>,
            external_fn_ret: &HashMap<String, TypeSpec>,
            find_ident_span: &dyn Fn(&str) -> std::ops::Range<usize>,
            errors: &mut Vec<CompileError>,
            has_path: &mut bool,
            stderr: &mut codespan_reporting::term::termcolor::StandardStream,
            file: &codespan_reporting::files::SimpleFile<&str, &str>,
            type_span_map: &HashMap<String, std::ops::Range<usize>>,
            params: &Vec<String>,
            local_param_intent: &HashMap<String, ParamIntent>,
            assigned: &mut HashSet<String>,
            read_vars: &mut HashSet<String>,
            settings: &SemaSettings,
        ) {
            for st in stmts {
                match st {
                    Stmt::Assign { name, value } => {
                        walk_reads_in_expr(value, read_vars);
                        if let Some(vty) = infer_expr_type(value, local, fn_map, external_fn_ret) {
                            if let Some(dty) = local.get(&name.to_ascii_lowercase()) {
                                if !match_types(dty, &vty) {
                                    let span = match type_span_map.get(&name.to_ascii_lowercase()) {
                                        Some(s) => s.clone(),
                                        None => find_ident_span(name),
                                    };
                                    report_error(
                                        &format!("type mismatch in assignment to `{}`", name),
                                        span,
                                        stderr,
                                        file,
                                        errors,
                                    );
                                }
                            }
                        }
                        let lname = name.to_ascii_lowercase();
                        if let Some(intent) = local_param_intent.get(&lname) {
                            if *intent == ParamIntent::In {
                                let span = match type_span_map.get(&lname) {
                                    Some(s) => s.clone(),
                                    None => find_ident_span(name),
                                };
                                warn_or_error(
                                    "intent_in_assignment",
                                    format!("assignment to INTENT(IN) dummy argument `{}`", name),
                                    span,
                                    settings,
                                    stderr,
                                    file,
                                    errors,
                                );
                            } else {
                                assigned.insert(lname.clone());
                            }
                        }
                        read_vars.remove(&lname);
                        if is_function && name.eq_ignore_ascii_case(fname) {
                            *has_path = true;
                        }
                    }
                    Stmt::Return(expr_opt) => {
                        if let Some(e) = expr_opt {
                            if let Some(rt) = declared_ret.clone().or_else(|| {
                                external_fn_ret.get(&fname.to_ascii_lowercase()).cloned()
                            }) {
                                if let Some(vty) =
                                    infer_expr_type(e, local, fn_map, external_fn_ret)
                                {
                                    if !match_types(&rt, &vty) {
                                        let span =
                                            match type_span_map.get(&fname.to_ascii_lowercase()) {
                                                Some(s) => s.clone(),
                                                None => find_ident_span(fname),
                                            };
                                        report_error(
                                            &format!(
                                                "return type mismatch in function `{}`",
                                                fname
                                            ),
                                            span,
                                            stderr,
                                            file,
                                            errors,
                                        );
                                    }
                                }
                            }
                        }
                        if let Some(e) = expr_opt {
                            walk_reads_in_expr(e, read_vars);
                        }
                        *has_path = true;
                    }
                    Stmt::If {
                        cond,
                        then_body,
                        else_body,
                        ..
                    } => {
                        walk_reads_in_expr(cond, read_vars);
                        inner(
                            then_body,
                            fname,
                            is_function,
                            declared_ret,
                            local,
                            fn_map,
                            external_fn_ret,
                            find_ident_span,
                            errors,
                            has_path,
                            stderr,
                            file,
                            type_span_map,
                            params,
                            local_param_intent,
                            assigned,
                            read_vars,
                            settings,
                        );
                        if let Some(eb) = else_body {
                            inner(
                                eb,
                                fname,
                                is_function,
                                declared_ret,
                                local,
                                fn_map,
                                external_fn_ret,
                                find_ident_span,
                                errors,
                                has_path,
                                stderr,
                                file,
                                type_span_map,
                                params,
                                local_param_intent,
                                assigned,
                                read_vars,
                                settings,
                            );
                        }
                    }
                    Stmt::Do {
                        start,
                        end,
                        body: loop_body,
                        ..
                    } => {
                        walk_reads_in_expr(start, read_vars);
                        walk_reads_in_expr(end, read_vars);
                        inner(
                            loop_body,
                            fname,
                            is_function,
                            declared_ret,
                            local,
                            fn_map,
                            external_fn_ret,
                            find_ident_span,
                            errors,
                            has_path,
                            stderr,
                            file,
                            type_span_map,
                            params,
                            local_param_intent,
                            assigned,
                            read_vars,
                            settings,
                        );
                    }
                    Stmt::Function { .. } | Stmt::Subroutine { .. } => {}
                    Stmt::Print { items } => {
                        for it in items {
                            walk_reads_in_expr(it, read_vars);
                        }
                    }
                    _ => {}
                }
            }
        }
        fn walk_reads_in_expr(e: &Expr, read_vars: &mut HashSet<String>) {
            match e {
                Expr::Ident(id) => {
                    read_vars.insert(id.to_ascii_lowercase());
                }
                Expr::Call(_, args) => {
                    for a in args {
                        walk_reads_in_expr(a, read_vars);
                    }
                }
                Expr::Bin(_, l, r) => {
                    walk_reads_in_expr(l, read_vars);
                    walk_reads_in_expr(r, read_vars);
                }
                Expr::Un(_, inner) => walk_reads_in_expr(inner, read_vars),
                _ => {}
            }
        }

        inner(
            body,
            fname,
            is_function,
            declared_ret,
            &local,
            fn_map,
            external_fn_ret,
            find_ident_span,
            errors,
            &mut has_path,
            stderr,
            file,
            type_span_map,
            params,
            &local_param_intent,
            &mut assigned,
            &mut read_vars,
            settings,
        );
        if is_function && declared_ret.is_some() {
            if !has_path {
                let span = match type_span_map.get(&fname.to_ascii_lowercase()) {
                    Some(s) => s.clone(),
                    None => find_ident_span(fname),
                };
                report_error(
                    &format!(
                        "function `{}` with return type has no RETURN or assignment to its name",
                        fname
                    ),
                    span,
                    stderr,
                    file,
                    errors,
                );
            }
        }
        for (p, intent) in local_param_intent.iter() {
            if *intent == ParamIntent::Out {
                if !assigned.contains(p) {
                    let span = match type_span_map.get(p) {
                        Some(s) => s.clone(),
                        None => find_ident_span(p),
                    };
                    warn_or_error(
                        "intent_out_never_assigned",
                        format!("INTENT(OUT) dummy argument `{}` was never assigned", p),
                        span,
                        settings,
                        stderr,
                        file,
                        errors,
                    );
                }
            }
        }
        for p in params.iter() {
            let lname = p.to_ascii_lowercase();
            if lname == "dummy" {
                // ignore intentionally-unused parameter named 'dummy'
                continue;
            }
            if !read_vars.contains(&lname) {
                let span = find_ident_span(p);
                warn_or_error(
                    "unused_param",
                    format!("parameter `{}` is never read", p),
                    span,
                    settings,
                    stderr,
                    file,
                    errors,
                );
            }
        }
        for name in local_declared_names.iter() {
            let lname = name.to_ascii_lowercase();
            if lname == "dummy" {
                // ignore intentionally-unused local variable named 'dummy'
                continue;
            }
            if !read_vars.contains(&lname) {
                if is_function && name.eq_ignore_ascii_case(fname) {
                    continue;
                }
                warn_or_error(
                    "unused_local",
                    format!("local variable `{}` is never read", name),
                    find_ident_span(name),
                    settings,
                    stderr,
                    file,
                    errors,
                );
            }
        }
    }
    fn check_reads_before_assignment(
        params: &Vec<String>,
        body: &[Stmt],
        param_intent_map: &HashMap<String, ParamIntent>,
        find_ident_span: &dyn Fn(&str) -> std::ops::Range<usize>,
        stderr: &mut codespan_reporting::term::termcolor::StandardStream,
        file: &codespan_reporting::files::SimpleFile<&str, &str>,
        errors: &mut Vec<CompileError>,
        settings: &SemaSettings,
    ) {
        use std::collections::HashSet;
        let mut init_assigned: HashSet<String> = HashSet::new();
        for p in params.iter() {
            match param_intent_map.get(&p.to_ascii_lowercase()) {
                Some(ParamIntent::Out) => { /* unassigned initially */ }
                _ => {
                    init_assigned.insert(p.to_ascii_lowercase());
                }
            }
        }
        fn walk_expr_for_reads_flow(
            e: &Expr,
            cur_assigned: &HashSet<String>,
            param_intent_map: &HashMap<String, ParamIntent>,
            find_ident_span: &dyn Fn(&str) -> std::ops::Range<usize>,
            stderr: &mut codespan_reporting::term::termcolor::StandardStream,
            file: &codespan_reporting::files::SimpleFile<&str, &str>,
            errors: &mut Vec<CompileError>,
            settings: &SemaSettings,
        ) {
            match e {
                Expr::Ident(id) => {
                    let lname = id.to_ascii_lowercase();
                    if let Some(ParamIntent::Out) = param_intent_map.get(&lname) {
                        if !cur_assigned.contains(&lname) {
                            let span = find_ident_span(id);
                            warn_or_error(
                                "intent_out_read_before_assign",
                                format!(
                                    "use of INTENT(OUT) dummy argument `{}` before assignment",
                                    id
                                ),
                                span,
                                settings,
                                stderr,
                                file,
                                errors,
                            );
                        }
                    }
                }
                Expr::Call(_, args) => {
                    for a in args {
                        walk_expr_for_reads_flow(
                            a,
                            cur_assigned,
                            param_intent_map,
                            find_ident_span,
                            stderr,
                            file,
                            errors,
                            settings,
                        );
                    }
                }
                Expr::Bin(_, l, r) => {
                    walk_expr_for_reads_flow(
                        l,
                        cur_assigned,
                        param_intent_map,
                        find_ident_span,
                        stderr,
                        file,
                        errors,
                        settings,
                    );
                    walk_expr_for_reads_flow(
                        r,
                        cur_assigned,
                        param_intent_map,
                        find_ident_span,
                        stderr,
                        file,
                        errors,
                        settings,
                    );
                }
                Expr::Un(_, inner) => walk_expr_for_reads_flow(
                    inner,
                    cur_assigned,
                    param_intent_map,
                    find_ident_span,
                    stderr,
                    file,
                    errors,
                    settings,
                ),
                _ => {}
            }
        }
        fn process_stmts(
            stmts: &[Stmt],
            cur_assigned: &HashSet<String>,
            param_intent_map: &HashMap<String, ParamIntent>,
            find_ident_span: &dyn Fn(&str) -> std::ops::Range<usize>,
            stderr: &mut codespan_reporting::term::termcolor::StandardStream,
            file: &codespan_reporting::files::SimpleFile<&str, &str>,
            errors: &mut Vec<CompileError>,
            settings: &SemaSettings,
        ) -> HashSet<String> {
            use std::collections::HashSet;
            let mut cur = cur_assigned.clone();
            for s in stmts {
                match s {
                    Stmt::Assign { name, value } => {
                        walk_expr_for_reads_flow(
                            value,
                            &cur,
                            param_intent_map,
                            find_ident_span,
                            stderr,
                            file,
                            errors,
                            settings,
                        );
                        cur.insert(name.to_ascii_lowercase());
                    }
                    Stmt::If {
                        cond,
                        then_body,
                        else_body,
                        ..
                    } => {
                        walk_expr_for_reads_flow(
                            cond,
                            &cur,
                            param_intent_map,
                            find_ident_span,
                            stderr,
                            file,
                            errors,
                            settings,
                        );
                        let then_assigned = process_stmts(
                            then_body,
                            &cur,
                            param_intent_map,
                            find_ident_span,
                            stderr,
                            file,
                            errors,
                            settings,
                        );
                        let else_assigned = if let Some(eb) = else_body {
                            process_stmts(
                                eb,
                                &cur,
                                param_intent_map,
                                find_ident_span,
                                stderr,
                                file,
                                errors,
                                settings,
                            )
                        } else {
                            cur.clone()
                        };
                        let mut next = HashSet::new();
                        for p in cur
                            .iter()
                            .chain(then_assigned.iter())
                            .chain(else_assigned.iter())
                        {
                            let p = p.clone();
                            if then_assigned.contains(&p) && else_assigned.contains(&p) {
                                next.insert(p);
                            }
                        }
                        cur = next;
                    }
                    Stmt::Do {
                        start, end, body, ..
                    } => {
                        walk_expr_for_reads_flow(
                            start,
                            &cur,
                            param_intent_map,
                            find_ident_span,
                            stderr,
                            file,
                            errors,
                            settings,
                        );
                        walk_expr_for_reads_flow(
                            end,
                            &cur,
                            param_intent_map,
                            find_ident_span,
                            stderr,
                            file,
                            errors,
                            settings,
                        );
                        let body_assigned = process_stmts(
                            body,
                            &cur,
                            param_intent_map,
                            find_ident_span,
                            stderr,
                            file,
                            errors,
                            settings,
                        );
                        cur = cur.union(&body_assigned).cloned().collect();
                    }
                    Stmt::Return(Some(e)) => {
                        walk_expr_for_reads_flow(
                            e,
                            &cur,
                            param_intent_map,
                            find_ident_span,
                            stderr,
                            file,
                            errors,
                            settings,
                        );
                    }
                    Stmt::Function { body, .. }
                    | Stmt::Subroutine { body, .. }
                    | Stmt::Module { body, .. } => {
                        let _ = process_stmts(
                            body,
                            &cur,
                            param_intent_map,
                            find_ident_span,
                            stderr,
                            file,
                            errors,
                            settings,
                        );
                    }
                    _ => {}
                }
            }
            cur
        }

        let _final = process_stmts(
            body,
            &init_assigned,
            param_intent_map,
            find_ident_span,
            stderr,
            file,
            errors,
            settings,
        );
    }
    for (_fname, info) in fn_map.iter() {
        check_reads_before_assignment(
            &info.params,
            &info.body,
            &param_intent_map,
            &find_ident_span,
            &mut stderr,
            &file,
            &mut errors,
            settings,
        );
    }
    for (fname, info) in fn_map.clone() {
        analyze_fn_body(
            &fname,
            info.is_function,
            &info.return_type,
            &info.params,
            &info.body,
            &sym,
            &fn_map,
            &external_fn_ret,
            &param_intent_map,
            &find_ident_span,
            &find_type_keyword_before,
            tokens,
            &mut errors,
            &mut stderr,
            &file,
            &type_span_map,
            settings,
            &program_level_reads,
        );
    }
    for stmt in &program.body {
        if let Stmt::VarDecl { names, .. } = stmt {
            for n in names {
                let lname = n.to_ascii_lowercase();
                if lname == "dummy" {
                    // intentionally ignore frequently unused placeholder variable 'dummy'
                    continue;
                }
                if fn_map.contains_key(&lname) || external_fn_ret.contains_key(&lname) {
                    continue;
                }
                if !program_level_reads.contains(&lname) {
                    let span = find_ident_span(n);
                    warn_or_error(
                        "unused_program_var",
                        format!("variable `{}` is never used", n),
                        span,
                        settings,
                        &mut stderr,
                        &file,
                        &mut errors,
                    );
                }
            }
        }
    }
    for stmt in &program.body {
        if let Stmt::Assign { name, value } = stmt {
            let lname = name.to_ascii_lowercase();
            if let Some(ts) = sym.get(&lname) {
                match (ts, value) {
                    (TypeSpec::Character(Some(len)), Expr::Str(s)) => {
                        if s.len() > *len {
                            if !settings.allow.contains("character_overflow") {
                                let span =
                                    find_assign_rhs_span(tokens, name, value).unwrap_or(0..0);
                                let msg = format!("string literal length {} exceeds CHARACTER(len={}), will be truncated", s.len(), len);
                                warn_or_error(
                                    "character_overflow",
                                    msg,
                                    span,
                                    settings,
                                    &mut stderr,
                                    &file,
                                    &mut errors,
                                );
                            }
                        }
                    }
                    (TypeSpec::Character(Some(dst_len)), Expr::Ident(src_name)) => {
                        if let Some(TypeSpec::Character(Some(src_len))) =
                            sym.get(&src_name.to_ascii_lowercase())
                        {
                            if src_len > dst_len {
                                if !settings.allow.contains("character_overflow") {
                                    let span =
                                        find_assign_rhs_span(tokens, name, value).unwrap_or(0..0);
                                    let msg = format!("assigning CHARACTER(len={}) to CHARACTER(len={}) will truncate", src_len, dst_len);
                                    warn_or_error(
                                        "character_overflow",
                                        msg,
                                        span,
                                        settings,
                                        &mut stderr,
                                        &file,
                                        &mut errors,
                                    );
                                }
                            }
                        }
                    }
                    (TypeSpec::Integer(kind_opt), Expr::IntLit(s)) => match i128::from_str(s) {
                        Ok(v128) => {
                            let bytes = match kind_opt {
                                Some(k) => *k as u32,
                                None => 4,
                            };
                            let bits = 8u32 * bytes;
                            let (min, max) = match bits {
                                8 => (i8::MIN as i128, i8::MAX as i128),
                                16 => (i16::MIN as i128, i16::MAX as i128),
                                32 => (i32::MIN as i128, i32::MAX as i128),
                                64 => (i64::MIN as i128, i64::MAX as i128),
                                128 => (i128::MIN, i128::MAX),
                                _ => (i64::MIN as i128, i64::MAX as i128),
                            };
                            if v128 < min || v128 > max {
                                let span =
                                    find_assign_rhs_span(tokens, name, value).unwrap_or(0..0);
                                let msg = format!(
                                    "integer literal {} does not fit in INTEGER({} byte{})",
                                    s,
                                    bytes,
                                    if bytes == 1 { "" } else { "s" }
                                );
                                warn_or_error(
                                    "integer_overflow",
                                    msg,
                                    span,
                                    settings,
                                    &mut stderr,
                                    &file,
                                    &mut errors,
                                );
                            }
                        }
                        Err(_) => {
                            let span = find_assign_rhs_span(tokens, name, value).unwrap_or(0..0);
                            let msg = format!("invalid integer literal `{}`", s);
                            warn_or_error(
                                "invalid_integer_literal",
                                msg,
                                span,
                                settings,
                                &mut stderr,
                                &file,
                                &mut errors,
                            );
                        }
                    },
                    (TypeSpec::Real, Expr::RealLit(s))
                    | (TypeSpec::DoublePrecision, Expr::RealLit(s)) => match f64::from_str(s) {
                        Ok(_v) => {
                            if let TypeSpec::Real = ts {
                                if settings.wall {
                                    let v = _v;
                                    let rounded = format!("{:.6}", v).parse::<f64>().unwrap_or(v);
                                    if (rounded - v).abs() > 0.0 {
                                        let span = find_assign_rhs_span(tokens, name, value)
                                            .unwrap_or(0..0);
                                        let msg = "real literal exceeds default precision (6 digits), will be rounded".to_string();
                                        warn_or_error(
                                            "real_precision_strip",
                                            msg,
                                            span,
                                            settings,
                                            &mut stderr,
                                            &file,
                                            &mut errors,
                                        );
                                    }
                                }
                            }
                        }
                        Err(_) => {
                            let span = find_assign_rhs_span(tokens, name, value).unwrap_or(0..0);
                            let msg = format!("invalid real literal `{}`", s);
                            warn_or_error(
                                "invalid_real_literal",
                                msg,
                                span,
                                settings,
                                &mut stderr,
                                &file,
                                &mut errors,
                            );
                        }
                    },
                    _ => {}
                }
            }
        }
    }
    if settings.wall {
        for stmt in &program.body {
            if let Stmt::If {
                cond,
                then_body,
                else_body,
            } = stmt
            {
                match cond {
                    Expr::Logical(true) => {
                        if let Some(else_b) = else_body {
                            if !else_b.is_empty() {
                                if let Some(span) = tokens
                                    .iter()
                                    .position(|t| matches!(t.kind, TokenKind::KwIf))
                                    .map(|i| tokens[i].span.clone())
                                {
                                    let msg = "unreachable ELSE branch".to_string();
                                    warn_or_error(
                                        "unreachable_else",
                                        msg,
                                        span,
                                        settings,
                                        &mut stderr,
                                        &file,
                                        &mut errors,
                                    );
                                }
                            }
                        }
                    }
                    Expr::Logical(false) => {
                        if !then_body.is_empty() {
                            if let Some(span) = tokens
                                .iter()
                                .position(|t| matches!(t.kind, TokenKind::KwIf))
                                .map(|i| tokens[i].span.clone())
                            {
                                let msg = "unreachable THEN branch".to_string();
                                warn_or_error(
                                    "unreachable_then",
                                    msg,
                                    span,
                                    settings,
                                    &mut stderr,
                                    &file,
                                    &mut errors,
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    for stmt in &program.body {
        if let Stmt::Assign { name, value } = stmt {
            if let Expr::Call(fname, _) = value {
                let lname_target = name.to_ascii_lowercase();
                if let Some(target_ts) = sym.get(&lname_target) {
                    if let Some(finfo) = fn_map.get(&fname.to_ascii_lowercase()) {
                        if finfo.is_function {
                            let frt = finfo.return_type.clone().or_else(|| {
                                external_fn_ret.get(&fname.to_ascii_lowercase()).cloned()
                            });
                            if let Some(ft) = frt {
                                if !match_types(target_ts, &ft) {
                                    let span = find_ident_span(fname);
                                    let msg = format!(
                                        "type mismatch: assigning result of `{}` to `{}`",
                                        fname, name
                                    );
                                    warn_or_error(
                                        "call_assign_type_mismatch",
                                        msg,
                                        span,
                                        settings,
                                        &mut stderr,
                                        &file,
                                        &mut errors,
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    fn walk_expr_undef<'a>(
        e: &'a Expr,
        fn_map: &HashMap<String, FnInfo>,
        external_fn_ret: &HashMap<String, TypeSpec>,
        undef: &mut Vec<&'a str>,
    ) {
        match e {
            Expr::Call(name, args) => {
                let lname = name.to_ascii_lowercase();
                if !fn_map.contains_key(&lname) && !external_fn_ret.contains_key(&lname) {
                    undef.push(name);
                }
                for a in args {
                    walk_expr_undef(a, fn_map, external_fn_ret, undef);
                }
            }
            Expr::Bin(_, l, r) => {
                walk_expr_undef(l, fn_map, external_fn_ret, undef);
                walk_expr_undef(r, fn_map, external_fn_ret, undef);
            }
            Expr::Un(_, inner) => walk_expr_undef(inner, fn_map, external_fn_ret, undef),
            _ => {}
        }
    }
    fn walk_stmt_undef<'a>(
        s: &'a Stmt,
        fn_map: &HashMap<String, FnInfo>,
        external_fn_ret: &HashMap<String, TypeSpec>,
        undef: &mut Vec<&'a str>,
    ) {
        match s {
            Stmt::Assign { value, .. } => walk_expr_undef(value, fn_map, external_fn_ret, undef),
            Stmt::Return(Some(e)) => walk_expr_undef(e, fn_map, external_fn_ret, undef),
            Stmt::If {
                cond,
                then_body,
                else_body,
            } => {
                walk_expr_undef(cond, fn_map, external_fn_ret, undef);
                for st in then_body {
                    walk_stmt_undef(st, fn_map, external_fn_ret, undef);
                }
                if let Some(eb) = else_body {
                    for st in eb {
                        walk_stmt_undef(st, fn_map, external_fn_ret, undef);
                    }
                }
            }
            Stmt::Do {
                start, end, body, ..
            } => {
                walk_expr_undef(start, fn_map, external_fn_ret, undef);
                walk_expr_undef(end, fn_map, external_fn_ret, undef);
                for st in body {
                    walk_stmt_undef(st, fn_map, external_fn_ret, undef);
                }
            }
            Stmt::CallSub { name, args } => {
                let lname = name.to_ascii_lowercase();
                if !fn_map.contains_key(&lname) && !external_fn_ret.contains_key(&lname) {
                    undef.push(name);
                }
                for a in args {
                    walk_expr_undef(a, fn_map, external_fn_ret, undef);
                }
            }
            Stmt::Function { body, .. }
            | Stmt::Subroutine { body, .. }
            | Stmt::Module { body, .. } => {
                for st in body {
                    walk_stmt_undef(st, fn_map, external_fn_ret, undef);
                }
            }
            _ => {}
        }
    }
    let mut undef_calls: Vec<&str> = Vec::new();
    for stmt in &program.body {
        walk_stmt_undef(stmt, &fn_map, &external_fn_ret, &mut undef_calls);
    }
    use std::collections::HashSet as _HashSet;
    let mut seen_undef = _HashSet::new();
    for name in undef_calls {
        let lname = name.to_ascii_lowercase();
        if linked_obj_exports.contains(&lname) {
            continue;
        }
        if seen_undef.insert(lname.clone()) {
            let span = find_ident_span(name);
            report_error(
                &format!("undefined function or subroutine `{}`", name),
                span,
                &mut stderr,
                &file,
                &mut errors,
            );
        }
    }
    let mut called_fns: HashSet<String> = HashSet::new();
    fn walk_calls<'a>(e: &'a Expr, called: &mut HashSet<String>) {
        match e {
            Expr::Call(name, args) => {
                called.insert(name.to_ascii_lowercase());
                for a in args {
                    walk_calls(a, called);
                }
            }
            Expr::Bin(_, l, r) => {
                walk_calls(l, called);
                walk_calls(r, called);
            }
            Expr::Un(_, inner) => walk_calls(inner, called),
            _ => {}
        }
    }
    fn walk_stmt_calls<'a>(s: &'a Stmt, called: &mut HashSet<String>) {
        match s {
            Stmt::Assign { value, .. } => walk_calls(value, called),
            Stmt::Return(Some(e)) => walk_calls(e, called),
            Stmt::If {
                cond,
                then_body,
                else_body,
            } => {
                walk_calls(cond, called);
                for st in then_body {
                    walk_stmt_calls(st, called);
                }
                if let Some(eb) = else_body {
                    for st in eb {
                        walk_stmt_calls(st, called);
                    }
                }
            }
            Stmt::Do {
                start, end, body, ..
            } => {
                walk_calls(start, called);
                walk_calls(end, called);
                for st in body {
                    walk_stmt_calls(st, called);
                }
            }
            Stmt::Function { body, .. }
            | Stmt::Subroutine { body, .. }
            | Stmt::Module { body, .. } => {
                for st in body {
                    walk_stmt_calls(st, called);
                }
            }
            Stmt::Print { items } => {
                for it in items {
                    walk_calls(it, called);
                }
            }
            Stmt::CallSub { name, args } => {
                called.insert(name.to_ascii_lowercase());
                for a in args {
                    walk_calls(a, called);
                }
            }
            _ => {}
        }
    }
    for stmt in &program.body {
        walk_stmt_calls(stmt, &mut called_fns);
    }
    for (fname, _) in fn_map.iter() {
        if !called_fns.contains(fname) {
            if module_fn_names.contains(fname) {
                continue;
            }
            let span = find_ident_span(fname);
            warn_or_error(
                "unused_function",
                format!("function or subroutine `{}` is never called", fname),
                span,
                settings,
                &mut stderr,
                &file,
                &mut errors,
            );
        }
    }

    errors
}

pub fn analyze_with_src_ext(
    program: &Program,
    src: &str,
    tokens: &[Token],
    filename: &str,
    settings: &SemaSettings,
    external_funcs: &std::collections::HashSet<String>,
    linked_obj_exports: &HashSet<String>,
) -> Vec<CompileError> {
    let mut errs = analyze_with_src(program, src, tokens, filename, settings, linked_obj_exports);
    errs.retain(|e| {
        let msg = &e.message;
        if let Some(rest) = msg.strip_prefix("undefined function or subroutine `") {
            if let Some(name_part) = rest.strip_suffix("`") {
                let lname = name_part.to_ascii_lowercase();
                return !external_funcs.contains(&lname);
            }
        }
        true
    });
    errs
}
