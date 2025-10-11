#[derive(Debug, Clone)]
pub struct Module {
    pub funcs: Vec<Function>,
    #[allow(dead_code)]
    pub uses_modules: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<IStmt>,
    pub return_type: Option<crate::ast::TypeSpec>,
}

#[derive(Debug, Clone)]
pub enum IExpr {
    Str(String),
    IntLit(String),
    RealLit(String),
    Logical(bool),
    Ident(String),
    Index(String, Vec<IExpr>),
    Bin(crate::ast::BinOp, Box<IExpr>, Box<IExpr>),
    Un(crate::ast::UnOp, Box<IExpr>),
    Call(String, Vec<IExpr>),
}

#[derive(Debug, Clone)]
pub enum IStmt {
    Print(Vec<IExpr>),

    DeclVar(crate::ast::TypeSpec, String, Vec<IExpr>),
    AssignStr(String, String),
    AssignIntLit(String, String),
    AssignRealLit(String, String),
    AssignBool(String, bool),
    AssignIdent(String, String),
    AssignExpr(String, IExpr),

    AssignIndex(String, Vec<IExpr>, IExpr),
    Return(Option<IExpr>),
    If {
        cond: IExpr,
        then_body: Vec<IStmt>,
        else_body: Option<Vec<IStmt>>,
    },
    Do {
        var: String,
        start: IExpr,
        end: IExpr,
        body: Vec<IStmt>,
    },
    DoWhile {
        cond: IExpr,
        body: Vec<IStmt>,
    },
    Call(String, Vec<IExpr>),
    Read(Vec<IExpr>),
    SelectCase {
        selector: IExpr,
        cases: Vec<IcCase>,
        default: Option<Vec<IStmt>>,
    },
    Block {
        body: Vec<IStmt>,
    },
    Exit,
    Cycle,
}

#[derive(Debug, Clone)]
pub struct IcCase {
    pub items: Vec<IcCaseItem>,
    pub body: Vec<IStmt>,
}

#[derive(Debug, Clone)]
pub enum IcCaseItem {
    Range(IExpr, IExpr),
    Single(IExpr),
    Multi(Vec<IExpr>),
}

#[derive(Debug, Clone)]
pub struct LowerOutput {
    pub module: Module,
    pub defines_modules: Vec<String>,
    pub uses_modules: Vec<String>,
    pub has_program: bool,
}

use crate::ast::{Expr, Program, Stmt};
use anyhow::Result;

impl std::fmt::Display for IExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IExpr::Str(s) => write!(f, "\"{}\"", s),
            IExpr::IntLit(s) => write!(f, "{}", s),
            IExpr::RealLit(s) => write!(f, "{}", s),
            IExpr::Logical(b) => write!(f, "{}", if *b { ".TRUE." } else { ".FALSE." }),
            IExpr::Ident(id) => write!(f, "{}", id),
            IExpr::Index(name, indices) => {
                write!(f, "{}(", name)?;
                for (i, idx) in indices.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", idx)?;
                }
                write!(f, ")")
            }
            IExpr::Bin(op, l, r) => write!(f, "({} {:?} {})", l, op, r),
            IExpr::Un(op, e) => write!(f, "({:?} {})", op, e),
            IExpr::Call(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

fn fmt_istmt(stmt: &IStmt, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
    fn write_indent(f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        if indent > 0 {
            write!(f, "{}", " ".repeat(indent))?;
        }
        Ok(())
    }

    match stmt {
        IStmt::Print(exprs) => {
            write_indent(f, indent)?;
            write!(f, "PRINT(")?;
            for (i, expr) in exprs.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", expr)?;
            }
            writeln!(f, ")")
        }
        IStmt::DeclVar(kind, name, dims) => {
            write_indent(f, indent)?;
            write!(f, "DECL {:?} {}", kind, name)?;
            if !dims.is_empty() {
                write!(f, "(")?;
                for (i, dim) in dims.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", dim)?;
                }
                write!(f, ")")?;
            }
            writeln!(f)
        }
        IStmt::AssignStr(name, s) => {
            write_indent(f, indent)?;
            writeln!(f, "{} = \"{}\"", name, s)
        }
        IStmt::AssignIntLit(name, s) => {
            write_indent(f, indent)?;
            writeln!(f, "{} = {}", name, s)
        }
        IStmt::AssignRealLit(name, s) => {
            write_indent(f, indent)?;
            writeln!(f, "{} = {}", name, s)
        }
        IStmt::AssignBool(name, b) => {
            write_indent(f, indent)?;
            writeln!(f, "{} = {}", name, if *b { ".TRUE." } else { ".FALSE." })
        }
        IStmt::AssignIdent(name, id) => {
            write_indent(f, indent)?;
            writeln!(f, "{} = {}", name, id)
        }
        IStmt::AssignExpr(name, expr) => {
            write_indent(f, indent)?;
            writeln!(f, "{} = {}", name, expr)
        }
        IStmt::AssignIndex(name, indices, expr) => {
            write_indent(f, indent)?;
            write!(f, "{}(", name)?;
            for (i, idx) in indices.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", idx)?;
            }
            writeln!(f, ") = {}", expr)
        }
        IStmt::Return(opt) => {
            write_indent(f, indent)?;
            if let Some(expr) = opt {
                writeln!(f, "RETURN {}", expr)
            } else {
                writeln!(f, "RETURN")
            }
        }
        IStmt::If {
            cond,
            then_body,
            else_body,
        } => {
            write_indent(f, indent)?;
            writeln!(f, "IF ({}) THEN", cond)?;
            for s in then_body {
                fmt_istmt(s, f, indent + 4)?;
            }
            if let Some(else_stmts) = else_body {
                write_indent(f, indent)?;
                writeln!(f, "ELSE")?;
                for s in else_stmts {
                    fmt_istmt(s, f, indent + 4)?;
                }
            }
            write_indent(f, indent)?;
            writeln!(f, "END IF")
        }
        IStmt::Do {
            var,
            start,
            end,
            body,
        } => {
            write_indent(f, indent)?;
            writeln!(f, "DO {} = {} TO {}", var, start, end)?;
            for s in body {
                fmt_istmt(s, f, indent + 4)?;
            }
            write_indent(f, indent)?;
            writeln!(f, "END DO")
        }
        IStmt::DoWhile { cond, body } => {
            write_indent(f, indent)?;
            writeln!(f, "DO WHILE ({})", cond)?;
            for s in body {
                fmt_istmt(s, f, indent + 4)?;
            }
            write_indent(f, indent)?;
            writeln!(f, "END DO")
        }
        IStmt::Call(name, args) => {
            write_indent(f, indent)?;
            write!(f, "CALL {}(", name)?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
            }
            writeln!(f, ")")
        }
        IStmt::Read(exprs) => {
            write_indent(f, indent)?;
            write!(f, "READ(")?;
            for (i, expr) in exprs.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", expr)?;
            }
            writeln!(f, ")")
        }
        IStmt::SelectCase {
            selector,
            cases,
            default,
        } => {
            write_indent(f, indent)?;
            writeln!(f, "SELECT CASE ({})", selector)?;
            for case in cases {
                write_indent(f, indent)?;
                write!(f, "CASE(")?;
                for (i, item) in case.items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    match item {
                        IcCaseItem::Single(e) => write!(f, "{}", e)?,
                        IcCaseItem::Range(l, r) => write!(f, "{}:{}", l, r)?,
                        IcCaseItem::Multi(exprs) => {
                            for (j, e) in exprs.iter().enumerate() {
                                if j > 0 {
                                    write!(f, ", ")?;
                                }
                                write!(f, "{}", e)?;
                            }
                        }
                    }
                }
                writeln!(f, ")")?;
                for s in &case.body {
                    fmt_istmt(s, f, indent + 4)?;
                }
            }
            if let Some(default_stmts) = default {
                write_indent(f, indent)?;
                writeln!(f, "CASE DEFAULT")?;
                for s in default_stmts {
                    fmt_istmt(s, f, indent + 4)?;
                }
            }
            write_indent(f, indent)?;
            writeln!(f, "END SELECT")
        }
        IStmt::Block { body } => {
            write_indent(f, indent)?;
            writeln!(f, "BLOCK")?;
            for s in body {
                fmt_istmt(s, f, indent + 4)?;
            }
            write_indent(f, indent)?;
            writeln!(f, "END BLOCK")
        }
        IStmt::Exit => {
            write_indent(f, indent)?;
            writeln!(f, "EXIT")
        }
        IStmt::Cycle => {
            write_indent(f, indent)?;
            writeln!(f, "CYCLE")
        }
    }
}

impl std::fmt::Display for IStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_istmt(self, f, 0)
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(return_type) = &self.return_type {
            writeln!(
                f,
                "FUNCTION {}({}) -> {:?}",
                self.name,
                self.params.join(", "),
                return_type
            )?;
        } else if self.params.is_empty() {
            writeln!(f, "PROGRAM {}", self.name)?;
        } else {
            writeln!(f, "SUBROUTINE {}({})", self.name, self.params.join(", "))?;
        }
        for stmt in &self.body {
            fmt_istmt(stmt, f, 2)?;
        }
        if self.return_type.is_some() {
            writeln!(f, "END FUNCTION")
        } else if self.params.is_empty() {
            writeln!(f, "END PROGRAM")
        } else {
            writeln!(f, "END SUBROUTINE")
        }
    }
}

pub fn print_ir(module: &Module, title: &str) {
    println!("\n=== {} ===", title);
    for uses in &module.uses_modules {
        println!("USE {}", uses);
    }
    for func in &module.funcs {
        println!("{}", func);
    }
    println!("=== End {} ===\n", title);
}

#[allow(dead_code)]
pub fn lower_to_ir(program: &Program) -> Result<LowerOutput> {
    lower_to_ir_with_debug(program, false)
}

pub fn lower_to_ir_with_debug(program: &Program, debug: bool) -> Result<LowerOutput> {
    let is_library = program.name == "<anon>";
    let entry_name = if is_library { "__f90c_init" } else { "main" };
    let mut current = Function {
        name: entry_name.into(),
        params: vec![],
        body: vec![],
        return_type: None,
    };
    let mut funcs: Vec<Function> = vec![];
    let mut fn_names = std::collections::HashSet::new();
    for s in &program.body {
        if let Stmt::Function { name, .. } = s {
            fn_names.insert(name.clone());
        } else if let Stmt::Subroutine { name, .. } = s {
            fn_names.insert(name.clone());
        }
    }
    let mut external_ret: std::collections::HashMap<String, crate::ast::TypeSpec> =
        std::collections::HashMap::new();
    for s in &program.body {
        if let Stmt::VarDecl { kind, names } = s {
            for n in names {
                if fn_names.contains(n) {
                    external_ret.insert(n.clone(), kind.clone());
                }
            }
        }
    }
    let mut defines_modules: Vec<String> = Vec::new();
    let mut uses_modules: Vec<String> = Vec::new();
    for s in &program.body {
        match s {
            Stmt::Module { name, .. } => defines_modules.push(name.to_ascii_lowercase()),
            Stmt::Use { module } => uses_modules.push(module.to_ascii_lowercase()),
            _ => {}
        }
    }
    let has_program = program.name != "<anon>";

    fn fold_binary_const(op: &crate::ast::BinOp, left: &IExpr, right: &IExpr) -> Option<IExpr> {
        use crate::ast::BinOp::*;
        match (left, right) {
            (IExpr::IntLit(l), IExpr::IntLit(r)) => {
                let lval: i64 = l.parse().ok()?;
                let rval: i64 = r.parse().ok()?;

                if lval.abs() > 1_000_000_000_000i64 || rval.abs() > 1_000_000_000_000i64 {
                    return None;
                }

                let result = match op {
                    Add => lval.checked_add(rval)?,
                    Sub => lval.checked_sub(rval)?,
                    Mul => lval.checked_mul(rval)?,
                    Div if rval != 0 => lval.checked_div(rval)?,
                    Pow if rval >= 0 && rval <= 30 => {
                        let result = (lval as f64).powf(rval as f64);
                        if result.is_finite() {
                            return Some(IExpr::RealLit(result.to_string()));
                        } else {
                            return None;
                        }
                    }
                    Eq => return Some(IExpr::Logical(lval == rval)),
                    Ne => return Some(IExpr::Logical(lval != rval)),
                    Lt => return Some(IExpr::Logical(lval < rval)),
                    Gt => return Some(IExpr::Logical(lval > rval)),
                    Le => return Some(IExpr::Logical(lval <= rval)),
                    Ge => return Some(IExpr::Logical(lval >= rval)),
                    _ => return None,
                };
                Some(IExpr::IntLit(result.to_string()))
            }

            (IExpr::RealLit(_), IExpr::RealLit(_)) => None,

            (IExpr::IntLit(_), IExpr::RealLit(_)) => None,

            (IExpr::RealLit(_), IExpr::IntLit(_)) => None,

            (IExpr::Logical(l), IExpr::Logical(r)) => {
                let result = match op {
                    And => *l && *r,
                    Or => *l || *r,
                    Eqv => *l == *r,
                    Neqv => *l != *r,
                    Eq => *l == *r,
                    Ne => *l != *r,
                    _ => return None,
                };
                Some(IExpr::Logical(result))
            }

            (IExpr::Str(l), IExpr::Str(r)) => match op {
                Concat => Some(IExpr::Str(format!("{}{}", l, r))),
                Eq => Some(IExpr::Logical(l == r)),
                Ne => Some(IExpr::Logical(l != r)),
                Lt => Some(IExpr::Logical(l < r)),
                Gt => Some(IExpr::Logical(l > r)),
                Le => Some(IExpr::Logical(l <= r)),
                Ge => Some(IExpr::Logical(l >= r)),
                _ => None,
            },
            _ => None,
        }
    }

    fn fold_unary_const(op: &crate::ast::UnOp, operand: &IExpr) -> Option<IExpr> {
        use crate::ast::UnOp::*;
        match (op, operand) {
            (Neg, IExpr::IntLit(s)) => {
                let val: i64 = s.parse().ok()?;

                if val.abs() > 1_000_000_000_000i64 {
                    return None;
                }
                val.checked_neg()
                    .map(|result| IExpr::IntLit(result.to_string()))
            }
            (Neg, IExpr::RealLit(_)) => None,
            (Not, IExpr::Logical(b)) => Some(IExpr::Logical(!b)),
            _ => None,
        }
    }

    #[allow(dead_code)]
    fn expr_has_side_effects(expr: &IExpr) -> bool {
        match expr {
            IExpr::Str(_)
            | IExpr::IntLit(_)
            | IExpr::RealLit(_)
            | IExpr::Logical(_)
            | IExpr::Ident(_) => false,
            IExpr::Index(_, indices) => indices.iter().any(expr_has_side_effects),
            IExpr::Call(_, _) => true,
            IExpr::Bin(_, l, r) => expr_has_side_effects(l) || expr_has_side_effects(r),
            IExpr::Un(_, e) => expr_has_side_effects(e),
        }
    }

    fn stmt_has_side_effects_or_control_flow(stmt: &IStmt) -> bool {
        match stmt {
            IStmt::Print(_) | IStmt::Read(_) | IStmt::Call(_, _) => true,

            IStmt::Exit | IStmt::Cycle => true,

            IStmt::AssignExpr(_, expr) => expr_has_side_effects(expr),
            IStmt::AssignIndex(_, indices, expr) => {
                indices.iter().any(expr_has_side_effects) || expr_has_side_effects(expr)
            }
            IStmt::Return(opt_expr) => opt_expr.as_ref().map_or(false, expr_has_side_effects),

            IStmt::DeclVar(_, _, dims) => dims.iter().any(expr_has_side_effects),
            IStmt::AssignStr(_, _)
            | IStmt::AssignIntLit(_, _)
            | IStmt::AssignRealLit(_, _)
            | IStmt::AssignBool(_, _)
            | IStmt::AssignIdent(_, _) => false,

            IStmt::If {
                cond,
                then_body,
                else_body,
            } => {
                expr_has_side_effects(cond)
                    || then_body.iter().any(stmt_has_side_effects_or_control_flow)
                    || else_body.as_ref().map_or(false, |stmts| {
                        stmts.iter().any(stmt_has_side_effects_or_control_flow)
                    })
            }
            IStmt::Do {
                start, end, body, ..
            } => {
                expr_has_side_effects(start)
                    || expr_has_side_effects(end)
                    || body.iter().any(stmt_has_side_effects_or_control_flow)
            }
            IStmt::DoWhile { cond, body } => {
                expr_has_side_effects(cond)
                    || body.iter().any(stmt_has_side_effects_or_control_flow)
            }
            IStmt::SelectCase {
                selector,
                cases,
                default,
            } => {
                expr_has_side_effects(selector)
                    || cases
                        .iter()
                        .any(|case| case.body.iter().any(stmt_has_side_effects_or_control_flow))
                    || default.as_ref().map_or(false, |stmts| {
                        stmts.iter().any(stmt_has_side_effects_or_control_flow)
                    })
            }
            IStmt::Block { body } => body.iter().any(stmt_has_side_effects_or_control_flow),
        }
    }

    #[allow(dead_code)]
    fn is_const_expr(expr: &IExpr) -> bool {
        match expr {
            IExpr::Str(_) | IExpr::IntLit(_) | IExpr::RealLit(_) | IExpr::Logical(_) => true,
            IExpr::Ident(_) | IExpr::Index(_, _) | IExpr::Call(_, _) => false,
            IExpr::Bin(_, l, r) => is_const_expr(l) && is_const_expr(r),
            IExpr::Un(_, e) => is_const_expr(e),
        }
    }

    fn optimize_stmt_list(stmts: Vec<IStmt>, func_name: Option<&str>) -> Vec<IStmt> {
        let mut constants = std::collections::HashMap::new();

        let mut potential_function_names = std::collections::HashSet::new();
        let mut declared_vars = std::collections::HashSet::new();
        let mut assigned_vars = std::collections::HashSet::new();
        let mut used_vars = std::collections::HashSet::new();

        for stmt in &stmts {
            match stmt {
                IStmt::DeclVar(_, var, _) => {
                    declared_vars.insert(var.clone());
                }
                IStmt::AssignIntLit(var, _)
                | IStmt::AssignRealLit(var, _)
                | IStmt::AssignBool(var, _)
                | IStmt::AssignStr(var, _) => {
                    assigned_vars.insert(var.clone());
                }
                IStmt::AssignExpr(var, _) => {
                    assigned_vars.insert(var.clone());

                    collect_variable_uses_in_stmt(stmt, &mut used_vars);
                }
                _ => {
                    collect_variable_uses_in_stmt(stmt, &mut used_vars);
                }
            }
        }

        if let Some(function_name) = func_name {
            potential_function_names.insert(function_name.to_string());
        }

        let mut modified_vars = std::collections::HashSet::new();
        let mut dowhile_condition_vars = std::collections::HashSet::new();
        let mut dowhile_body_vars = std::collections::HashSet::new();
        for stmt in &stmts {
            match stmt {
                IStmt::AssignIntLit(var, _)
                | IStmt::AssignRealLit(var, _)
                | IStmt::AssignBool(var, _)
                | IStmt::AssignStr(var, _)
                | IStmt::AssignIdent(var, _)
                | IStmt::AssignExpr(var, _)
                | IStmt::AssignIndex(var, _, _) => {
                    modified_vars.insert(var.clone());
                }
                IStmt::DoWhile { cond, body } => {
                    collect_variable_uses(cond, &mut dowhile_condition_vars);

                    for stmt in body {
                        collect_assigned_vars_in_stmt(stmt, &mut dowhile_body_vars);
                    }
                }
                _ => {}
            }
        }

        let mut multiply_assigned_vars = std::collections::HashSet::new();
        let mut assigned_once = std::collections::HashSet::new();
        for stmt in &stmts {
            match stmt {
                IStmt::AssignIntLit(var, _)
                | IStmt::AssignRealLit(var, _)
                | IStmt::AssignBool(var, _)
                | IStmt::AssignStr(var, _)
                | IStmt::AssignExpr(var, _)
                | IStmt::AssignIdent(var, _) => {
                    if assigned_once.contains(var) {
                        multiply_assigned_vars.insert(var.clone());
                    } else {
                        assigned_once.insert(var.clone());
                    }
                }
                _ => {}
            }
        }

        for stmt in &stmts {
            match stmt {
                IStmt::AssignIntLit(var, val) => {
                    if !potential_function_names.contains(var)
                        && !dowhile_condition_vars.contains(var)
                        && !dowhile_body_vars.contains(var)
                        && !multiply_assigned_vars.contains(var)
                    {
                        constants.insert(var.clone(), IExpr::IntLit(val.clone()));
                    }
                }
                IStmt::AssignRealLit(var, val) => {
                    if !potential_function_names.contains(var)
                        && !dowhile_condition_vars.contains(var)
                        && !dowhile_body_vars.contains(var)
                        && !multiply_assigned_vars.contains(var)
                    {
                        constants.insert(var.clone(), IExpr::RealLit(val.clone()));
                    }
                }
                IStmt::AssignBool(var, val) => {
                    if !potential_function_names.contains(var)
                        && !dowhile_condition_vars.contains(var)
                        && !dowhile_body_vars.contains(var)
                        && !multiply_assigned_vars.contains(var)
                    {
                        constants.insert(var.clone(), IExpr::Logical(*val));
                    }
                }
                IStmt::AssignStr(var, val) => {
                    if !potential_function_names.contains(var)
                        && !dowhile_condition_vars.contains(var)
                        && !dowhile_body_vars.contains(var)
                        && !multiply_assigned_vars.contains(var)
                    {
                        constants.insert(var.clone(), IExpr::Str(val.clone()));
                    }
                }

                IStmt::AssignExpr(var, expr) => {
                    if !potential_function_names.contains(var)
                        && !dowhile_condition_vars.contains(var)
                        && !dowhile_body_vars.contains(var)
                        && !multiply_assigned_vars.contains(var)
                    {
                        if let Some(const_val) = evaluate_const_expr(expr, &constants) {
                            constants.insert(var.clone(), const_val);
                        }
                    }
                }
                _ => {}
            }
        }

        let mut final_optimized_stmts: Vec<IStmt> = stmts
            .into_iter()
            .flat_map(|stmt| optimize_stmt_with_constants(stmt, &constants))
            .collect();

        let mut multiply_assigned_after_opt = std::collections::HashSet::new();
        let mut assigned_once_after_opt = std::collections::HashSet::new();

        for stmt in &final_optimized_stmts {
            match stmt {
                IStmt::AssignIntLit(var, _)
                | IStmt::AssignRealLit(var, _)
                | IStmt::AssignBool(var, _)
                | IStmt::AssignStr(var, _)
                | IStmt::AssignExpr(var, _)
                | IStmt::AssignIdent(var, _) => {
                    if assigned_once_after_opt.contains(var) {
                        multiply_assigned_after_opt.insert(var.clone());
                    } else {
                        assigned_once_after_opt.insert(var.clone());
                    }
                }
                _ => {}
            }
        }

        let multiply_assigned_vars = multiply_assigned_after_opt;

        for stmt in &mut final_optimized_stmts {
            if let IStmt::Print(args) = stmt {
                for arg in args {
                    if let IExpr::IntLit(val) = arg {
                        for var in &multiply_assigned_vars {
                            if let Some(IExpr::IntLit(const_val)) = constants.get(var) {
                                if const_val == val {
                                    *arg = IExpr::Ident(var.clone());
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut array_elem_consts: std::collections::HashMap<(String, i64), i64> =
            std::collections::HashMap::new();
        for s in &final_optimized_stmts {
            if let IStmt::AssignIndex(name, indices, expr) = s {
                if indices.len() == 1 {
                    if let IExpr::IntLit(idx_str) = &indices[0] {
                        if let Ok(idx_val) = idx_str.parse::<i64>() {
                            if let IExpr::IntLit(val_str) = expr {
                                if let Ok(v) = val_str.parse::<i64>() {
                                    array_elem_consts.insert((name.clone(), idx_val), v);
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut folded_stmts: Vec<IStmt> = final_optimized_stmts.clone();
        let mut pos = 0usize;
        while pos < folded_stmts.len() {
            if let IStmt::AssignIntLit(acc_var, init_val) = &folded_stmts[pos] {
                if init_val == "0" {
                    let mut found: Option<(usize, i64)> = None;
                    for j in (pos + 1)..folded_stmts.len() {
                        if let IStmt::Do {
                            var: loop_var,
                            start,
                            end,
                            body,
                        } = &folded_stmts[j]
                        {
                            if body.len() == 1 {
                                if let IStmt::AssignExpr(lhs, expr) = &body[0] {
                                    if lhs == acc_var {
                                        if let IExpr::Bin(op, left, right) = expr {
                                            if matches!(op, crate::ast::BinOp::Add) {
                                                let (array_name_opt, loop_idx_opt) =
                                                    match (&**left, &**right) {
                                                        (
                                                            IExpr::Ident(id),
                                                            IExpr::Index(arr, idxs),
                                                        ) if id == acc_var && idxs.len() == 1 => {
                                                            if let IExpr::Ident(ii) = &idxs[0] {
                                                                (
                                                                    Some(arr.clone()),
                                                                    Some(ii.clone()),
                                                                )
                                                            } else {
                                                                (None, None)
                                                            }
                                                        }
                                                        (
                                                            IExpr::Index(arr, idxs),
                                                            IExpr::Ident(id),
                                                        ) if id == acc_var && idxs.len() == 1 => {
                                                            if let IExpr::Ident(ii) = &idxs[0] {
                                                                (
                                                                    Some(arr.clone()),
                                                                    Some(ii.clone()),
                                                                )
                                                            } else {
                                                                (None, None)
                                                            }
                                                        }
                                                        _ => (None, None),
                                                    };
                                                if let (Some(array_name), Some(loop_idx_name)) =
                                                    (array_name_opt, loop_idx_opt)
                                                {
                                                    if loop_idx_name == *loop_var {
                                                        if let (
                                                            IExpr::IntLit(sstr),
                                                            IExpr::IntLit(estr),
                                                        ) = (start, end)
                                                        {
                                                            if let (Ok(sv), Ok(ev)) = (
                                                                sstr.parse::<i64>(),
                                                                estr.parse::<i64>(),
                                                            ) {
                                                                let mut all_present = true;
                                                                let mut total: i64 = 0;
                                                                for idx in sv..=ev {
                                                                    if let Some(v) =
                                                                        array_elem_consts.get(&(
                                                                            array_name.clone(),
                                                                            idx,
                                                                        ))
                                                                    {
                                                                        total = total
                                                                            .saturating_add(*v);
                                                                    } else {
                                                                        all_present = false;
                                                                        break;
                                                                    }
                                                                }
                                                                if all_present {
                                                                    let mut assign_count = 0usize;
                                                                    for s in &folded_stmts {
                                                                        match s {
                                                                            IStmt::AssignIntLit(v, _) | IStmt::AssignRealLit(v, _) | IStmt::AssignBool(v, _) | IStmt::AssignStr(v, _) | IStmt::AssignIdent(v, _) | IStmt::AssignExpr(v, _) => {
                                                                                if v == acc_var { assign_count += 1 }
                                                                            }
                                                                            IStmt::AssignIndex(v, _, _) => { if v == acc_var { assign_count += 1 } }
                                                                            IStmt::Do { body, .. } => {
                                                                                for bs in body { if let IStmt::AssignExpr(v, _) = bs { if v == acc_var { assign_count += 1 } } }
                                                                            }
                                                                            _ => {}
                                                                        }
                                                                    }

                                                                    if assign_count == 2 {
                                                                        found = Some((j, total));
                                                                        break;
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if let Some((do_idx, total)) = found {
                        folded_stmts[pos] = IStmt::AssignIntLit(acc_var.clone(), total.to_string());
                        folded_stmts.remove(do_idx);
                    }
                }
            }
            pos += 1;
        }

        let mut folded2: Vec<IStmt> = Vec::new();
        let mut idx = 0usize;
        while idx < folded_stmts.len() {
            if let IStmt::AssignIntLit(var, zero) = &folded_stmts[idx] {
                if zero == "0" {
                    let mut j = idx + 1;
                    let mut indices: Vec<(String, i64)> = Vec::new();
                    while j < folded_stmts.len() {
                        if let IStmt::AssignExpr(lhs, rhs) = &folded_stmts[j] {
                            if lhs == var {
                                if let IExpr::Index(arr, idxs) = rhs {
                                    if idxs.len() == 1 {
                                        if let IExpr::IntLit(idx_str) = &idxs[0] {
                                            if let Ok(idx_val) = idx_str.parse::<i64>() {
                                                indices.push((arr.clone(), idx_val));
                                                j += 1;
                                                continue;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        break;
                    }
                    if !indices.is_empty() {
                        let arr0 = &indices[0].0;
                        let mut ok = true;
                        let mut total: i64 = 0;
                        for (arrn, idv) in &indices {
                            if arrn != arr0 {
                                ok = false;
                                break;
                            }
                            if let Some(v) = array_elem_consts.get(&(arrn.clone(), *idv)) {
                                total = total.saturating_add(*v);
                            } else {
                                ok = false;
                                break;
                            }
                        }
                        if ok {
                            folded2.push(IStmt::AssignIntLit(var.clone(), total.to_string()));
                            idx = j;
                            continue;
                        }
                    }
                }
            }

            if let IStmt::AssignExpr(var, _rhs) = &folded_stmts[idx] {
                let mut j = idx;
                let mut indices: Vec<(String, i64)> = Vec::new();
                while j < folded_stmts.len() {
                    if let IStmt::AssignExpr(lhs, rhs) = &folded_stmts[j] {
                        if lhs == var {
                            if let IExpr::Index(arr, idxs) = rhs {
                                if idxs.len() == 1 {
                                    if let IExpr::IntLit(idx_str) = &idxs[0] {
                                        if let Ok(idx_val) = idx_str.parse::<i64>() {
                                            indices.push((arr.clone(), idx_val));
                                            j += 1;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    break;
                }
                if indices.len() > 1 {
                    let arr0 = &indices[0].0;
                    let mut ok = true;
                    let mut total: i64 = 0;
                    for (arrn, idv) in &indices {
                        if arrn != arr0 {
                            ok = false;
                            break;
                        }
                        if let Some(v) = array_elem_consts.get(&(arrn.clone(), *idv)) {
                            total = total.saturating_add(*v);
                        } else {
                            ok = false;
                            break;
                        }
                    }
                    if ok {
                        folded2.push(IStmt::AssignIntLit(var.clone(), total.to_string()));
                        idx = j;
                        continue;
                    }
                }
            }

            folded2.push(folded_stmts[idx].clone());
            idx += 1;
        }

        let final_folded: Vec<IStmt> = folded2.into_iter().collect();
        final_folded
    }

    fn eliminate_dead_code_final(stmts: Vec<IStmt>, func_name: Option<&str>) -> Vec<IStmt> {
        let mut all_uses = std::collections::HashSet::new();
        let mut function_names = std::collections::HashSet::new();
        let mut function_return_vars = std::collections::HashSet::new();
        let mut loop_critical = std::collections::HashSet::new();
        let mut declared_vars = std::collections::HashSet::new();
        let mut assigned_vars = std::collections::HashSet::new();

        for stmt in &stmts {
            collect_variable_uses_in_stmt(stmt, &mut all_uses);

            match stmt {
                IStmt::DeclVar(_, var, _) => {
                    declared_vars.insert(var.clone());
                }
                IStmt::AssignIntLit(var, _)
                | IStmt::AssignRealLit(var, _)
                | IStmt::AssignBool(var, _)
                | IStmt::AssignStr(var, _)
                | IStmt::AssignExpr(var, _) => {
                    assigned_vars.insert(var.clone());
                }
                _ => {}
            }
        }

        if let Some(function_name) = func_name {
            function_names.insert(function_name.to_string());
        }

        for stmt in &stmts {
            match stmt {
                IStmt::Print(args) => {
                    for arg in args {
                        collect_variable_uses(arg, &mut all_uses);
                    }
                }
                IStmt::AssignExpr(var, expr) => {
                    collect_variable_uses(expr, &mut all_uses);

                    if let IExpr::Ident(rhs_var) = expr {
                        function_return_vars.insert(rhs_var.clone());
                        function_names.insert(var.clone());
                    }

                    if !matches!(expr, IExpr::Ident(_)) {
                        function_names.insert(var.clone());
                    }
                }
                IStmt::AssignIndex(var, indices, expr) => {
                    all_uses.insert(var.clone());
                    for idx_expr in indices {
                        collect_variable_uses(idx_expr, &mut all_uses);
                    }
                    collect_variable_uses(expr, &mut all_uses);
                }
                IStmt::If {
                    cond,
                    then_body,
                    else_body,
                } => {
                    collect_variable_uses(cond, &mut all_uses);
                    for s in then_body {
                        collect_variable_uses_in_stmt(s, &mut all_uses);
                    }
                    if let Some(else_stmts) = else_body {
                        for s in else_stmts {
                            collect_variable_uses_in_stmt(s, &mut all_uses);
                        }
                    }
                }
                IStmt::Do {
                    var,
                    start,
                    end,
                    body,
                } => {
                    loop_critical.insert(var.clone());
                    collect_variable_uses(start, &mut all_uses);
                    collect_variable_uses(end, &mut all_uses);
                    for s in body {
                        collect_variable_uses_in_stmt(s, &mut all_uses);

                        collect_assigned_vars_in_stmt(s, &mut loop_critical);
                    }
                }
                IStmt::DoWhile { cond, body } => {
                    collect_variable_uses(cond, &mut all_uses);
                    for s in body {
                        collect_variable_uses_in_stmt(s, &mut all_uses);

                        collect_assigned_vars_in_stmt(s, &mut loop_critical);
                    }
                }
                IStmt::Return(Some(expr)) => {
                    collect_variable_uses(expr, &mut all_uses);
                }
                _ => {}
            }
        }

        let mut needed_vars = all_uses.clone();
        needed_vars.extend(function_names);
        needed_vars.extend(function_return_vars);
        needed_vars.extend(loop_critical);

        let mut result = Vec::new();
        for (_idx, stmt) in stmts.into_iter().enumerate() {
            match &stmt {
                IStmt::DeclVar(_, var, _) => {
                    if needed_vars.contains(var) {
                        result.push(stmt);
                    }
                }
                IStmt::AssignIntLit(var, _)
                | IStmt::AssignRealLit(var, _)
                | IStmt::AssignBool(var, _)
                | IStmt::AssignStr(var, _)
                | IStmt::AssignExpr(var, _)
                | IStmt::AssignIndex(var, _, _) => {
                    if needed_vars.contains(var) {
                        result.push(stmt);
                    }
                }
                _ => {
                    result.push(stmt);
                }
            }
        }

        result
    }

    fn collect_variable_uses(expr: &IExpr, uses: &mut std::collections::HashSet<String>) {
        match expr {
            IExpr::Ident(name) => {
                uses.insert(name.clone());
            }
            IExpr::Bin(_, l, r) => {
                collect_variable_uses(l, uses);
                collect_variable_uses(r, uses);
            }
            IExpr::Un(_, e) => {
                collect_variable_uses(e, uses);
            }
            IExpr::Index(name, indices) => {
                uses.insert(name.clone());
                for idx in indices {
                    collect_variable_uses(idx, uses);
                }
            }
            IExpr::Call(_, args) => {
                for arg in args {
                    collect_variable_uses(arg, uses);
                }
            }
            _ => {}
        }
    }

    fn collect_variable_uses_in_stmt(stmt: &IStmt, uses: &mut std::collections::HashSet<String>) {
        match stmt {
            IStmt::Print(args) => {
                for arg in args {
                    collect_variable_uses(arg, uses);
                }
            }
            IStmt::AssignExpr(_, expr) => {
                collect_variable_uses(expr, uses);
            }
            IStmt::AssignIndex(var, indices, expr) => {
                uses.insert(var.clone());
                for idx in indices {
                    collect_variable_uses(idx, uses);
                }
                collect_variable_uses(expr, uses);
            }
            IStmt::If {
                cond,
                then_body,
                else_body,
            } => {
                collect_variable_uses(cond, uses);
                for s in then_body {
                    collect_variable_uses_in_stmt(s, uses);
                }
                if let Some(else_stmts) = else_body {
                    for s in else_stmts {
                        collect_variable_uses_in_stmt(s, uses);
                    }
                }
            }
            IStmt::Do {
                var: _,
                start,
                end,
                body,
            } => {
                collect_variable_uses(start, uses);
                collect_variable_uses(end, uses);
                for s in body {
                    collect_variable_uses_in_stmt(s, uses);
                }
            }
            IStmt::DoWhile { cond, body } => {
                collect_variable_uses(cond, uses);
                for s in body {
                    collect_variable_uses_in_stmt(s, uses);
                }
            }
            IStmt::Call(_, args) => {
                for arg in args {
                    collect_variable_uses(arg, uses);
                }
            }
            IStmt::Return(opt_expr) => {
                if let Some(expr) = opt_expr {
                    collect_variable_uses(expr, uses);
                }
            }
            IStmt::Read(exprs) => {
                for expr in exprs {
                    collect_variable_uses(expr, uses);
                }
            }
            IStmt::SelectCase {
                selector,
                cases,
                default,
            } => {
                collect_variable_uses(selector, uses);
                for case in cases {
                    for stmt in &case.body {
                        collect_variable_uses_in_stmt(stmt, uses);
                    }
                }
                if let Some(default_stmts) = default {
                    for stmt in default_stmts {
                        collect_variable_uses_in_stmt(stmt, uses);
                    }
                }
            }
            IStmt::Block { body } => {
                for s in body {
                    collect_variable_uses_in_stmt(s, uses);
                }
            }
            IStmt::DeclVar(_, _, dims) => {
                for dim in dims {
                    collect_variable_uses(dim, uses);
                }
            }
            _ => {}
        }
    }

    fn collect_assigned_vars_in_stmt(
        stmt: &IStmt,
        assigned: &mut std::collections::HashSet<String>,
    ) {
        match stmt {
            IStmt::AssignStr(var, _) => {
                assigned.insert(var.clone());
            }
            IStmt::AssignIntLit(var, _) => {
                assigned.insert(var.clone());
            }
            IStmt::AssignRealLit(var, _) => {
                assigned.insert(var.clone());
            }
            IStmt::AssignBool(var, _) => {
                assigned.insert(var.clone());
            }
            IStmt::AssignIdent(var, _) => {
                assigned.insert(var.clone());
            }
            IStmt::AssignExpr(var, _) => {
                assigned.insert(var.clone());
            }
            IStmt::AssignIndex(var, _, _) => {
                assigned.insert(var.clone());
            }
            IStmt::If {
                then_body,
                else_body,
                ..
            } => {
                for s in then_body {
                    collect_assigned_vars_in_stmt(s, assigned);
                }
                if let Some(else_stmts) = else_body {
                    for s in else_stmts {
                        collect_assigned_vars_in_stmt(s, assigned);
                    }
                }
            }
            IStmt::Do { body, .. } => {
                for s in body {
                    collect_assigned_vars_in_stmt(s, assigned);
                }
            }
            IStmt::DoWhile { body, .. } => {
                for s in body {
                    collect_assigned_vars_in_stmt(s, assigned);
                }
            }
            IStmt::SelectCase { cases, default, .. } => {
                for case in cases {
                    for s in &case.body {
                        collect_assigned_vars_in_stmt(s, assigned);
                    }
                }
                if let Some(default_stmts) = default {
                    for s in default_stmts {
                        collect_assigned_vars_in_stmt(s, assigned);
                    }
                }
            }
            IStmt::Block { body } => {
                for s in body {
                    collect_assigned_vars_in_stmt(s, assigned);
                }
            }
            _ => {}
        }
    }

    fn collect_assigned_vars_in_stmt_list(stmts: &[IStmt]) -> std::collections::HashSet<String> {
        let mut assigned = std::collections::HashSet::new();
        for stmt in stmts {
            collect_assigned_vars_in_stmt(stmt, &mut assigned);
        }
        assigned
    }

    fn evaluate_const_expr(
        expr: &IExpr,
        constants: &std::collections::HashMap<String, IExpr>,
    ) -> Option<IExpr> {
        match expr {
            IExpr::IntLit(_) | IExpr::RealLit(_) | IExpr::Logical(_) | IExpr::Str(_) => {
                Some(expr.clone())
            }
            IExpr::Ident(name) => constants.get(name).cloned(),
            IExpr::Bin(op, l, r) => {
                let left = evaluate_const_expr(l, constants)?;
                let right = evaluate_const_expr(r, constants)?;
                fold_binary_const(op, &left, &right)
            }
            IExpr::Un(op, e) => {
                let operand = evaluate_const_expr(e, constants)?;
                fold_unary_const(op, &operand)
            }
            _ => None,
        }
    }

    fn optimize_stmt_with_constants(
        stmt: IStmt,
        constants: &std::collections::HashMap<String, IExpr>,
    ) -> Vec<IStmt> {
        match stmt {
            IStmt::If {
                cond,
                then_body,
                else_body,
            } => {
                let optimized_cond = optimize_expr_with_constants(cond, constants);

                if let IExpr::Logical(condition_value) = &optimized_cond {
                    if *condition_value {
                        then_body
                            .into_iter()
                            .flat_map(|stmt| optimize_stmt_with_constants(stmt, constants))
                            .collect()
                    } else {
                        if let Some(else_stmts) = else_body {
                            else_stmts
                                .into_iter()
                                .flat_map(|stmt| optimize_stmt_with_constants(stmt, constants))
                                .collect()
                        } else {
                            vec![]
                        }
                    }
                } else {
                    let optimized_then: Vec<IStmt> = then_body
                        .into_iter()
                        .flat_map(|stmt| optimize_stmt_with_constants(stmt, constants))
                        .collect();
                    let optimized_else = else_body.map(|else_stmts| {
                        else_stmts
                            .into_iter()
                            .flat_map(|stmt| optimize_stmt_with_constants(stmt, constants))
                            .collect()
                    });

                    vec![IStmt::If {
                        cond: optimized_cond,
                        then_body: optimized_then,
                        else_body: optimized_else,
                    }]
                }
            }
            IStmt::Do {
                var,
                start,
                end,
                body,
            } => {
                let optimized_start = optimize_expr_with_constants(start, constants);
                let optimized_end = optimize_expr_with_constants(end, constants);

                let assigned_in_loop = collect_assigned_vars_in_stmt_list(&body);

                let mut loop_constants = constants.clone();
                for assigned_var in &assigned_in_loop {
                    loop_constants.remove(assigned_var);
                }

                loop_constants.remove(&var);

                let optimized_body: Vec<IStmt> = body
                    .into_iter()
                    .flat_map(|stmt| optimize_stmt_with_constants(stmt, &loop_constants))
                    .collect();

                if let (IExpr::IntLit(start_str), IExpr::IntLit(end_str)) =
                    (&optimized_start, &optimized_end)
                {
                    if let (Ok(start_val), Ok(end_val)) =
                        (start_str.parse::<i64>(), end_str.parse::<i64>())
                    {
                        let loop_count = if end_val >= start_val {
                            (end_val - start_val + 1) as usize
                        } else {
                            0
                        };

                        let assigned_conflicts_constants =
                            assigned_in_loop.iter().any(|v| constants.contains_key(v));
                        if loop_count <= 1000
                            && loop_count > 0
                            && !optimized_body
                                .iter()
                                .any(stmt_has_side_effects_or_control_flow)
                            && !assigned_conflicts_constants
                        {
                            let mut unrolled = Vec::new();
                            for i in start_val..=end_val {
                                let mut loop_constants = constants.clone();
                                loop_constants.insert(var.clone(), IExpr::IntLit(i.to_string()));

                                for stmt in &optimized_body {
                                    let substituted = substitute_loop_var(
                                        stmt.clone(),
                                        &var,
                                        &IExpr::IntLit(i.to_string()),
                                    );
                                    unrolled.extend(optimize_stmt_with_constants(
                                        substituted,
                                        &loop_constants,
                                    ));
                                }
                            }
                            return unrolled;
                        }
                    }
                }

                vec![IStmt::Do {
                    var,
                    start: optimized_start,
                    end: optimized_end,
                    body: optimized_body,
                }]
            }
            IStmt::AssignExpr(var, expr) => {
                let optimized_expr = optimize_expr_with_constants(expr, constants);
                match optimized_expr {
                    IExpr::IntLit(val) => vec![IStmt::AssignIntLit(var.clone(), val)],
                    IExpr::RealLit(val) => vec![IStmt::AssignRealLit(var.clone(), val)],
                    IExpr::Logical(val) => vec![IStmt::AssignBool(var.clone(), val)],
                    IExpr::Str(val) => vec![IStmt::AssignStr(var.clone(), val)],
                    _ => vec![IStmt::AssignExpr(var.clone(), optimized_expr)],
                }
            }
            IStmt::AssignIndex(var, indices, expr) => {
                let optimized_indices = indices
                    .into_iter()
                    .map(|idx| optimize_expr_with_constants(idx, constants))
                    .collect();
                let optimized_expr = optimize_expr_with_constants(expr, constants);
                vec![IStmt::AssignIndex(var, optimized_indices, optimized_expr)]
            }
            IStmt::AssignIdent(var, id) => {
                if let Some(const_val) = constants.get(&id) {
                    match const_val {
                        IExpr::IntLit(v) => vec![IStmt::AssignIntLit(var, v.clone())],
                        IExpr::RealLit(v) => vec![IStmt::AssignRealLit(var, v.clone())],
                        IExpr::Logical(v) => vec![IStmt::AssignBool(var, *v)],
                        IExpr::Str(v) => vec![IStmt::AssignStr(var, v.clone())],
                        _ => vec![IStmt::AssignIdent(var, id)],
                    }
                } else {
                    vec![IStmt::AssignIdent(var, id)]
                }
            }
            IStmt::Print(args) => {
                let optimized_args = args
                    .into_iter()
                    .map(|arg| match arg {
                        IExpr::Ident(id) => IExpr::Ident(id),
                        other => optimize_expr_with_constants(other, constants),
                    })
                    .collect();
                vec![IStmt::Print(optimized_args)]
            }
            IStmt::Read(vars) => {
                let optimized_vars = vars
                    .into_iter()
                    .map(|var| optimize_expr_with_constants(var, constants))
                    .collect();
                vec![IStmt::Read(optimized_vars)]
            }
            IStmt::Call(name, args) => {
                let optimized_args = args
                    .into_iter()
                    .map(|arg| optimize_expr_with_constants(arg, constants))
                    .collect();
                vec![IStmt::Call(name, optimized_args)]
            }
            IStmt::Return(value) => {
                let optimized_value = value.map(|v| optimize_expr_with_constants(v, constants));
                vec![IStmt::Return(optimized_value)]
            }
            IStmt::DoWhile { cond, body } => {
                let empty_constants = std::collections::HashMap::new();
                let optimized_body: Vec<IStmt> = body
                    .into_iter()
                    .flat_map(|stmt| optimize_stmt_with_constants(stmt, &empty_constants))
                    .collect();

                vec![IStmt::DoWhile {
                    cond: cond,
                    body: optimized_body,
                }]
            }
            IStmt::SelectCase {
                selector,
                cases,
                default,
            } => {
                let optimized_selector = optimize_expr_with_constants(selector, constants);
                let optimized_cases = cases
                    .into_iter()
                    .map(|case| IcCase {
                        items: case.items,
                        body: case
                            .body
                            .into_iter()
                            .flat_map(|stmt| optimize_stmt_with_constants(stmt, constants))
                            .collect(),
                    })
                    .collect();
                let optimized_default = default.map(|default_stmts| {
                    default_stmts
                        .into_iter()
                        .flat_map(|stmt| optimize_stmt_with_constants(stmt, constants))
                        .collect()
                });

                vec![IStmt::SelectCase {
                    selector: optimized_selector,
                    cases: optimized_cases,
                    default: optimized_default,
                }]
            }
            IStmt::Block { body } => {
                let optimized_body: Vec<IStmt> = body
                    .into_iter()
                    .flat_map(|stmt| optimize_stmt_with_constants(stmt, constants))
                    .collect();
                if optimized_body.is_empty() {
                    vec![]
                } else {
                    vec![IStmt::Block {
                        body: optimized_body,
                    }]
                }
            }
            IStmt::DeclVar(kind, name, dims) => {
                let optimized_dims = dims
                    .into_iter()
                    .map(|d| optimize_expr_with_constants(d, constants))
                    .collect();
                vec![IStmt::DeclVar(kind, name, optimized_dims)]
            }

            other => vec![other],
        }
    }

    fn optimize_expr_with_constants(
        expr: IExpr,
        constants: &std::collections::HashMap<String, IExpr>,
    ) -> IExpr {
        if let Some(const_val) = evaluate_const_expr(&expr, constants) {
            return const_val;
        }

        match expr {
            IExpr::Bin(op, l, r) => {
                let left = optimize_expr_with_constants(*l, constants);
                let right = optimize_expr_with_constants(*r, constants);

                if let Some(folded) = fold_binary_const(&op, &left, &right) {
                    folded
                } else {
                    match (&op, &left, &right) {
                        (crate::ast::BinOp::Add, expr, IExpr::IntLit(s))
                        | (crate::ast::BinOp::Add, IExpr::IntLit(s), expr)
                            if s == "0" =>
                        {
                            expr.clone()
                        }
                        (crate::ast::BinOp::Add, expr, IExpr::RealLit(s))
                        | (crate::ast::BinOp::Add, IExpr::RealLit(s), expr)
                            if s == "0" || s == "0.0" =>
                        {
                            expr.clone()
                        }

                        (crate::ast::BinOp::Sub, expr, IExpr::IntLit(s)) if s == "0" => {
                            expr.clone()
                        }
                        (crate::ast::BinOp::Sub, expr, IExpr::RealLit(s))
                            if s == "0" || s == "0.0" =>
                        {
                            expr.clone()
                        }

                        (crate::ast::BinOp::Mul, _, IExpr::IntLit(s))
                        | (crate::ast::BinOp::Mul, IExpr::IntLit(s), _)
                            if s == "0" =>
                        {
                            IExpr::IntLit("0".to_string())
                        }
                        (crate::ast::BinOp::Mul, _, IExpr::RealLit(s))
                        | (crate::ast::BinOp::Mul, IExpr::RealLit(s), _)
                            if s == "0" || s == "0.0" =>
                        {
                            IExpr::RealLit("0.0".to_string())
                        }

                        (crate::ast::BinOp::Mul, expr, IExpr::IntLit(s))
                        | (crate::ast::BinOp::Mul, IExpr::IntLit(s), expr)
                            if s == "1" =>
                        {
                            expr.clone()
                        }
                        (crate::ast::BinOp::Mul, expr, IExpr::RealLit(s))
                        | (crate::ast::BinOp::Mul, IExpr::RealLit(s), expr)
                            if s == "1" || s == "1.0" =>
                        {
                            expr.clone()
                        }

                        (crate::ast::BinOp::Div, expr, IExpr::IntLit(s)) if s == "1" => {
                            expr.clone()
                        }
                        (crate::ast::BinOp::Div, expr, IExpr::RealLit(s))
                            if s == "1" || s == "1.0" =>
                        {
                            expr.clone()
                        }

                        (crate::ast::BinOp::Pow, expr, IExpr::IntLit(s)) if s == "1" => {
                            expr.clone()
                        }
                        (crate::ast::BinOp::Pow, expr, IExpr::RealLit(s))
                            if s == "1" || s == "1.0" =>
                        {
                            expr.clone()
                        }
                        (crate::ast::BinOp::Pow, _, IExpr::IntLit(s)) if s == "0" => {
                            IExpr::IntLit("1".to_string())
                        }
                        (crate::ast::BinOp::Pow, _, IExpr::RealLit(s))
                            if s == "0" || s == "0.0" =>
                        {
                            IExpr::RealLit("1.0".to_string())
                        }

                        (crate::ast::BinOp::And, expr, IExpr::Logical(true))
                        | (crate::ast::BinOp::And, IExpr::Logical(true), expr) => expr.clone(),
                        (crate::ast::BinOp::And, _, IExpr::Logical(false))
                        | (crate::ast::BinOp::And, IExpr::Logical(false), _) => {
                            IExpr::Logical(false)
                        }

                        (crate::ast::BinOp::Or, expr, IExpr::Logical(false))
                        | (crate::ast::BinOp::Or, IExpr::Logical(false), expr) => expr.clone(),
                        (crate::ast::BinOp::Or, _, IExpr::Logical(true))
                        | (crate::ast::BinOp::Or, IExpr::Logical(true), _) => IExpr::Logical(true),

                        _ => IExpr::Bin(op, Box::new(left), Box::new(right)),
                    }
                }
            }
            IExpr::Un(op, e) => {
                let operand = optimize_expr_with_constants(*e, constants);

                if let Some(folded) = fold_unary_const(&op, &operand) {
                    folded
                } else {
                    match (&op, &operand) {
                        (crate::ast::UnOp::Neg, IExpr::Un(crate::ast::UnOp::Neg, inner)) => {
                            *inner.clone()
                        }
                        (crate::ast::UnOp::Not, IExpr::Un(crate::ast::UnOp::Not, inner)) => {
                            *inner.clone()
                        }
                        _ => IExpr::Un(op, Box::new(operand)),
                    }
                }
            }
            IExpr::Index(name, indices) => {
                let optimized_indices = indices
                    .into_iter()
                    .map(|idx| optimize_expr_with_constants(idx, constants))
                    .collect();
                IExpr::Index(name, optimized_indices)
            }
            IExpr::Call(name, args) => {
                let optimized_args = args
                    .into_iter()
                    .map(|arg| optimize_expr_with_constants(arg, constants))
                    .collect();
                IExpr::Call(name, optimized_args)
            }

            other => other,
        }
    }

    fn lower_expr(
        e: &Expr,
        types: &std::collections::HashMap<String, crate::ast::TypeSpec>,
    ) -> IExpr {
        match e {
            Expr::Str(s) => IExpr::Str(s.clone()),
            Expr::IntLit(s) => IExpr::IntLit(s.clone()),
            Expr::RealLit(s) => IExpr::RealLit(s.clone()),
            Expr::Logical(b) => IExpr::Logical(*b),
            Expr::Ident(id) => IExpr::Ident(id.clone()),
            Expr::Bin(op, l, r) => {
                let left = lower_expr(l, types);
                let right = lower_expr(r, types);
                IExpr::Bin(op.clone(), Box::new(left), Box::new(right))
            }
            Expr::Un(op, e) => {
                let operand = lower_expr(e, types);
                IExpr::Un(op.clone(), Box::new(operand))
            }
            Expr::Call(name, args) => {
                if types.contains_key(name) {
                    IExpr::Index(
                        name.clone(),
                        args.iter().map(|a| lower_expr(a, types)).collect(),
                    )
                } else {
                    IExpr::Call(
                        name.clone(),
                        args.iter().map(|a| lower_expr(a, types)).collect(),
                    )
                }
            }
            Expr::Index(name, args) => IExpr::Index(
                name.clone(),
                args.iter().map(|a| lower_expr(a, types)).collect(),
            ),
        }
    }

    fn lower_stmts(
        out: &mut Vec<IStmt>,
        stmts: &[Stmt],
        collected: &mut Vec<Function>,
        external_ret: &std::collections::HashMap<String, crate::ast::TypeSpec>,
        types: &mut std::collections::HashMap<String, crate::ast::TypeSpec>,
    ) {
        for s in stmts {
            let mut new_stmts = Vec::new();
            match s {
                Stmt::Print { items } => {
                    let v = items.iter().map(|i| lower_expr(i, types)).collect();
                    new_stmts.push(IStmt::Print(v));
                }
                Stmt::VarDecl { kind, names } => {
                    let mut kept = Vec::new();
                    for n in names {
                        if !external_ret.contains_key(n) {
                            kept.push(n.clone());
                        }
                    }
                    if !kept.is_empty() {
                        for n in kept.iter() {
                            types.insert(n.clone(), kind.clone());
                        }
                        for n in kept {
                            new_stmts.push(IStmt::DeclVar(kind.clone(), n, Vec::new()));
                        }
                    }
                }
                Stmt::Assign { name, value } => match value {
                    Expr::Str(s) => new_stmts.push(IStmt::AssignStr(name.clone(), s.clone())),
                    Expr::IntLit(s) => new_stmts.push(IStmt::AssignIntLit(name.clone(), s.clone())),
                    Expr::RealLit(s) => {
                        new_stmts.push(IStmt::AssignRealLit(name.clone(), s.clone()))
                    }
                    Expr::Logical(b) => new_stmts.push(IStmt::AssignBool(name.clone(), *b)),
                    Expr::Ident(id) => new_stmts.push(IStmt::AssignIdent(name.clone(), id.clone())),
                    _ => new_stmts.push(IStmt::AssignExpr(name.clone(), lower_expr(value, types))),
                },
                Stmt::AssignIndex {
                    name,
                    indices,
                    value,
                } => {
                    let mut idxs: Vec<IExpr> = Vec::new();
                    for a in indices {
                        idxs.push(lower_expr(a, types));
                    }
                    new_stmts.push(IStmt::AssignIndex(
                        name.clone(),
                        idxs,
                        lower_expr(value, types),
                    ));
                }
                Stmt::ArrayDecl { kind, name, dims } => {
                    let mut idxs: Vec<IExpr> = Vec::new();
                    for d in dims {
                        idxs.push(lower_expr(d, types));
                    }
                    types.insert(name.clone(), kind.clone());
                    new_stmts.push(IStmt::DeclVar(kind.clone(), name.clone(), idxs));
                }
                Stmt::Return(opt) => {
                    new_stmts.push(IStmt::Return(opt.as_ref().map(|o| lower_expr(o, types))));
                }
                Stmt::If {
                    cond,
                    then_body,
                    else_body,
                } => {
                    let mut then_ir = Vec::new();

                    let mut child_types = types.clone();
                    lower_stmts(
                        &mut then_ir,
                        then_body,
                        collected,
                        external_ret,
                        &mut child_types,
                    );
                    let else_ir = else_body.as_ref().map(|eb| {
                        let mut v = Vec::new();
                        let mut child_types = types.clone();
                        lower_stmts(&mut v, eb, collected, external_ret, &mut child_types);
                        v
                    });
                    new_stmts.push(IStmt::If {
                        cond: lower_expr(cond, types),
                        then_body: then_ir,
                        else_body: else_ir,
                    });
                }
                Stmt::Do {
                    var,
                    start,
                    end,
                    body,
                } => {
                    let mut body_ir = Vec::new();

                    let mut child_types = types.clone();
                    child_types.insert(var.clone(), crate::ast::TypeSpec::Integer(None));
                    lower_stmts(
                        &mut body_ir,
                        body,
                        collected,
                        external_ret,
                        &mut child_types,
                    );
                    new_stmts.push(IStmt::Do {
                        var: var.clone(),
                        start: lower_expr(start, types),
                        end: lower_expr(end, types),
                        body: body_ir,
                    });
                }
                Stmt::Function {
                    name,
                    params,
                    return_type: rt,
                    body,
                } => {
                    let mut f_body: Vec<IStmt> = Vec::new();

                    let mut fn_types: std::collections::HashMap<String, crate::ast::TypeSpec> =
                        std::collections::HashMap::new();
                    lower_stmts(&mut f_body, body, collected, external_ret, &mut fn_types);
                    if !params.is_empty() {
                        let param_set: std::collections::HashSet<_> =
                            params.iter().cloned().collect();
                        f_body.retain(|st| match st {
                            IStmt::DeclVar(_, var, _) if param_set.contains(var) => false,
                            _ => true,
                        });
                    }
                    let eff_rt = if rt.is_none() {
                        external_ret.get(name).cloned()
                    } else {
                        rt.clone()
                    };

                    let optimized_body = optimize_stmt_list(f_body, Some(name));
                    collected.push(Function {
                        name: name.clone(),
                        params: params.clone(),
                        body: optimized_body,
                        return_type: eff_rt,
                    });
                }
                Stmt::Subroutine { name, params, body } => {
                    let mut f_body: Vec<IStmt> = Vec::new();
                    let mut fn_types: std::collections::HashMap<String, crate::ast::TypeSpec> =
                        std::collections::HashMap::new();
                    lower_stmts(&mut f_body, body, collected, external_ret, &mut fn_types);

                    let optimized_body = optimize_stmt_list(f_body, Some(name));
                    collected.push(Function {
                        name: name.clone(),
                        params: params.clone(),
                        body: optimized_body,
                        return_type: None,
                    });
                }
                Stmt::CallSub { name, args } => {
                    new_stmts.push(IStmt::Call(
                        name.clone(),
                        args.iter().map(|a| lower_expr(a, types)).collect(),
                    ));
                }
                Stmt::Read { args } => {
                    new_stmts.push(IStmt::Read(
                        args.iter().map(|a| lower_expr(a, types)).collect(),
                    ));
                }
                Stmt::SelectCase {
                    expr,
                    cases,
                    default,
                } => {
                    let selector = lower_expr(expr, types);
                    let mut icases: Vec<IcCase> = Vec::new();
                    for cb in cases {
                        let mut items: Vec<IcCaseItem> = Vec::new();

                        let mut pending_singles: Vec<IExpr> = Vec::new();
                        for it in &cb.items {
                            match it {
                                crate::ast::CaseItem::Single(e) => {
                                    pending_singles.push(lower_expr(e, types));
                                }
                                crate::ast::CaseItem::Range(l, r) => {
                                    if !pending_singles.is_empty() {
                                        items.push(IcCaseItem::Multi(pending_singles));
                                        pending_singles = Vec::new();
                                    }
                                    items.push(IcCaseItem::Range(
                                        lower_expr(l, types),
                                        lower_expr(r, types),
                                    ));
                                }
                            }
                        }
                        if !pending_singles.is_empty() {
                            if pending_singles.len() == 1 {
                                items.push(IcCaseItem::Single(pending_singles.remove(0)));
                            } else {
                                items.push(IcCaseItem::Multi(pending_singles));
                            }
                        }
                        icases.push(IcCase {
                            items,
                            body: {
                                let mut b = Vec::new();
                                let mut child_types = types.clone();
                                lower_stmts(
                                    &mut b,
                                    &cb.body,
                                    collected,
                                    external_ret,
                                    &mut child_types,
                                );
                                b
                            },
                        });
                    }
                    let default_ir = default.as_ref().map(|d| {
                        let mut v = Vec::new();
                        let mut child_types = types.clone();
                        lower_stmts(&mut v, d, collected, external_ret, &mut child_types);
                        v
                    });
                    new_stmts.push(IStmt::SelectCase {
                        selector,
                        cases: icases,
                        default: default_ir,
                    });
                }
                Stmt::Use { .. } => {}
                Stmt::Module {
                    name: _,
                    body: module_body,
                } => {
                    for s in module_body {
                        match s {
                            Stmt::Function {
                                name,
                                params,
                                return_type: rt,
                                body,
                            } => {
                                let mut f_body: Vec<IStmt> = Vec::new();
                                let mut fn_types: std::collections::HashMap<
                                    String,
                                    crate::ast::TypeSpec,
                                > = std::collections::HashMap::new();
                                lower_stmts(
                                    &mut f_body,
                                    body,
                                    collected,
                                    external_ret,
                                    &mut fn_types,
                                );
                                if !params.is_empty() {
                                    let param_set: std::collections::HashSet<_> =
                                        params.iter().cloned().collect();
                                    f_body.retain(|st| match st {
                                        IStmt::DeclVar(_, var, _) if param_set.contains(var) => {
                                            false
                                        }
                                        _ => true,
                                    });
                                }
                                let eff_rt = if rt.is_none() {
                                    external_ret.get(name).cloned()
                                } else {
                                    rt.clone()
                                };

                                let optimized_body = optimize_stmt_list(f_body, Some(name));
                                collected.push(Function {
                                    name: name.clone(),
                                    params: params.clone(),
                                    body: optimized_body,
                                    return_type: eff_rt.clone(),
                                });
                            }
                            Stmt::Subroutine { name, params, body } => {
                                let mut f_body: Vec<IStmt> = Vec::new();
                                let mut fn_types: std::collections::HashMap<
                                    String,
                                    crate::ast::TypeSpec,
                                > = std::collections::HashMap::new();
                                lower_stmts(
                                    &mut f_body,
                                    body,
                                    collected,
                                    external_ret,
                                    &mut fn_types,
                                );

                                let optimized_body = optimize_stmt_list(f_body, Some(name));
                                collected.push(Function {
                                    name: name.clone(),
                                    params: params.clone(),
                                    body: optimized_body,
                                    return_type: None,
                                });
                            }
                            _ => {
                                let mut child_types = types.clone();
                                lower_stmts(
                                    out,
                                    std::slice::from_ref(s),
                                    collected,
                                    external_ret,
                                    &mut child_types,
                                );
                            }
                        }
                    }
                }
                Stmt::Implicit(_) => {
                    // Implicit statements are handled during semantic analysis
                }
                Stmt::Block { body } => {
                    let mut b = Vec::new();
                    let mut child_types = types.clone();
                    lower_stmts(&mut b, body, collected, external_ret, &mut child_types);
                    new_stmts.push(IStmt::Block { body: b });
                }
                Stmt::DoWhile { cond, body } => {
                    let mut b = Vec::new();
                    let mut child_types = types.clone();
                    lower_stmts(&mut b, body, collected, external_ret, &mut child_types);
                    new_stmts.push(IStmt::DoWhile {
                        cond: lower_expr(cond, types),
                        body: b,
                    });
                }
                Stmt::Exit => {
                    new_stmts.push(IStmt::Exit);
                }
                Stmt::Cycle => {
                    new_stmts.push(IStmt::Cycle);
                }
            }

            out.extend(new_stmts);
        }
    }

    let mut top_types: std::collections::HashMap<String, crate::ast::TypeSpec> =
        std::collections::HashMap::new();
    for s in &program.body {
        if let Stmt::Function {
            name,
            params,
            return_type: rt,
            body,
        } = s
        {
            let mut f_body: Vec<IStmt> = Vec::new();

            let mut fn_types: std::collections::HashMap<String, crate::ast::TypeSpec> =
                std::collections::HashMap::new();
            lower_stmts(&mut f_body, body, &mut funcs, &external_ret, &mut fn_types);
            let eff_rt = if rt.is_none() {
                external_ret.get(name).cloned()
            } else {
                rt.clone()
            };
            let func_name = if is_library && name == "main" {
                "__f90c_init".to_string()
            } else {
                name.clone()
            };

            let optimized_body = optimize_stmt_list(f_body, Some(&func_name));
            funcs.push(Function {
                name: func_name,
                params: params.clone(),
                body: optimized_body,
                return_type: eff_rt,
            });
        } else if let Stmt::Subroutine { name, params, body } = s {
            let mut f_body: Vec<IStmt> = Vec::new();
            let mut fn_types: std::collections::HashMap<String, crate::ast::TypeSpec> =
                std::collections::HashMap::new();
            lower_stmts(&mut f_body, body, &mut funcs, &external_ret, &mut fn_types);
            let subr_name = if is_library && name == "main" {
                "__f90c_init".to_string()
            } else {
                name.clone()
            };

            let optimized_body = optimize_stmt_list(f_body, Some(&subr_name));
            funcs.push(Function {
                name: subr_name,
                params: params.clone(),
                body: optimized_body,
                return_type: None,
            });
        } else if let Stmt::VarDecl { kind, names } = s {
            for n in names {
                if !external_ret.contains_key(n) {
                    top_types.insert(n.clone(), kind.clone());
                    current
                        .body
                        .push(IStmt::DeclVar(kind.clone(), n.clone(), Vec::new()));
                }
            }
        } else {
            lower_stmts(
                &mut current.body,
                std::slice::from_ref(s),
                &mut funcs,
                &external_ret,
                &mut top_types,
            );
        }
    }

    funcs.insert(0, current.clone());

    if debug {
        let unoptimized_module = Module {
            funcs: funcs.clone(),
            uses_modules: uses_modules.clone(),
        };
        print_ir(&unoptimized_module, "IR Before Optimization");
    }
    for func in &mut funcs {
        func.body = optimize_stmt_list(func.body.clone(), Some(&func.name));
    }

    for func in &mut funcs {
        let mut array_elem_consts: std::collections::HashMap<(String, i64), i64> =
            std::collections::HashMap::new();
        for st in &func.body {
            if let IStmt::AssignIndex(name, indices, expr) = st {
                if indices.len() == 1 {
                    if let IExpr::IntLit(idx_str) = &indices[0] {
                        if let Ok(idx_val) = idx_str.parse::<i64>() {
                            if let IExpr::IntLit(val_str) = expr {
                                if let Ok(v) = val_str.parse::<i64>() {
                                    array_elem_consts.insert((name.clone(), idx_val), v);
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut i = 0usize;

        let mut plans: Vec<(usize, usize, String, i64)> = Vec::new();
        while i < func.body.len() {
            if let IStmt::AssignIntLit(acc_var, zero_str) = &func.body[i] {
                if zero_str == "0" {
                    let mut found: Option<(usize, i64)> = None;
                    for j in (i + 1)..func.body.len() {
                        if let IStmt::Do {
                            var: loop_var,
                            start,
                            end,
                            body,
                        } = &func.body[j]
                        {
                            if body.len() == 1 {
                                if let IStmt::AssignExpr(lhs, expr) = &body[0] {
                                    if lhs == acc_var {
                                        if let IExpr::Bin(op, left, right) = expr {
                                            if matches!(op, crate::ast::BinOp::Add) {
                                                let (array_name_opt, loop_idx_opt) =
                                                    match (&**left, &**right) {
                                                        (
                                                            IExpr::Ident(id),
                                                            IExpr::Index(arr, idxs),
                                                        ) if id == acc_var && idxs.len() == 1 => {
                                                            if let IExpr::Ident(ii) = &idxs[0] {
                                                                (
                                                                    Some(arr.clone()),
                                                                    Some(ii.clone()),
                                                                )
                                                            } else {
                                                                (None, None)
                                                            }
                                                        }
                                                        (
                                                            IExpr::Index(arr, idxs),
                                                            IExpr::Ident(id),
                                                        ) if id == acc_var && idxs.len() == 1 => {
                                                            if let IExpr::Ident(ii) = &idxs[0] {
                                                                (
                                                                    Some(arr.clone()),
                                                                    Some(ii.clone()),
                                                                )
                                                            } else {
                                                                (None, None)
                                                            }
                                                        }
                                                        _ => (None, None),
                                                    };
                                                if let (Some(array_name), Some(loop_idx_name)) =
                                                    (array_name_opt, loop_idx_opt)
                                                {
                                                    if loop_idx_name == *loop_var {
                                                        if let (
                                                            IExpr::IntLit(sstr),
                                                            IExpr::IntLit(estr),
                                                        ) = (start, end)
                                                        {
                                                            if let (Ok(sv), Ok(ev)) = (
                                                                sstr.parse::<i64>(),
                                                                estr.parse::<i64>(),
                                                            ) {
                                                                let mut all_present = true;
                                                                let mut total: i64 = 0;
                                                                for idx in sv..=ev {
                                                                    if let Some(v) =
                                                                        array_elem_consts.get(&(
                                                                            array_name.clone(),
                                                                            idx,
                                                                        ))
                                                                    {
                                                                        total = total
                                                                            .saturating_add(*v);
                                                                    } else {
                                                                        all_present = false;
                                                                        break;
                                                                    }
                                                                }
                                                                if all_present {
                                                                    let mut assigns = 0usize;
                                                                    for st in &func.body {
                                                                        match st {
                                                                            IStmt::AssignIntLit(v, _) | IStmt::AssignRealLit(v, _) | IStmt::AssignBool(v, _) | IStmt::AssignStr(v, _) | IStmt::AssignIdent(v, _) | IStmt::AssignExpr(v, _) => { if v == acc_var { assigns += 1 } }
                                                                            IStmt::Do { body, .. } => { for bs in body { if let IStmt::AssignExpr(v, _) = bs { if v == acc_var { assigns += 1 } } } }
                                                                            _ => {}
                                                                        }
                                                                    }
                                                                    if assigns == 2 {
                                                                        found = Some((j, total));
                                                                        break;
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if let Some((do_idx, total)) = found {
                        plans.push((i, do_idx, acc_var.clone(), total));
                    }
                }
            }
            i += 1;
        }

        plans.sort_by_key(|p| p.0);
        for (init_idx, do_idx, acc_var, total) in plans.into_iter().rev() {
            func.body[init_idx] = IStmt::AssignIntLit(acc_var.clone(), total.to_string());
            func.body.remove(do_idx);
        }
    }

    for func in &mut funcs {
        func.body = eliminate_dead_code_final(func.body.clone(), Some(&func.name));
    }

    for func in &mut funcs {
        let mut single_literal_map: std::collections::HashMap<String, IExpr> =
            std::collections::HashMap::new();
        let mut assign_counts: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();
        for st in &func.body {
            match st {
                IStmt::AssignIntLit(v, val) => {
                    *assign_counts.entry(v.clone()).or_default() += 1;
                    single_literal_map.insert(v.clone(), IExpr::IntLit(val.clone()));
                }
                IStmt::AssignRealLit(v, val) => {
                    *assign_counts.entry(v.clone()).or_default() += 1;
                    single_literal_map.insert(v.clone(), IExpr::RealLit(val.clone()));
                }
                IStmt::AssignBool(v, val) => {
                    *assign_counts.entry(v.clone()).or_default() += 1;
                    single_literal_map.insert(v.clone(), IExpr::Logical(*val));
                }
                IStmt::AssignStr(v, val) => {
                    *assign_counts.entry(v.clone()).or_default() += 1;
                    single_literal_map.insert(v.clone(), IExpr::Str(val.clone()));
                }
                _ => {
                    if let IStmt::AssignExpr(v, _)
                    | IStmt::AssignIdent(v, _)
                    | IStmt::AssignIndex(v, _, _) = st
                    {
                        *assign_counts.entry(v.clone()).or_default() += 1;
                    }
                }
            }
        }

        let mut replacements: Vec<(String, IExpr)> = Vec::new();
        for (name, expr) in single_literal_map.into_iter() {
            if assign_counts.get(&name).cloned().unwrap_or(0) == 1 {
                replacements.push((name, expr));
            }
        }

        if !replacements.is_empty() {
            let mut new_body: Vec<IStmt> = Vec::new();
            'stmt_loop: for st in func.body.drain(..) {
                match st {
                    IStmt::Print(mut args) => {
                        for (name, val) in &replacements {
                            args = args
                                .into_iter()
                                .map(|a| substitute_loop_var_in_expr(a, name, val))
                                .collect();
                        }
                        let mut folded_args: Vec<IExpr> = Vec::new();
                        for arg in args {
                            match arg {
                                IExpr::Str(s) => {
                                    if let Some(IExpr::Str(prev)) = folded_args.last_mut() {
                                        *prev = format!("{}{}", prev, s);
                                    } else {
                                        folded_args.push(IExpr::Str(s));
                                    }
                                }
                                other => folded_args.push(other),
                            }
                        }
                        new_body.push(IStmt::Print(folded_args));
                        continue 'stmt_loop;
                    }
                    IStmt::Call(nm, mut args) => {
                        for (name, val) in &replacements {
                            args = args
                                .into_iter()
                                .map(|a| substitute_loop_var_in_expr(a, name, val))
                                .collect();
                        }
                        new_body.push(IStmt::Call(nm, args));
                        continue 'stmt_loop;
                    }
                    IStmt::Return(opt) => {
                        let new_opt = opt.map(|e| {
                            let mut e2 = e;
                            for (name, val) in &replacements {
                                e2 = substitute_loop_var_in_expr(e2, name, val);
                            }
                            e2
                        });
                        new_body.push(IStmt::Return(new_opt));
                        continue 'stmt_loop;
                    }
                    _ => {
                        new_body.push(st);
                        continue 'stmt_loop;
                    }
                }
            }
            func.body = eliminate_dead_code_final(new_body, Some(&func.name));
        }
    }

    let final_module = Module {
        funcs,
        uses_modules: uses_modules.clone(),
    };

    let mut pre_fm = final_module.clone();
    for func in &mut pre_fm.funcs {
        let mut assign_counts: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();
        let mut str_assigns: std::collections::HashMap<String, String> =
            std::collections::HashMap::new();
        for st in &func.body {
            match st {
                IStmt::AssignStr(v, val) => {
                    *assign_counts.entry(v.clone()).or_default() += 1;
                    str_assigns.insert(v.clone(), val.clone());
                }
                IStmt::AssignExpr(v, _)
                | IStmt::AssignIdent(v, _)
                | IStmt::AssignIndex(v, _, _) => {
                    *assign_counts.entry(v.clone()).or_default() += 1;
                }
                IStmt::AssignIntLit(v, _)
                | IStmt::AssignRealLit(v, _)
                | IStmt::AssignBool(v, _) => {
                    *assign_counts.entry(v.clone()).or_default() += 1;
                }
                _ => {}
            }
        }

        let single_strs: std::collections::HashMap<String, String> = str_assigns
            .into_iter()
            .filter(|(k, _)| assign_counts.get(k).cloned().unwrap_or(0) == 1)
            .collect();

        if !single_strs.is_empty() {
            let mut new_body: Vec<IStmt> = Vec::new();
            for st in func.body.drain(..) {
                match st {
                    IStmt::Print(args) => {
                        let mut replaced: Vec<IExpr> = Vec::new();
                        for a in args {
                            match a {
                                IExpr::Ident(name) => {
                                    if let Some(s) = single_strs.get(&name) {
                                        replaced.push(IExpr::Str(s.clone()));
                                    } else {
                                        replaced.push(IExpr::Ident(name));
                                    }
                                }
                                other => replaced.push(other),
                            }
                        }

                        let mut folded: Vec<IExpr> = Vec::new();
                        for a in replaced {
                            if let IExpr::Str(s) = a {
                                if let Some(IExpr::Str(prev)) = folded.last_mut() {
                                    *prev = format!("{}{}", prev, s);
                                } else {
                                    folded.push(IExpr::Str(s));
                                }
                            } else {
                                folded.push(a);
                            }
                        }
                        new_body.push(IStmt::Print(folded));
                    }
                    other => new_body.push(other),
                }
            }
            func.body = eliminate_dead_code_final(new_body, Some(&func.name));
        }
    }
    let mut fm = pre_fm;

    for func in &mut fm.funcs {
        let mut array_elem_consts: std::collections::HashMap<(String, i64), i64> =
            std::collections::HashMap::new();
        let mut array_dims: std::collections::HashMap<String, i64> =
            std::collections::HashMap::new();
        let mut zero_inits: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut assigned_counts: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();

        for st in &func.body {
            match st {
                IStmt::AssignIndex(name, indices, expr) => {
                    if indices.len() == 1 {
                        if let IExpr::IntLit(idx_str) = &indices[0] {
                            if let Ok(idx_val) = idx_str.parse::<i64>() {
                                if let IExpr::IntLit(val_str) = expr {
                                    if let Ok(v) = val_str.parse::<i64>() {
                                        array_elem_consts.insert((name.clone(), idx_val), v);
                                    }
                                }
                            }
                        }
                    }
                    *assigned_counts.entry(name.clone()).or_default() += 1;
                }
                IStmt::DeclVar(_, name, dims) => {
                    if dims.len() == 1 {
                        if let IExpr::IntLit(dim_str) = &dims[0] {
                            if let Ok(dim_val) = dim_str.parse::<i64>() {
                                array_dims.insert(name.clone(), dim_val);
                            }
                        }
                    }
                }
                IStmt::AssignIntLit(v, val) => {
                    *assigned_counts.entry(v.clone()).or_default() += 1;
                    if val == "0" {
                        zero_inits.insert(v.clone());
                    }
                }
                IStmt::AssignExpr(v, _)
                | IStmt::AssignIdent(v, _)
                | IStmt::AssignRealLit(v, _)
                | IStmt::AssignBool(v, _)
                | IStmt::AssignStr(v, _) => {
                    *assigned_counts.entry(v.clone()).or_default() += 1;
                }
                _ => {}
            }
        }

        let mut full_arrays: std::collections::HashMap<String, i64> =
            std::collections::HashMap::new();
        for (name, dim) in &array_dims {
            let mut all = true;
            let mut tot: i64 = 0;
            for i in 1..=*dim {
                if let Some(v) = array_elem_consts.get(&(name.clone(), i)) {
                    tot = tot.saturating_add(*v);
                } else {
                    all = false;
                    break;
                }
            }
            if all {
                full_arrays.insert(name.clone(), tot);
            }
        }

        if !full_arrays.is_empty() && !zero_inits.is_empty() {
            for st in &mut func.body {
                if let IStmt::Print(args) = st {
                    for arg in args.iter_mut() {
                        if let IExpr::Ident(name) = arg {
                            if zero_inits.contains(name)
                                && assigned_counts.get(name).cloned().unwrap_or(0) == 1
                            {
                                if let Some((_arr, total)) = full_arrays.iter().next() {
                                    *arg = IExpr::IntLit(total.to_string());
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    let final_module = fm;

    if debug {
        print_ir(&final_module, "IR After Optimization");
    }

    if is_library {
        if let Some(first) = final_module.funcs.first() {
            if first.name == "__f90c_init" && first.body.is_empty() {
                let mut trimmed_funcs = final_module.funcs;
                trimmed_funcs.remove(0);
                let trimmed_module = Module {
                    funcs: trimmed_funcs,
                    uses_modules: uses_modules.clone(),
                };
                return Ok(LowerOutput {
                    module: trimmed_module,
                    defines_modules,
                    uses_modules,
                    has_program,
                });
            }
        }
    }

    Ok(LowerOutput {
        module: final_module,
        defines_modules,
        uses_modules,
        has_program,
    })
}

fn substitute_loop_var(stmt: IStmt, var: &str, value: &IExpr) -> IStmt {
    match stmt {
        IStmt::AssignExpr(assign_var, expr) => {
            IStmt::AssignExpr(assign_var, substitute_loop_var_in_expr(expr, var, value))
        }
        IStmt::AssignIndex(assign_var, indices, expr) => IStmt::AssignIndex(
            assign_var,
            indices
                .into_iter()
                .map(|idx| substitute_loop_var_in_expr(idx, var, value))
                .collect(),
            substitute_loop_var_in_expr(expr, var, value),
        ),
        IStmt::Print(args) => IStmt::Print(
            args.into_iter()
                .map(|arg| substitute_loop_var_in_expr(arg, var, value))
                .collect(),
        ),
        IStmt::If {
            cond,
            then_body,
            else_body,
        } => IStmt::If {
            cond: substitute_loop_var_in_expr(cond, var, value),
            then_body: then_body
                .into_iter()
                .map(|s| substitute_loop_var(s, var, value))
                .collect(),
            else_body: else_body.map(|eb| {
                eb.into_iter()
                    .map(|s| substitute_loop_var(s, var, value))
                    .collect()
            }),
        },
        IStmt::Call(name, args) => IStmt::Call(
            name,
            args.into_iter()
                .map(|arg| substitute_loop_var_in_expr(arg, var, value))
                .collect(),
        ),
        IStmt::Return(ret_val) => {
            IStmt::Return(ret_val.map(|v| substitute_loop_var_in_expr(v, var, value)))
        }
        IStmt::Read(vars) => IStmt::Read(
            vars.into_iter()
                .map(|v| substitute_loop_var_in_expr(v, var, value))
                .collect(),
        ),
        IStmt::DoWhile { cond, body } => IStmt::DoWhile {
            cond: substitute_loop_var_in_expr(cond, var, value),
            body: body
                .into_iter()
                .map(|s| substitute_loop_var(s, var, value))
                .collect(),
        },
        IStmt::Do {
            var: loop_var,
            start,
            end,
            body,
        } => IStmt::Do {
            var: loop_var,
            start: substitute_loop_var_in_expr(start, var, value),
            end: substitute_loop_var_in_expr(end, var, value),
            body: body
                .into_iter()
                .map(|s| substitute_loop_var(s, var, value))
                .collect(),
        },
        IStmt::Block { body } => IStmt::Block {
            body: body
                .into_iter()
                .map(|s| substitute_loop_var(s, var, value))
                .collect(),
        },
        IStmt::DeclVar(kind, name, dims) => IStmt::DeclVar(
            kind,
            name,
            dims.into_iter()
                .map(|d| substitute_loop_var_in_expr(d, var, value))
                .collect(),
        ),
        other => other,
    }
}

fn substitute_loop_var_in_expr(expr: IExpr, var: &str, value: &IExpr) -> IExpr {
    match expr {
        IExpr::Ident(name) if name == var => value.clone(),
        IExpr::Bin(op, l, r) => IExpr::Bin(
            op,
            Box::new(substitute_loop_var_in_expr(*l, var, value)),
            Box::new(substitute_loop_var_in_expr(*r, var, value)),
        ),
        IExpr::Un(op, e) => IExpr::Un(op, Box::new(substitute_loop_var_in_expr(*e, var, value))),
        IExpr::Index(name, indices) => IExpr::Index(
            name,
            indices
                .into_iter()
                .map(|idx| substitute_loop_var_in_expr(idx, var, value))
                .collect(),
        ),
        IExpr::Call(name, args) => IExpr::Call(
            name,
            args.into_iter()
                .map(|arg| substitute_loop_var_in_expr(arg, var, value))
                .collect(),
        ),
        other => other,
    }
}
