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
    // DeclVar: type, name, optional dimensions (empty => scalar)
    DeclVar(crate::ast::TypeSpec, String, Vec<IExpr>),
    AssignStr(String, String),
    AssignIntLit(String, String),
    AssignRealLit(String, String),
    AssignBool(String, bool),
    AssignIdent(String, String),
    AssignExpr(String, IExpr),
    // assignment to array element: name, indices, value
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
    pub module_only: bool,
}

use crate::ast::{Expr, Program, Stmt};
use anyhow::Result;

pub fn lower_to_ir(program: &Program) -> Result<LowerOutput> {
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
    let module_only = !program.body.is_empty()
        && program
            .body
            .iter()
            .all(|s| matches!(s, Stmt::Module { .. }));
    let has_program = program.name != "<anon>";

    // types: a map of known variable names -> their TypeSpec. If an Expr::Call
    // references a name present in `types`, we treat it as an index expression
    // (e.g. a(i)) rather than a function call.
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
            Expr::Bin(op, l, r) => IExpr::Bin(
                op.clone(),
                Box::new(lower_expr(l, types)),
                Box::new(lower_expr(r, types)),
            ),
            Expr::Un(op, e) => IExpr::Un(op.clone(), Box::new(lower_expr(e, types))),
            Expr::Call(name, args) => {
                // If the name is a declared variable, interpret as an index.
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
            match s {
                Stmt::Print { items } => {
                    let v = items.iter().map(|i| lower_expr(i, types)).collect();
                    out.push(IStmt::Print(v));
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
                            // register declared variable into local types map
                            types.insert(n.clone(), kind.clone());
                        }
                        for n in kept {
                            out.push(IStmt::DeclVar(kind.clone(), n, Vec::new()));
                        }
                    }
                }
                Stmt::Assign { name, value } => match value {
                    Expr::Str(s) => out.push(IStmt::AssignStr(name.clone(), s.clone())),
                    Expr::IntLit(s) => out.push(IStmt::AssignIntLit(name.clone(), s.clone())),
                    Expr::RealLit(s) => out.push(IStmt::AssignRealLit(name.clone(), s.clone())),
                    Expr::Logical(b) => out.push(IStmt::AssignBool(name.clone(), *b)),
                    Expr::Ident(id) => out.push(IStmt::AssignIdent(name.clone(), id.clone())),
                    _ => out.push(IStmt::AssignExpr(name.clone(), lower_expr(value, types))),
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
                    out.push(IStmt::AssignIndex(
                        name.clone(),
                        idxs,
                        lower_expr(value, types),
                    ));
                }
                Stmt::ArrayDecl { kind, name, dims } => {
                    // lower dims to IExpr and register declared array in types
                    let mut idxs: Vec<IExpr> = Vec::new();
                    for d in dims {
                        idxs.push(lower_expr(d, types));
                    }
                    types.insert(name.clone(), kind.clone());
                    out.push(IStmt::DeclVar(kind.clone(), name.clone(), idxs));
                }
                Stmt::Return(opt) => {
                    out.push(IStmt::Return(opt.as_ref().map(|o| lower_expr(o, types))));
                }
                Stmt::If {
                    cond,
                    then_body,
                    else_body,
                } => {
                    let mut then_ir = Vec::new();
                    // child scope: clone current types map so inner declarations don't leak
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
                    out.push(IStmt::If {
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
                    // loop introduces its loop variable as integer in a child scope
                    let mut child_types = types.clone();
                    child_types.insert(var.clone(), crate::ast::TypeSpec::Integer(None));
                    lower_stmts(
                        &mut body_ir,
                        body,
                        collected,
                        external_ret,
                        &mut child_types,
                    );
                    out.push(IStmt::Do {
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
                    // new types map for the function body
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
                    collected.push(Function {
                        name: name.clone(),
                        params: params.clone(),
                        body: f_body,
                        return_type: eff_rt,
                    });
                }
                Stmt::Subroutine { name, params, body } => {
                    let mut f_body: Vec<IStmt> = Vec::new();
                    let mut fn_types: std::collections::HashMap<String, crate::ast::TypeSpec> =
                        std::collections::HashMap::new();
                    lower_stmts(&mut f_body, body, collected, external_ret, &mut fn_types);
                    collected.push(Function {
                        name: name.clone(),
                        params: params.clone(),
                        body: f_body,
                        return_type: None,
                    });
                }
                Stmt::CallSub { name, args } => {
                    out.push(IStmt::Call(
                        name.clone(),
                        args.iter().map(|a| lower_expr(a, types)).collect(),
                    ));
                }
                Stmt::Read { args } => {
                    out.push(IStmt::Read(
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
                        // convert CaseItem -> IcCaseItem
                        // Group consecutive Single items into a Multi variant so the
                        // backend can lower a parenthesized CASE(...,...) as one group.
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
                    out.push(IStmt::SelectCase {
                        selector,
                        cases: icases,
                        default: default_ir,
                    });
                }
                Stmt::Use { .. } => { /* ignore for now */ }
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
                                collected.push(Function {
                                    name: name.clone(),
                                    params: params.clone(),
                                    body: f_body,
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
                                collected.push(Function {
                                    name: name.clone(),
                                    params: params.clone(),
                                    body: f_body,
                                    return_type: None,
                                });
                            }
                            _ => {
                                // use a child copy of types for module-level nested statements
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
                Stmt::ImplicitNone => { /* no-op */ } // SelectCase already handled above; nothing to do here
                Stmt::Block { body } => {
                    let mut b = Vec::new();
                    let mut child_types = types.clone();
                    lower_stmts(&mut b, body, collected, external_ret, &mut child_types);
                    out.push(IStmt::Block { body: b });
                }
                Stmt::DoWhile { cond, body } => {
                    let mut b = Vec::new();
                    let mut child_types = types.clone();
                    lower_stmts(&mut b, body, collected, external_ret, &mut child_types);
                    out.push(IStmt::DoWhile {
                        cond: lower_expr(cond, types),
                        body: b,
                    });
                }
                Stmt::Exit => {
                    out.push(IStmt::Exit);
                }
                Stmt::Cycle => {
                    out.push(IStmt::Cycle);
                }
            }
        }
    }

    // track top-level declared variable types while lowering
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
            // function uses its own local type map
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
            funcs.push(Function {
                name: func_name,
                params: params.clone(),
                body: f_body,
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
            funcs.push(Function {
                name: subr_name,
                params: params.clone(),
                body: f_body,
                return_type: None,
            });
        } else if let Stmt::VarDecl { kind, names } = s {
            for n in names {
                if !external_ret.contains_key(n) {
                    // record top-level var decl
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
    funcs.insert(0, current);
    if is_library {
        if let Some(first) = funcs.first() {
            if first.name == "__f90c_init" && first.body.is_empty() {
                funcs.remove(0);
            }
        }
    }
    Ok(LowerOutput {
        module: Module {
            funcs,
            uses_modules: uses_modules.clone(),
        },
        defines_modules,
        uses_modules,
        has_program,
        module_only,
    })
}
