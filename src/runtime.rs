use crate::ast::{BinOp, Expr, Program, Stmt, TypeSpec, UnOp};
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct FuncDef {
    params: Vec<String>,
    body: Vec<Stmt>,
    is_function: bool, // true = function, false = subroutine
}

pub fn interpret(program: &Program) {
    let env_str: HashMap<String, String> = HashMap::new();
    let env_int: HashMap<String, i64> = HashMap::new();
    let env_real32: HashMap<String, f32> = HashMap::new();
    let env_real64: HashMap<String, f64> = HashMap::new();
    let env_log: HashMap<String, bool> = HashMap::new();
    let mut funcs: HashMap<String, FuncDef> = HashMap::new();
    for stmt in &program.body {
        match stmt {
            Stmt::Function {
                name, params, body, ..
            } => {
                funcs.insert(
                    name.clone(),
                    FuncDef {
                        params: params.clone(),
                        body: body.clone(),
                        is_function: true,
                    },
                );
            }
            Stmt::Subroutine { name, params, body } => {
                funcs.insert(
                    name.clone(),
                    FuncDef {
                        params: params.clone(),
                        body: body.clone(),
                        is_function: false,
                    },
                );
            }
            _ => {}
        }
    }

    fn parse_i64_lossy(s: &str) -> i64 {
        i128::from_str_radix(s, 10)
            .ok()
            .and_then(|v| i64::try_from(v).ok())
            .unwrap_or(0)
    }
    fn parse_f64_lossy(s: &str) -> f64 {
        s.parse::<f64>().unwrap_or(0.0)
    }

    #[derive(Clone, Debug, Default)]
    struct ValueTuple(
        pub Option<i64>,
        pub Option<f64>,
        pub Option<bool>,
        pub Option<String>,
    );

    impl ValueTuple {
        fn from_int(i: i64) -> Self {
            Self(Some(i), None, None, None)
        }
        fn from_real(f: f64) -> Self {
            Self(None, Some(f), None, None)
        }
        fn from_bool(b: bool) -> Self {
            Self(None, None, Some(b), None)
        }
        fn from_str(s: String) -> Self {
            Self(None, None, None, Some(s))
        }
    }

    fn eval_expr(
        e: &Expr,
        env_str: &HashMap<String, String>,
        env_int: &HashMap<String, i64>,
        env_real32: &HashMap<String, f32>,
        env_real64: &HashMap<String, f64>,
        env_log: &HashMap<String, bool>,
        funcs: &HashMap<String, FuncDef>,
        call_stack: &mut Vec<CallFrame>,
    ) -> ValueTuple {
        match e {
            Expr::IntLit(s) => ValueTuple::from_int(parse_i64_lossy(s)),
            Expr::RealLit(s) => ValueTuple::from_real(parse_f64_lossy(s)),
            Expr::Logical(b) => ValueTuple::from_bool(*b),
            Expr::Str(s) => ValueTuple::from_str(s.clone()),
            Expr::Ident(id) => {
                if let Some(v) = env_int.get(id) {
                    ValueTuple::from_int(*v)
                } else if let Some(v) = env_real64.get(id) {
                    ValueTuple::from_real(*v)
                } else if let Some(v) = env_real32.get(id) {
                    ValueTuple::from_real(*v as f64)
                } else if let Some(v) = env_log.get(id) {
                    ValueTuple::from_bool(*v)
                } else if let Some(v) = env_str.get(id) {
                    ValueTuple::from_str(v.clone())
                } else {
                    ValueTuple::from_int(0)
                }
            }
            Expr::Un(UnOp::Neg, inner) => {
                let vt = eval_expr(
                    inner, env_str, env_int, env_real32, env_real64, env_log, funcs, call_stack,
                );
                if let Some(i) = vt.0 {
                    ValueTuple::from_int(-i)
                } else if let Some(f) = vt.1 {
                    ValueTuple::from_real(-f)
                } else {
                    ValueTuple::from_int(0)
                }
            }
            Expr::Un(UnOp::Not, inner) => {
                let vt = eval_expr(
                    inner, env_str, env_int, env_real32, env_real64, env_log, funcs, call_stack,
                );
                ValueTuple::from_bool(!vt.2.unwrap_or(false))
            }
            Expr::Bin(op, l, r) => {
                let lv = eval_expr(
                    l, env_str, env_int, env_real32, env_real64, env_log, funcs, call_stack,
                );
                let rv = eval_expr(
                    r, env_str, env_int, env_real32, env_real64, env_log, funcs, call_stack,
                );
                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Pow => {
                        let both_int = lv.0.is_some() && rv.0.is_some();
                        if both_int && matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul) {
                            let li = lv.0.unwrap();
                            let ri = rv.0.unwrap();
                            let res = match op {
                                BinOp::Add => li + ri,
                                BinOp::Sub => li - ri,
                                BinOp::Mul => li * ri,
                                _ => 0,
                            };
                            ValueTuple::from_int(res)
                        } else {
                            let a = lv.1.or(lv.0.map(|v| v as f64)).unwrap_or(0.0);
                            let b = rv.1.or(rv.0.map(|v| v as f64)).unwrap_or(0.0);
                            let res = match op {
                                BinOp::Add => a + b,
                                BinOp::Sub => a - b,
                                BinOp::Mul => a * b,
                                BinOp::Div => {
                                    if b == 0.0 {
                                        0.0
                                    } else {
                                        a / b
                                    }
                                }
                                BinOp::Pow => a.powf(b),
                                _ => 0.0,
                            };
                            ValueTuple::from_real(res)
                        }
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                        let a = lv.1.or(lv.0.map(|v| v as f64)).unwrap_or(0.0);
                        let b = rv.1.or(rv.0.map(|v| v as f64)).unwrap_or(0.0);
                        let res = match op {
                            BinOp::Eq => a == b,
                            BinOp::Ne => a != b,
                            BinOp::Lt => a < b,
                            BinOp::Gt => a > b,
                            BinOp::Le => a <= b,
                            BinOp::Ge => a >= b,
                            _ => false,
                        };
                        ValueTuple::from_bool(res)
                    }
                    BinOp::And => {
                        ValueTuple::from_bool(lv.2.unwrap_or(false) && rv.2.unwrap_or(false))
                    }
                    BinOp::Or => {
                        ValueTuple::from_bool(lv.2.unwrap_or(false) || rv.2.unwrap_or(false))
                    }
                    BinOp::Eqv => {
                        ValueTuple::from_bool(lv.2.unwrap_or(false) == rv.2.unwrap_or(false))
                    }
                    BinOp::Neqv => {
                        ValueTuple::from_bool(lv.2.unwrap_or(false) != rv.2.unwrap_or(false))
                    }
                    BinOp::Concat => {
                        let ls = lv.3.unwrap_or_else(|| {
                            lv.0.map(|v| v.to_string())
                                .or(lv.1.map(|f| format!("{}", f)))
                                .unwrap_or_default()
                        });
                        let rs = rv.3.unwrap_or_else(|| {
                            rv.0.map(|v| v.to_string())
                                .or(rv.1.map(|f| format!("{}", f)))
                                .unwrap_or_default()
                        });
                        ValueTuple::from_str(format!("{}{}", ls, rs))
                    }
                }
            }
            Expr::Call(name, args) => {
                if let Some(fd) = funcs.get(name) {
                    let mut arg_vals: Vec<ValueTuple> = Vec::new();
                    for a in args {
                        arg_vals.push(eval_expr(
                            a, env_str, env_int, env_real32, env_real64, env_log, funcs, call_stack,
                        ));
                    }
                    let mut frame = CallFrame::new(fd, Some(name.clone()));
                    for (i, pn) in fd.params.iter().enumerate() {
                        let av = arg_vals.get(i);
                        if let Some(v) = av {
                            if let Some(iv) = v.0 {
                                frame.env_int.insert(pn.clone(), iv);
                            } else if let Some(fv) = v.1 {
                                frame.env_real64.insert(pn.clone(), fv);
                            } else if let Some(bv) = v.2 {
                                frame.env_log.insert(pn.clone(), bv);
                            } else if let Some(sv) = v.3.clone() {
                                frame.env_str.insert(pn.clone(), sv);
                            } else {
                                frame.env_int.insert(pn.clone(), 0);
                            }
                        } else {
                            frame.env_int.insert(pn.clone(), 0);
                        }
                    }
                    if fd.is_function {
                        frame.env_int.insert(name.clone(), 0);
                    }
                    call_stack.push(frame);
                    let ret = exec_stmts(&fd.body, funcs, call_stack);
                    let frame = call_stack.pop().unwrap();
                    if let Some(rv) = ret {
                        return rv;
                    }
                    if fd.is_function {
                        if let Some(v) = frame.env_int.get(name) {
                            return ValueTuple::from_int(*v);
                        }
                    }
                    ValueTuple::from_int(0)
                } else {
                    // Try intrinsic implementations (case-insensitive)
                    let mut arg_vals: Vec<ValueTuple> = Vec::new();
                    for a in args {
                        arg_vals.push(eval_expr(
                            a, env_str, env_int, env_real32, env_real64, env_log, funcs, call_stack,
                        ));
                    }
                    if let Some(v) = handle_intrinsic(name, &arg_vals) {
                        return v;
                    }
                    ValueTuple::from_int(0)
                }
            }
        }
    }

    // Intrinsic function implementations for the interpreter. These are
    // best-effort implementations covering numeric, logical, string and bit
    // operations used by tests. Names are matched case-insensitively.
    fn to_real(v: &ValueTuple) -> Option<f64> {
        v.1.or(v.0.map(|i| i as f64))
    }
    fn to_int(v: &ValueTuple) -> Option<i64> {
        v.0.or(v.1.map(|f| f as i64))
    }

    fn handle_intrinsic(name: &str, args: &[ValueTuple]) -> Option<ValueTuple> {
        let n = name.to_ascii_lowercase();
        match n.as_str() {
            // Numeric
            "abs" => {
                if let Some(f) = to_real(args.get(0)?) {
                    Some(ValueTuple::from_real(f.abs()))
                } else {
                    Some(ValueTuple::from_int(
                        to_int(args.get(0)?).unwrap_or(0).abs(),
                    ))
                }
            }
            "aint" | "anint" | "nint" => {
                if let Some(f) = to_real(args.get(0)?) {
                    Some(ValueTuple::from_int(f.trunc() as i64))
                } else {
                    Some(ValueTuple::from_int(to_int(args.get(0)?)?))
                }
            }
            "ceiling" => {
                if let Some(f) = to_real(args.get(0)?) {
                    Some(ValueTuple::from_int(f.ceil() as i64))
                } else {
                    Some(ValueTuple::from_int(to_int(args.get(0)?)?))
                }
            }
            "floor" => {
                if let Some(f) = to_real(args.get(0)?) {
                    Some(ValueTuple::from_int(f.floor() as i64))
                } else {
                    Some(ValueTuple::from_int(to_int(args.get(0)?)?))
                }
            }
            "sqrt" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.sqrt())),
            "exp" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.exp())),
            "log" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.ln())),
            "log10" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.log10())),
            "sin" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.sin())),
            "cos" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.cos())),
            "tan" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.tan())),
            "asin" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.asin())),
            "acos" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.acos())),
            "atan" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.atan())),
            "atan2" => {
                let y = to_real(args.get(0)?)?;
                let x = to_real(args.get(1)?)?;
                Some(ValueTuple::from_real(y.atan2(x)))
            }
            "sinh" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.sinh())),
            "cosh" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.cosh())),
            "tanh" => to_real(args.get(0)?).map(|f| ValueTuple::from_real(f.tanh())),
            "dim" => {
                let a = to_real(args.get(0)?)?;
                let b = to_real(args.get(1)?)?;
                Some(ValueTuple::from_real((a - b).max(0.0)))
            }
            "dprod" => {
                if args.len() >= 2 {
                    Some(ValueTuple::from_real(
                        to_real(args.get(0)?)? * to_real(args.get(1)?)?,
                    ))
                } else {
                    None
                }
            }
            "mod" => {
                if to_int(args.get(0)?).is_some() && to_int(args.get(1)?).is_some() {
                    let a = to_int(args.get(0)?)?;
                    let b = to_int(args.get(1)?)?;
                    if b == 0 {
                        Some(ValueTuple::from_int(0))
                    } else {
                        Some(ValueTuple::from_int(a % b))
                    }
                } else {
                    let a = to_real(args.get(0)?)?;
                    let b = to_real(args.get(1)?)?;
                    if b == 0.0 {
                        Some(ValueTuple::from_real(0.0))
                    } else {
                        Some(ValueTuple::from_real(a % b))
                    }
                }
            }
            "modulo" => {
                let a = to_real(args.get(0)?)?;
                let b = to_real(args.get(1)?)?;
                if b == 0.0 {
                    Some(ValueTuple::from_real(0.0))
                } else {
                    Some(ValueTuple::from_real(a - (a / b).floor() * b))
                }
            }
            "int" => Some(ValueTuple::from_int(to_int(args.get(0)?)?)),
            "real" | "dble" => Some(ValueTuple::from_real(to_real(args.get(0)?)?)),
            "sign" => {
                let a = to_real(args.get(0)?)?;
                let b = to_real(args.get(1)?)?;
                Some(ValueTuple::from_real(
                    a.abs() * if b.is_sign_negative() { -1.0 } else { 1.0 },
                ))
            }
            // Numeric inquiries (best-effort)
            "epsilon" => Some(ValueTuple::from_real(f64::EPSILON)),
            "huge" => Some(ValueTuple::from_real(f64::MAX)),
            "tiny" => Some(ValueTuple::from_real(f64::MIN_POSITIVE)),
            "digits" => Some(ValueTuple::from_int(15)),
            "precision" => Some(ValueTuple::from_int(6)),
            "radix" => Some(ValueTuple::from_int(2)),
            // Logical / reductions
            "all" => Some(ValueTuple::from_bool(
                args.get(0).map(|v| v.2.unwrap_or(false)).unwrap_or(false),
            )),
            "any" => Some(ValueTuple::from_bool(
                args.get(0).map(|v| v.2.unwrap_or(false)).unwrap_or(false),
            )),
            "count" => Some(ValueTuple::from_int(
                if args.get(0).map(|v| v.2.unwrap_or(false)).unwrap_or(false) {
                    1
                } else {
                    0
                },
            )),
            // Simple array reductions / fallbacks: treat scalar as single-element
            "sum" => {
                if let Some(r) = to_real(args.get(0)?) {
                    Some(ValueTuple::from_real(r))
                } else {
                    Some(ValueTuple::from_int(to_int(args.get(0)?)?))
                }
            }
            "product" => {
                if let Some(r) = to_real(args.get(0)?) {
                    Some(ValueTuple::from_real(r))
                } else {
                    Some(ValueTuple::from_int(to_int(args.get(0)?)?))
                }
            }
            "maxval" | "minval" | "max" | "min" => {
                // scalar case: return the argument
                if let Some(r) = to_real(args.get(0)?) {
                    Some(ValueTuple::from_real(r))
                } else {
                    Some(ValueTuple::from_int(to_int(args.get(0)?)?))
                }
            }
            // Character / string
            "len" => args
                .get(0)
                .and_then(|v| v.3.as_ref().map(|s| ValueTuple::from_int(s.len() as i64))),
            "len_trim" => args.get(0).and_then(|v| {
                v.3.as_ref()
                    .map(|s| ValueTuple::from_int(s.trim_end().len() as i64))
            }),
            "achar" => args.get(0).and_then(|v| {
                to_int(v).map(|i| ValueTuple::from_str((i as u8 as char).to_string()))
            }),
            "ichar" | "iachar" => args.get(0).and_then(|v| {
                v.3.as_ref()
                    .map(|s| ValueTuple::from_int(s.bytes().next().unwrap_or(0) as i64))
            }),
            "trim" => args.get(0).and_then(|v| {
                v.3.as_ref()
                    .map(|s| ValueTuple::from_str(s.trim_end().to_string()))
            }),
            "repeat" => {
                if let Some(s) = args.get(0).and_then(|v| v.3.clone()) {
                    let n = to_int(args.get(1)?)?;
                    Some(ValueTuple::from_str(s.repeat(n as usize)))
                } else {
                    None
                }
            }
            "index" => {
                if let (Some(hay), Some(needle)) = (
                    args.get(0).and_then(|v| v.3.clone()),
                    args.get(1).and_then(|v| v.3.clone()),
                ) {
                    let idx = hay.find(&needle).map(|i| (i + 1) as i64).unwrap_or(0);
                    Some(ValueTuple::from_int(idx))
                } else {
                    None
                }
            }
            // Bitwise/integer ops
            "btest" => {
                let a = to_int(args.get(0)?)?;
                let pos = to_int(args.get(1)?)?;
                Some(ValueTuple::from_bool(((a >> pos) & 1) != 0))
            }
            "iand" => Some(ValueTuple::from_int(
                to_int(args.get(0)?)? & to_int(args.get(1)?)?,
            )),
            "ior" => Some(ValueTuple::from_int(
                to_int(args.get(0)?)? | to_int(args.get(1)?)?,
            )),
            "ieor" => Some(ValueTuple::from_int(
                to_int(args.get(0)?)? ^ to_int(args.get(1)?)?,
            )),
            "not" => Some(ValueTuple::from_int(!to_int(args.get(0)?)?)),
            "ibset" => {
                let a = to_int(args.get(0)?)?;
                let pos = to_int(args.get(1)?)?;
                Some(ValueTuple::from_int(a | (1 << pos)))
            }
            "ibclr" => {
                let a = to_int(args.get(0)?)?;
                let pos = to_int(args.get(1)?)?;
                Some(ValueTuple::from_int(a & !(1 << pos)))
            }
            "ibits" => {
                let a = to_int(args.get(0)?)?;
                let pos = to_int(args.get(1)?)? as u32;
                let len = to_int(args.get(2).unwrap_or(&ValueTuple::from_int(1)))? as u32;
                let mask = if len >= 64 {
                    u64::MAX
                } else {
                    (1u64 << len) - 1
                };
                Some(ValueTuple::from_int(
                    ((a as i64 as u64 >> pos) & mask) as i64,
                ))
            }
            "ishft" => {
                let a = to_int(args.get(0)?)?;
                let sh = to_int(args.get(1)?)?;
                if sh >= 0 {
                    Some(ValueTuple::from_int(a << sh))
                } else {
                    Some(ValueTuple::from_int((a as u64 >> (-sh) as u32) as i64))
                }
            }
            // Fallbacks for dot_product/matmul on scalars
            "dot_product" | "matmul" => {
                if args.len() >= 2 {
                    Some(ValueTuple::from_real(
                        to_real(args.get(0)?)? * to_real(args.get(1)?)?,
                    ))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn format_real(f: f64, double: bool) -> String {
        if f.fract() == 0.0 {
            format!("{:.1}", f)
        } else if double {
            format!("{:.15}", f)
        } else {
            format!("{:.6}", f)
        }
    }

    fn eval_logical(
        e: &Expr,
        env_log: &HashMap<String, bool>,
        env_int: &HashMap<String, i64>,
        env_real64: &HashMap<String, f64>,
        env_real32: &HashMap<String, f32>,
        env_str: &HashMap<String, String>,
        funcs: &HashMap<String, FuncDef>,
        call_stack: &mut Vec<CallFrame>,
    ) -> bool {
        match e {
            Expr::Logical(b) => *b,
            Expr::Ident(id) => *env_log.get(id).unwrap_or(&false),
            _ => {
                let vt = eval_expr(
                    e, env_str, env_int, env_real32, env_real64, env_log, funcs, call_stack,
                );
                vt.2.unwrap_or(false)
            }
        }
    }

    #[derive(Clone, Debug)]
    struct CallFrame {
        env_str: HashMap<String, String>,
        env_int: HashMap<String, i64>,
        env_real32: HashMap<String, f32>,
        env_real64: HashMap<String, f64>,
        env_log: HashMap<String, bool>,
        func_name: Option<String>,
        is_function: bool,
    }
    impl CallFrame {
        fn new(fd: &FuncDef, func_name: Option<String>) -> Self {
            Self {
                env_str: HashMap::new(),
                env_int: HashMap::new(),
                env_real32: HashMap::new(),
                env_real64: HashMap::new(),
                env_log: HashMap::new(),
                func_name,
                is_function: fd.is_function,
            }
        }
    }
    fn exec_stmts(
        stmts: &[Stmt],
        funcs: &HashMap<String, FuncDef>,
        call_stack: &mut Vec<CallFrame>,
    ) -> Option<ValueTuple> {
        let mut idx = 0;
        while idx < stmts.len() {
            let stmt = &stmts[idx];
            match stmt {
                Stmt::VarDecl { kind, names } => {
                    let frame = call_stack.last_mut().unwrap();
                    for n in names {
                        if frame.env_int.contains_key(n)
                            || frame.env_real32.contains_key(n)
                            || frame.env_real64.contains_key(n)
                            || frame.env_log.contains_key(n)
                            || frame.env_str.contains_key(n)
                        {
                            continue;
                        }
                        match kind {
                            TypeSpec::Integer(_) => {
                                frame.env_int.entry(n.clone()).or_insert(0);
                            }
                            TypeSpec::Real => {
                                frame.env_real32.entry(n.clone()).or_insert(0.0);
                            }
                            TypeSpec::DoublePrecision => {
                                frame.env_real64.entry(n.clone()).or_insert(0.0);
                            }
                            TypeSpec::Character(_) => {
                                frame.env_str.entry(n.clone()).or_insert(String::new());
                            }
                            TypeSpec::Logical => {
                                frame.env_log.entry(n.clone()).or_insert(false);
                            }
                        }
                    }
                }
                Stmt::Read { args } => {
                    // Handle READ as syntactic sugar for CALL read(args...)
                    let fr = call_stack.last_mut().unwrap();
                    use std::io;
                    let mut input = String::new();
                    for arg in args {
                        input.clear();
                        io::stdin().read_line(&mut input).ok();
                        let tok = input.trim_end();
                        if let Expr::Ident(id) = arg {
                            if fr.env_int.contains_key(id) {
                                let v = parse_i64_lossy(tok);
                                fr.env_int.insert(id.clone(), v);
                            } else if fr.env_real64.contains_key(id) {
                                let v = parse_f64_lossy(tok);
                                fr.env_real64.insert(id.clone(), v);
                            } else if fr.env_real32.contains_key(id) {
                                let v = parse_f64_lossy(tok) as f32;
                                fr.env_real32.insert(id.clone(), v);
                            } else if fr.env_log.contains_key(id) {
                                let v = matches!(
                                    tok.to_ascii_lowercase().as_str(),
                                    ".true." | "true" | "1"
                                );
                                fr.env_log.insert(id.clone(), v);
                            } else if fr.env_str.contains_key(id) {
                                fr.env_str.insert(id.clone(), tok.to_string());
                            } else {
                                let v = parse_i64_lossy(tok);
                                fr.env_int.insert(id.clone(), v);
                            }
                        }
                    }
                }
                Stmt::Assign { name, value } => {
                    let (strs, ints, r32, r64, logs) = {
                        let fr = call_stack.last().unwrap();
                        (
                            fr.env_str.clone(),
                            fr.env_int.clone(),
                            fr.env_real32.clone(),
                            fr.env_real64.clone(),
                            fr.env_log.clone(),
                        )
                    };
                    let vt = eval_expr(value, &strs, &ints, &r32, &r64, &logs, funcs, call_stack);
                    let frame = call_stack.last_mut().unwrap();
                    if let Some(s) = vt.3 {
                        frame.env_str.insert(name.clone(), s);
                        frame.env_int.remove(name);
                    } else if let Some(i) = vt.0 {
                        frame.env_int.insert(name.clone(), i);
                    } else if let Some(f) = vt.1 {
                        if frame.env_int.contains_key(name) {
                            frame.env_int.insert(name.clone(), f as i64);
                        } else if frame.env_real64.contains_key(name) {
                            frame.env_real64.insert(name.clone(), f);
                        } else if frame.env_real32.contains_key(name) {
                            frame.env_real32.insert(name.clone(), f as f32);
                        } else {
                            frame.env_real64.insert(name.clone(), f);
                        }
                    } else if let Some(b) = vt.2 {
                        frame.env_log.insert(name.clone(), b);
                    }
                }
                Stmt::Print { items } => {
                    let (strs, ints, r32, r64, logs) = {
                        let fr = call_stack.last().unwrap();
                        (
                            fr.env_str.clone(),
                            fr.env_int.clone(),
                            fr.env_real32.clone(),
                            fr.env_real64.clone(),
                            fr.env_log.clone(),
                        )
                    };
                    let mut parts = Vec::new();
                    for it in items {
                        match it {
                            Expr::Str(s) => parts.push(s.clone()),
                            Expr::IntLit(s) => parts.push(s.clone()),
                            Expr::RealLit(s) => {
                                let f = parse_f64_lossy(s);
                                parts.push(format_real(f, false));
                            }
                            Expr::Logical(b) => parts.push(if *b {
                                ".TRUE.".into()
                            } else {
                                ".FALSE.".into()
                            }),
                            Expr::Ident(id) => {
                                if let Some(v) = r64.get(id) {
                                    parts.push(format_real(*v, true));
                                } else if let Some(v) = r32.get(id) {
                                    parts.push(format_real(*v as f64, false));
                                } else if let Some(v) = ints.get(id) {
                                    parts.push(v.to_string());
                                } else if let Some(v) = logs.get(id) {
                                    parts.push(if *v {
                                        ".TRUE.".into()
                                    } else {
                                        ".FALSE.".into()
                                    });
                                } else if let Some(v) = strs.get(id) {
                                    parts.push(v.clone());
                                } else {
                                    parts.push("0".into());
                                }
                            }
                            _ => {
                                let vt = eval_expr(
                                    it, &strs, &ints, &r32, &r64, &logs, funcs, call_stack,
                                );
                                if let Some(s) = vt.3 {
                                    parts.push(s);
                                } else if let Some(f) = vt.1 {
                                    parts.push(format_real(f, false));
                                } else if let Some(i) = vt.0 {
                                    parts.push(i.to_string());
                                } else if let Some(b) = vt.2 {
                                    parts.push(if b { ".TRUE.".into() } else { ".FALSE.".into() });
                                }
                            }
                        }
                    }
                    println!("{}", parts.join(" "));
                }
                Stmt::If {
                    cond,
                    then_body,
                    else_body,
                } => {
                    let (strs, ints, r32, r64, logs) = {
                        let fr = call_stack.last().unwrap();
                        (
                            fr.env_str.clone(),
                            fr.env_int.clone(),
                            fr.env_real32.clone(),
                            fr.env_real64.clone(),
                            fr.env_log.clone(),
                        )
                    };
                    let cond_val =
                        eval_logical(cond, &logs, &ints, &r64, &r32, &strs, funcs, call_stack);
                    if cond_val {
                        if let Some(ret) = exec_stmts(then_body, funcs, call_stack) {
                            return Some(ret);
                        }
                    } else if let Some(eb) = else_body {
                        if let Some(ret) = exec_stmts(eb, funcs, call_stack) {
                            return Some(ret);
                        }
                    }
                }
                Stmt::Do {
                    var,
                    start,
                    end,
                    body,
                } => {
                    let (strs, ints, r32, r64, logs) = {
                        let fr = call_stack.last().unwrap();
                        (
                            fr.env_str.clone(),
                            fr.env_int.clone(),
                            fr.env_real32.clone(),
                            fr.env_real64.clone(),
                            fr.env_log.clone(),
                        )
                    };
                    let sv = eval_expr(start, &strs, &ints, &r32, &r64, &logs, funcs, call_stack);
                    let ev = eval_expr(end, &strs, &ints, &r32, &r64, &logs, funcs, call_stack);
                    let s = sv.0.or(sv.1.map(|f| f as i64)).unwrap_or(0);
                    let e = ev.0.or(ev.1.map(|f| f as i64)).unwrap_or(0);
                    for i in s..=e {
                        {
                            let frame = call_stack.last_mut().unwrap();
                            frame.env_int.insert(var.clone(), i);
                        }
                        if let Some(ret) = exec_stmts(body, funcs, call_stack) {
                            return Some(ret);
                        }
                    }
                }
                Stmt::Return(expr_opt) => {
                    let (strs, ints, r32, r64, logs) = {
                        let fr = call_stack.last().unwrap();
                        (
                            fr.env_str.clone(),
                            fr.env_int.clone(),
                            fr.env_real32.clone(),
                            fr.env_real64.clone(),
                            fr.env_log.clone(),
                        )
                    };
                    if let Some(e) = expr_opt {
                        let vt = eval_expr(e, &strs, &ints, &r32, &r64, &logs, funcs, call_stack);
                        return Some(vt);
                    } else {
                        let fr = call_stack.last().unwrap();
                        if fr.is_function {
                            if let Some(fname) = &fr.func_name {
                                if let Some(v) = fr.env_int.get(fname) {
                                    return Some(ValueTuple::from_int(*v));
                                }
                            }
                        }
                        return Some(ValueTuple::from_int(0));
                    }
                }
                Stmt::CallSub { name, args } => {
                    if let Some(fd) = funcs.get(name) {
                        let (strs, ints, r32, r64, logs) = {
                            let fr = call_stack.last().unwrap();
                            (
                                fr.env_str.clone(),
                                fr.env_int.clone(),
                                fr.env_real32.clone(),
                                fr.env_real64.clone(),
                                fr.env_log.clone(),
                            )
                        };
                        let mut sub_frame = CallFrame::new(fd, None);
                        for (i, pn) in fd.params.iter().enumerate() {
                            if let Some(arg) = args.get(i) {
                                let vt = eval_expr(
                                    arg, &strs, &ints, &r32, &r64, &logs, funcs, call_stack,
                                );
                                if let Some(iv) = vt.0 {
                                    sub_frame.env_int.insert(pn.clone(), iv);
                                } else if let Some(fv) = vt.1 {
                                    sub_frame.env_real64.insert(pn.clone(), fv);
                                } else if let Some(bv) = vt.2 {
                                    sub_frame.env_log.insert(pn.clone(), bv);
                                } else if let Some(sv) = vt.3 {
                                    sub_frame.env_str.insert(pn.clone(), sv);
                                }
                            }
                        }
                        call_stack.push(sub_frame);
                        let _ = exec_stmts(&fd.body, funcs, call_stack);
                        call_stack.pop();
                    }
                    // READ handling: special call named "read" with expr args (should be idents)
                    if name.to_ascii_lowercase() == "read" {
                        // read from stdin for each arg; support integers, reals, and strings
                        let fr = call_stack.last_mut().unwrap();
                        use std::io;
                        let mut input = String::new();
                        for arg in args {
                            // simple prompt suppression: don't print prompt
                            input.clear();
                            io::stdin().read_line(&mut input).ok();
                            let tok = input.trim_end();
                            match arg {
                                Expr::Ident(id) => {
                                    if fr.env_int.contains_key(id) {
                                        let v = parse_i64_lossy(tok);
                                        fr.env_int.insert(id.clone(), v);
                                    } else if fr.env_real64.contains_key(id) {
                                        let v = parse_f64_lossy(tok);
                                        fr.env_real64.insert(id.clone(), v);
                                    } else if fr.env_real32.contains_key(id) {
                                        let v = parse_f64_lossy(tok) as f32;
                                        fr.env_real32.insert(id.clone(), v);
                                    } else if fr.env_log.contains_key(id) {
                                        let v = matches!(
                                            tok.to_ascii_lowercase().as_str(),
                                            ".true." | "true" | "1"
                                        );
                                        fr.env_log.insert(id.clone(), v);
                                    } else if fr.env_str.contains_key(id) {
                                        fr.env_str.insert(id.clone(), tok.to_string());
                                    } else {
                                        // default to integer
                                        let v = parse_i64_lossy(tok);
                                        fr.env_int.insert(id.clone(), v);
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
                Stmt::Function { .. } | Stmt::Subroutine { .. } => {}
                Stmt::Use { .. } | Stmt::Module { .. } => {}
                Stmt::ImplicitNone => {}
            }
            idx += 1;
        }
        None
    }
    let top_frame = CallFrame {
        env_str,
        env_int,
        env_real32,
        env_real64,
        env_log,
        func_name: None,
        is_function: false,
    };
    let mut call_stack = vec![top_frame];
    let _ = exec_stmts(&program.body, &funcs, &mut call_stack);
}

// C-callable runtime helpers used by compiled codegen (Cranelift) to perform
// portable list-directed reads. These have a simple ABI and are safe to call
// from generated code in the same process.
#[no_mangle]
pub extern "C" fn f90c_read_i64(out: *mut i64) -> i32 {
    use std::io;
    if out.is_null() {
        return -1;
    }
    let mut input = String::new();
    if io::stdin().read_line(&mut input).is_err() {
        return -1;
    }
    let tok = input.trim();
    let v = i128::from_str_radix(tok, 10).ok().and_then(|x| i64::try_from(x).ok()).unwrap_or(0);
    unsafe {
        *out = v;
    }
    0
}

#[no_mangle]
pub extern "C" fn f90c_read_f64(out: *mut f64) -> i32 {
    use std::io;
    if out.is_null() {
        return -1;
    }
    let mut input = String::new();
    if io::stdin().read_line(&mut input).is_err() {
        return -1;
    }
    let tok = input.trim();
    let v = tok.parse::<f64>().unwrap_or(0.0);
    unsafe {
        *out = v;
    }
    0
}

#[no_mangle]
pub extern "C" fn f90c_read_bool(out: *mut u8) -> i32 {
    use std::io;
    if out.is_null() {
        return -1;
    }
    let mut input = String::new();
    if io::stdin().read_line(&mut input).is_err() {
        return -1;
    }
    let tok = input.trim().to_ascii_lowercase();
    let v = matches!(tok.as_str(), ".true." | "true" | "1");
    unsafe {
        *out = if v { 1 } else { 0 };
    }
    0
}

#[no_mangle]
pub extern "C" fn f90c_read_str(out: *mut u8, len: usize) -> i32 {
    use std::io;
    if out.is_null() || len == 0 {
        return -1;
    }
    // no debug prints
    let mut input = String::new();
    if io::stdin().read_line(&mut input).is_err() {
        return -1;
    }
    let tok = input.trim_end();
    let bytes = tok.as_bytes();
    let copy_len = std::cmp::min(len.saturating_sub(1), bytes.len());
    unsafe {
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), out, copy_len);
        *out.add(copy_len) = 0; // null terminate
    }
    0
}

pub fn run_executable(path: &std::path::Path) -> Result<(), std::io::Error> {
    use std::process::Command;

    let status = Command::new(path).status()?;

    if status.success() {
        Ok(())
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "Executable failed",
        ))
    }
}
