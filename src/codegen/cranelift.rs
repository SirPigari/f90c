use anyhow::Result;
use cranelift_codegen::{isa, settings};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use super::Backend;
use crate::ir::{IExpr, IStmt, IcCaseItem, Module};

use cranelift_codegen::ir;
use cranelift_codegen::ir::InstBuilder; // bring trait into scope
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, Linkage, Module as _};
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU32, Ordering};
static GLOBAL_STR_ID: AtomicU32 = AtomicU32::new(0);

pub struct CraneliftBackend {
    pub triple: Option<Triple>,
    pub flags: settings::Flags,
}
impl Default for CraneliftBackend {
    fn default() -> Self {
        Self {
            triple: None,
            flags: settings::Flags::new(settings::builder()),
        }
    }
}
impl CraneliftBackend {
    pub fn with_flags(flags: settings::Flags) -> Self {
        Self {
            triple: None,
            flags,
        }
    }
}
enum CgVal {
    I64(ir::Value),
    F64(ir::Value),
    Bool(ir::Value),
}

#[derive(Clone, Copy)]
enum RetKind {
    Void,
    I64,
    F64,
}

#[derive(Clone, Copy)]
enum ParamKind {
    I64,
    F64,
}

fn to_f64(b: &mut FunctionBuilder, v: CgVal) -> ir::Value {
    use ir::types;
    match v {
        CgVal::F64(x) => x,
        CgVal::I64(x) => b.ins().fcvt_from_sint(types::F64, x),
        CgVal::Bool(x) => {
            let one = b.ins().iconst(types::I64, 1);
            let zero = b.ins().iconst(types::I64, 0);
            let i = b.ins().select(x, one, zero);
            b.ins().fcvt_from_sint(types::F64, i)
        }
    }
}
fn to_i64(b: &mut FunctionBuilder, v: CgVal) -> ir::Value {
    use ir::types;
    match v {
        CgVal::I64(x) => x,
        CgVal::F64(x) => b.ins().fcvt_to_sint(types::I64, x),
        CgVal::Bool(x) => {
            let one = b.ins().iconst(types::I64, 1);
            let zero = b.ins().iconst(types::I64, 0);
            b.ins().select(x, one, zero)
        }
    }
}
fn to_bool(b: &mut FunctionBuilder, v: CgVal) -> ir::Value {
    use ir::condcodes::{FloatCC, IntCC};
    use ir::types;
    match v {
        CgVal::Bool(x) => x,
        CgVal::I64(x) => {
            let z = b.ins().iconst(types::I64, 0);
            b.ins().icmp(IntCC::NotEqual, x, z)
        }
        CgVal::F64(x) => {
            let z = b.ins().f64const(0.0);
            b.ins().fcmp(FloatCC::NotEqual, x, z)
        }
    }
}

struct Externs {
    printf: ir::FuncRef,
    sprintf: ir::FuncRef,
    gcvt: ir::FuncRef,
    strcmp: ir::FuncRef,
    pow: ir::FuncRef,
    scanf: ir::FuncRef,
    // memcpy removed
    fmt_s: ir::Value,
    fmt_f64: ir::Value,
    fmt_i64: ir::Value,
    fmt_i64_dot0: ir::Value,
    p_true: ir::Value,
    p_false: ir::Value,
    p_nl: ir::Value,
}
fn make_data(
    module: &mut ObjectModule,
    b: &mut FunctionBuilder,
    ptr_ty: ir::Type,
    _counter: &mut u32,
    bytes: &[u8],
) -> Result<ir::Value> {
    let mut raw = bytes.to_vec();
    if *raw.last().unwrap_or(&0) != 0 {
        raw.push(0);
    }
    let id_num = GLOBAL_STR_ID.fetch_add(1, Ordering::Relaxed);
    let name = format!(".str.cst.{}", id_num);
    let id = module.declare_data(&name, Linkage::Local, false, false)?;
    let mut dd = DataDescription::new();
    dd.define(raw.into_boxed_slice());
    module.define_data(id, &dd)?;
    let gv = module.declare_data_in_func(id, b.func);
    Ok(b.ins().global_value(ptr_ty, gv))
}

fn declare_externs(
    module: &mut ObjectModule,
    b: &mut FunctionBuilder,
    ptr_ty: ir::Type,
) -> Result<Externs> {
    use ir::{types, AbiParam, Signature};
    let cc = module.isa().default_call_conv();
    let mut sig_printf = Signature::new(cc);
    sig_printf.params.push(AbiParam::new(ptr_ty));
    sig_printf.params.push(AbiParam::new(ptr_ty));
    sig_printf.returns.push(AbiParam::new(types::I32));
    let printf_id = module.declare_function("printf", Linkage::Import, &sig_printf)?;
    let printf_ref = module.declare_func_in_func(printf_id, b.func);
    let mut sig_sprintf = Signature::new(cc);
    sig_sprintf.params.push(AbiParam::new(ptr_ty));
    sig_sprintf.params.push(AbiParam::new(ptr_ty));
    sig_sprintf.params.push(AbiParam::new(types::I64));
    sig_sprintf.returns.push(AbiParam::new(types::I32));
    let sprintf_id = module.declare_function("sprintf", Linkage::Import, &sig_sprintf)?;
    let sprintf_ref = module.declare_func_in_func(sprintf_id, b.func);
    let mut sig_gcvt = Signature::new(cc);
    sig_gcvt.params.push(AbiParam::new(types::F64));
    sig_gcvt.params.push(AbiParam::new(types::I32));
    sig_gcvt.params.push(AbiParam::new(ptr_ty));
    sig_gcvt.returns.push(AbiParam::new(ptr_ty));
    let gcvt_id = module.declare_function("_gcvt", Linkage::Import, &sig_gcvt)?;
    let gcvt_ref = module.declare_func_in_func(gcvt_id, b.func);
    let mut sig_pow = Signature::new(cc);
    sig_pow.params.push(AbiParam::new(types::F64));
    sig_pow.params.push(AbiParam::new(types::F64));
    sig_pow.returns.push(AbiParam::new(types::F64));
    let pow_id = module.declare_function("pow", Linkage::Import, &sig_pow)?;
    let pow_ref = module.declare_func_in_func(pow_id, b.func);
    // declare scanf (format pointer + argument pointer)
    let mut sig_scanf = Signature::new(cc);
    sig_scanf.params.push(AbiParam::new(ptr_ty));
    sig_scanf.params.push(AbiParam::new(ptr_ty));
    sig_scanf.returns.push(AbiParam::new(types::I32));
    let scanf_id = module.declare_function("scanf", Linkage::Import, &sig_scanf)?;
    let scanf_ref = module.declare_func_in_func(scanf_id, b.func);
    // strcmp import (C library)
    let mut sig_strcmp = Signature::new(cc);
    sig_strcmp.params.push(AbiParam::new(ptr_ty));
    sig_strcmp.params.push(AbiParam::new(ptr_ty));
    sig_strcmp.returns.push(AbiParam::new(types::I32));
    let strcmp_id = module.declare_function("strcmp", Linkage::Import, &sig_strcmp)?;
    let strcmp_ref = module.declare_func_in_func(strcmp_id, b.func);
    // no memcpy import
    let mut c = 0u32;
    let fmt_s = make_data(module, b, ptr_ty, &mut c, b"%s")?;
    let fmt_f64 = make_data(module, b, ptr_ty, &mut c, b"%lf")?;
    let fmt_i64 = make_data(module, b, ptr_ty, &mut c, b"%lld")?;
    let fmt_i64_dot0 = make_data(module, b, ptr_ty, &mut c, b"%lld.0")?;
    let p_true = make_data(module, b, ptr_ty, &mut c, b".TRUE.")?;
    let p_false = make_data(module, b, ptr_ty, &mut c, b".FALSE.")?;
    let p_nl = make_data(module, b, ptr_ty, &mut c, b"\n")?;
    Ok(Externs {
        printf: printf_ref,
        sprintf: sprintf_ref,
        gcvt: gcvt_ref,
        strcmp: strcmp_ref,
        pow: pow_ref,
        scanf: scanf_ref,
        fmt_s,
        fmt_f64,
        fmt_i64,
        fmt_i64_dot0,
        p_true,
        p_false,
        p_nl,
    })
}

fn int_to_str(
    b: &mut FunctionBuilder,
    ex: &Externs,
    ptr_ty: ir::Type,
    sprintf: ir::FuncRef,
    v: ir::Value,
) -> ir::Value {
    let buf = b.create_sized_stack_slot(ir::StackSlotData::new(
        ir::StackSlotKind::ExplicitSlot,
        32,
        0,
    ));
    let p = b.ins().stack_addr(ptr_ty, buf, 0);
    b.ins().call(sprintf, &[p, ex.fmt_i64, v]);
    p
}

fn eval_expr(
    b: &mut FunctionBuilder,
    ex: &Externs,
    ptr_ty: ir::Type,
    e: &IExpr,
    ints: &HashMap<String, ir::StackSlot>,
    reals: &HashMap<String, ir::StackSlot>,
    bools: &HashMap<String, ir::StackSlot>,
    func_meta: &HashMap<String, FuncMeta>,
) -> CgVal {
    use ir::{types, MemFlags};
    match e {
        IExpr::IntLit(s) => {
            let v = i128::from_str_radix(s, 10)
                .ok()
                .and_then(|v| i64::try_from(v).ok())
                .unwrap_or(0);
            CgVal::I64(b.ins().iconst(types::I64, v))
        }
        IExpr::RealLit(s) => {
            let v = s.parse::<f64>().unwrap_or(0.0);
            CgVal::F64(b.ins().f64const(v))
        }
        IExpr::Logical(bv) => {
            let z = b.ins().iconst(ir::types::I8, 0);
            if *bv {
                let o = b.ins().iconst(ir::types::I8, 1);
                CgVal::Bool(b.ins().icmp(ir::condcodes::IntCC::NotEqual, o, z))
            } else {
                CgVal::Bool(b.ins().icmp(ir::condcodes::IntCC::NotEqual, z, z))
            }
        }
        IExpr::Ident(id) => {
            if let Some(ss) = ints.get(id) {
                let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                CgVal::I64(b.ins().load(ir::types::I64, MemFlags::new(), a, 0))
            } else if let Some(ss) = reals.get(id) {
                let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                CgVal::F64(b.ins().load(ir::types::F64, MemFlags::new(), a, 0))
            } else if let Some(ss) = bools.get(id) {
                let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                let v = b.ins().load(ir::types::I8, MemFlags::new(), a, 0);
                let z = b.ins().iconst(ir::types::I8, 0);
                CgVal::Bool(b.ins().icmp(ir::condcodes::IntCC::NotEqual, v, z))
            } else {
                CgVal::I64(b.ins().iconst(ir::types::I64, 0))
            }
        }
        IExpr::Un(op, e1) => {
            let v = eval_expr(b, ex, ptr_ty, e1, ints, reals, bools, func_meta);
            match op {
                crate::ast::UnOp::Neg => match v {
                    CgVal::I64(x) => CgVal::I64(b.ins().ineg(x)),
                    CgVal::F64(x) => CgVal::F64(b.ins().fneg(x)),
                    CgVal::Bool(x) => {
                        let one = b.ins().iconst(ir::types::I64, 1);
                        let zero = b.ins().iconst(ir::types::I64, 0);
                        let x64 = b.ins().select(x, one, zero);
                        CgVal::I64(b.ins().ineg(x64))
                    }
                },
                crate::ast::UnOp::Not => {
                    let vb = to_bool(b, v);
                    let z0 = b.ins().iconst(ir::types::I8, 0);
                    let o1 = b.ins().iconst(ir::types::I8, 1);
                    let t = b.ins().icmp(ir::condcodes::IntCC::NotEqual, o1, z0);
                    CgVal::Bool(b.ins().bxor(vb, t))
                }
            }
        }
        IExpr::Bin(op, l, r) => {
            use crate::ast::BinOp;
            match op {
                BinOp::Concat => CgVal::I64(b.ins().iconst(ir::types::I64, 0)),
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Pow => {
                    let lv_raw = eval_expr(b, ex, ptr_ty, l, ints, reals, bools, func_meta);
                    let rv_raw = eval_expr(b, ex, ptr_ty, r, ints, reals, bools, func_meta);
                    match (op, &lv_raw, &rv_raw) {
                        (BinOp::Add, CgVal::I64(a), CgVal::I64(bv)) => {
                            CgVal::I64(b.ins().iadd(*a, *bv))
                        }
                        (BinOp::Sub, CgVal::I64(a), CgVal::I64(bv)) => {
                            CgVal::I64(b.ins().isub(*a, *bv))
                        }
                        (BinOp::Mul, CgVal::I64(a), CgVal::I64(bv)) => {
                            CgVal::I64(b.ins().imul(*a, *bv))
                        }
                        (BinOp::Div, CgVal::I64(a), CgVal::I64(bv)) => {
                            CgVal::I64(b.ins().sdiv(*a, *bv))
                        }
                        _ => {
                            let lv = to_f64(b, lv_raw);
                            let rv = to_f64(b, rv_raw);
                            let res = match op {
                                BinOp::Add => b.ins().fadd(lv, rv),
                                BinOp::Sub => b.ins().fsub(lv, rv),
                                BinOp::Mul => b.ins().fmul(lv, rv),
                                BinOp::Div => b.ins().fdiv(lv, rv),
                                BinOp::Pow => {
                                    let call = b.ins().call(ex.pow, &[lv, rv]);
                                    b.inst_results(call)[0]
                                }
                                _ => unreachable!(),
                            };
                            CgVal::F64(res)
                        }
                    }
                }
                BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                    use ir::condcodes::{FloatCC, IntCC};
                    let lv_raw = eval_expr(b, ex, ptr_ty, l, ints, reals, bools, func_meta);
                    let rv_raw = eval_expr(b, ex, ptr_ty, r, ints, reals, bools, func_meta);
                    match (&lv_raw, &rv_raw) {
                        (CgVal::I64(a), CgVal::I64(bv)) => {
                            let cc = match op {
                                BinOp::Eq => IntCC::Equal,
                                BinOp::Ne => IntCC::NotEqual,
                                BinOp::Lt => IntCC::SignedLessThan,
                                BinOp::Le => IntCC::SignedLessThanOrEqual,
                                BinOp::Gt => IntCC::SignedGreaterThan,
                                BinOp::Ge => IntCC::SignedGreaterThanOrEqual,
                                _ => unreachable!(),
                            };
                            CgVal::Bool(b.ins().icmp(cc, *a, *bv))
                        }
                        _ => {
                            let lv = to_f64(b, lv_raw);
                            let rv = to_f64(b, rv_raw);
                            let cc = match op {
                                BinOp::Eq => FloatCC::Equal,
                                BinOp::Ne => FloatCC::NotEqual,
                                BinOp::Lt => FloatCC::LessThan,
                                BinOp::Le => FloatCC::LessThanOrEqual,
                                BinOp::Gt => FloatCC::GreaterThan,
                                BinOp::Ge => FloatCC::GreaterThanOrEqual,
                                _ => unreachable!(),
                            };
                            CgVal::Bool(b.ins().fcmp(cc, lv, rv))
                        }
                    }
                }
                BinOp::And | BinOp::Or | BinOp::Eqv | BinOp::Neqv => {
                    let lv_raw = eval_expr(b, ex, ptr_ty, l, ints, reals, bools, func_meta);
                    let rv_raw = eval_expr(b, ex, ptr_ty, r, ints, reals, bools, func_meta);
                    let lv = to_bool(b, lv_raw);
                    let rv = to_bool(b, rv_raw);
                    match op {
                        BinOp::And => CgVal::Bool(b.ins().band(lv, rv)),
                        BinOp::Or => CgVal::Bool(b.ins().bor(lv, rv)),
                        BinOp::Eqv => {
                            let xor = b.ins().bxor(lv, rv);
                            let z0 = b.ins().iconst(ir::types::I8, 0);
                            let o1 = b.ins().iconst(ir::types::I8, 1);
                            let t = b.ins().icmp(ir::condcodes::IntCC::NotEqual, o1, z0);
                            CgVal::Bool(b.ins().bxor(xor, t))
                        }
                        BinOp::Neqv => CgVal::Bool(b.ins().bxor(lv, rv)),
                        _ => unreachable!(),
                    }
                }
            }
        }
        IExpr::Str(_) => CgVal::I64(b.ins().iconst(ir::types::I64, 0)),
        IExpr::Call(name, args) => {
            // Lower a few intrinsics directly to IR for performance and
            // correctness where possible (abs, int/real conversions, integer mod,
            // and simple bit ops). Otherwise fall back to imported functions via
            // func_meta (declared earlier based on import_arity/effect_ret).
            let lname = name.to_ascii_lowercase();
            match lname.as_str() {
                "abs" => {
                    let v = eval_expr(b, ex, ptr_ty, &args[0], ints, reals, bools, func_meta);
                    match v {
                        CgVal::I64(x) => {
                            let zero = b.ins().iconst(ir::types::I64, 0);
                            let neg = b.ins().ineg(x);
                            let is_neg =
                                b.ins().icmp(ir::condcodes::IntCC::SignedLessThan, x, zero);
                            let sel = b.ins().select(is_neg, neg, x);
                            CgVal::I64(sel)
                        }
                        CgVal::F64(x) => {
                            let neg = b.ins().fneg(x);
                            let zero = b.ins().f64const(0.0);
                            let is_neg = b.ins().fcmp(ir::condcodes::FloatCC::LessThan, x, zero);
                            let sel = b.ins().select(is_neg, neg, x);
                            CgVal::F64(sel)
                        }
                        CgVal::Bool(x) => {
                            let one = b.ins().iconst(ir::types::I64, 1);
                            let zero = b.ins().iconst(ir::types::I64, 0);
                            let sel = b.ins().select(x, one, zero);
                            CgVal::I64(sel)
                        }
                    }
                }
                "int" => {
                    let v = eval_expr(b, ex, ptr_ty, &args[0], ints, reals, bools, func_meta);
                    let iv = to_i64(b, v);
                    CgVal::I64(iv)
                }
                "real" | "dble" => {
                    let v = eval_expr(b, ex, ptr_ty, &args[0], ints, reals, bools, func_meta);
                    let fv = to_f64(b, v);
                    CgVal::F64(fv)
                }
                "mod" => {
                    // integer mod if both arguments are ints, otherwise float remainder via fmod
                    let a = eval_expr(b, ex, ptr_ty, &args[0], ints, reals, bools, func_meta);
                    let bval = eval_expr(b, ex, ptr_ty, &args[1], ints, reals, bools, func_meta);
                    match (a, bval) {
                        (CgVal::I64(ai), CgVal::I64(bi)) => {
                            let r = b.ins().srem(ai, bi);
                            CgVal::I64(r)
                        }
                        (aa, bb) => {
                            let af = to_f64(b, aa);
                            let bf = to_f64(b, bb);
                            // no native frem in Cranelift; use a-b*trunc(a/b)
                            let div = b.ins().fdiv(af, bf);
                            let trunc = b.ins().fcvt_to_sint(ir::types::I64, div);
                            let back = b.ins().fcvt_from_sint(ir::types::F64, trunc);
                            let prod = b.ins().fmul(bf, back);
                            let res = b.ins().fsub(af, prod);
                            CgVal::F64(res)
                        }
                    }
                }
                "btest" => {
                    let a_v = eval_expr(b, ex, ptr_ty, &args[0], ints, reals, bools, func_meta);
                    let pos_v = eval_expr(b, ex, ptr_ty, &args[1], ints, reals, bools, func_meta);
                    let a = to_i64(b, a_v);
                    let pos = to_i64(b, pos_v);
                    let one = b.ins().iconst(ir::types::I64, 1);
                    let shifted = b.ins().ushr(a, pos);
                    let bit = b.ins().band(shifted, one);
                    let zero = b.ins().iconst(ir::types::I64, 0);
                    let cmp = b.ins().icmp(ir::condcodes::IntCC::NotEqual, bit, zero);
                    CgVal::Bool(cmp)
                }
                "iand" => {
                    let a_v = eval_expr(b, ex, ptr_ty, &args[0], ints, reals, bools, func_meta);
                    let b_v = eval_expr(b, ex, ptr_ty, &args[1], ints, reals, bools, func_meta);
                    let a = to_i64(b, a_v);
                    let b2 = to_i64(b, b_v);
                    let r = b.ins().band(a, b2);
                    CgVal::I64(r)
                }
                "ior" => {
                    let a_v = eval_expr(b, ex, ptr_ty, &args[0], ints, reals, bools, func_meta);
                    let b_v = eval_expr(b, ex, ptr_ty, &args[1], ints, reals, bools, func_meta);
                    let a = to_i64(b, a_v);
                    let b2 = to_i64(b, b_v);
                    let r = b.ins().bor(a, b2);
                    CgVal::I64(r)
                }
                "ieor" => {
                    let a_v = eval_expr(b, ex, ptr_ty, &args[0], ints, reals, bools, func_meta);
                    let b_v = eval_expr(b, ex, ptr_ty, &args[1], ints, reals, bools, func_meta);
                    let a = to_i64(b, a_v);
                    let b2 = to_i64(b, b_v);
                    let r = b.ins().bxor(a, b2);
                    CgVal::I64(r)
                }
                "not" => {
                    let a_v = eval_expr(b, ex, ptr_ty, &args[0], ints, reals, bools, func_meta);
                    let a = to_i64(b, a_v);
                    let r = b.ins().bnot(a);
                    CgVal::I64(r)
                }
                "dot_product" | "matmul" => {
                    // scalar fallback: multiply and return
                    let a_v = eval_expr(b, ex, ptr_ty, &args[0], ints, reals, bools, func_meta);
                    let b_v = eval_expr(b, ex, ptr_ty, &args[1], ints, reals, bools, func_meta);
                    let a = to_f64(b, a_v);
                    let b2 = to_f64(b, b_v);
                    let r = b.ins().fmul(a, b2);
                    CgVal::F64(r)
                }
                _ => {
                    if let Some(meta) = func_meta.get(name) {
                        let mut argv = Vec::new();
                        for (i, a) in args.iter().enumerate() {
                            let v = eval_expr(b, ex, ptr_ty, a, ints, reals, bools, func_meta);
                            match meta.param_kinds.get(i).copied().unwrap_or(ParamKind::I64) {
                                ParamKind::I64 => argv.push(to_i64(b, v)),
                                ParamKind::F64 => argv.push(to_f64(b, v)),
                            }
                        }
                        let inst = b.ins().call(meta.fref, &argv);
                        let res = b.inst_results(inst);
                        match meta.ret {
                            RetKind::Void => CgVal::I64(b.ins().iconst(ir::types::I64, 0)),
                            RetKind::I64 => {
                                if !res.is_empty() {
                                    CgVal::I64(res[0])
                                } else {
                                    CgVal::I64(b.ins().iconst(ir::types::I64, 0))
                                }
                            }
                            RetKind::F64 => {
                                if !res.is_empty() {
                                    CgVal::F64(res[0])
                                } else {
                                    CgVal::F64(b.ins().f64const(0.0))
                                }
                            }
                        }
                    } else {
                        CgVal::I64(b.ins().iconst(ir::types::I64, 0))
                    }
                }
            }
        }
    }
}

fn print_expr(
    b: &mut FunctionBuilder,
    ex: &Externs,
    ptr_ty: ir::Type,
    module: &mut ObjectModule,
    _dc: &mut u32,
    ints: &HashMap<String, ir::StackSlot>,
    reals: &HashMap<String, ir::StackSlot>,
    bools: &HashMap<String, ir::StackSlot>,
    chars: &HashMap<String, (ir::StackSlot, usize)>,
    real_kind: &HashMap<String, bool>,
    i128_last: &HashMap<String, String>,
    func_meta: &HashMap<String, FuncMeta>,
    e: &IExpr,
) -> Result<()> {
    use ir::condcodes::FloatCC;
    use ir::{types, MemFlags};
    match e {
        IExpr::Str(s) => {
            let mut bytes = s.as_bytes().to_vec();
            bytes.push(0);
            let id_num = GLOBAL_STR_ID.fetch_add(1, Ordering::Relaxed);
            let name = format!(".str{}.data", id_num);
            let id = module.declare_data(&name, Linkage::Local, false, false)?;
            let mut dd = DataDescription::new();
            dd.define(bytes.into_boxed_slice());
            module.define_data(id, &dd)?;
            let gv = module.declare_data_in_func(id, b.func);
            let p = b.ins().global_value(ptr_ty, gv);
            b.ins().call(ex.printf, &[ex.fmt_s, p]);
        }
        IExpr::IntLit(s) => {
            let v = i128::from_str_radix(s, 10)
                .ok()
                .and_then(|v| i64::try_from(v).ok())
                .unwrap_or(0);
            let x = b.ins().iconst(types::I64, v);
            let p = int_to_str(b, ex, ptr_ty, ex.sprintf, x);
            b.ins().call(ex.printf, &[ex.fmt_s, p]);
        }
        IExpr::RealLit(s) => {
            let v = s.parse::<f64>().unwrap_or(0.0);
            let fv = b.ins().f64const(v);
            let trunc = b.ins().fcvt_to_sint(types::I64, fv);
            let back = b.ins().fcvt_from_sint(types::F64, trunc);
            let is_int = b.ins().fcmp(FloatCC::Equal, fv, back);
            let buf_int = b.create_sized_stack_slot(ir::StackSlotData::new(
                ir::StackSlotKind::ExplicitSlot,
                64,
                0,
            ));
            let p_int = b.ins().stack_addr(ptr_ty, buf_int, 0);
            b.ins().call(ex.sprintf, &[p_int, ex.fmt_i64_dot0, trunc]);
            let buf_f = b.create_sized_stack_slot(ir::StackSlotData::new(
                ir::StackSlotKind::ExplicitSlot,
                64,
                0,
            ));
            let p_f = b.ins().stack_addr(ptr_ty, buf_f, 0);
            let nd = b.ins().iconst(types::I32, 6);
            b.ins().call(ex.gcvt, &[fv, nd, p_f]);
            let p_sel = b.ins().select(is_int, p_int, p_f);
            b.ins().call(ex.printf, &[ex.fmt_s, p_sel]);
        }
        IExpr::Logical(bv) => {
            let p = if *bv { ex.p_true } else { ex.p_false };
            b.ins().call(ex.printf, &[ex.fmt_s, p]);
        }
        IExpr::Ident(id) => {
            if let Some(txt) = i128_last.get(id) {
                let mut bytes = txt.as_bytes().to_vec();
                bytes.push(0);
                let id_num = GLOBAL_STR_ID.fetch_add(1, Ordering::Relaxed);
                let name = format!(".str{}.data", id_num);
                let idd = module.declare_data(&name, Linkage::Local, false, false)?;
                let mut dd = DataDescription::new();
                dd.define(bytes.into_boxed_slice());
                module.define_data(idd, &dd)?;
                let gv = module.declare_data_in_func(idd, b.func);
                let p = b.ins().global_value(ptr_ty, gv);
                b.ins().call(ex.printf, &[ex.fmt_s, p]);
            } else if let Some(ss) = ints.get(id) {
                let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                let v = b.ins().load(types::I64, MemFlags::new(), a, 0);
                let p = int_to_str(b, ex, ptr_ty, ex.sprintf, v);
                b.ins().call(ex.printf, &[ex.fmt_s, p]);
            } else if let Some(ss) = reals.get(id) {
                let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                let fv = b.ins().load(types::F64, MemFlags::new(), a, 0);
                let trunc = b.ins().fcvt_to_sint(types::I64, fv);
                let back = b.ins().fcvt_from_sint(types::F64, trunc);
                let is_int = b.ins().fcmp(FloatCC::Equal, fv, back);
                let buf_int = b.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    64,
                    0,
                ));
                let p_int = b.ins().stack_addr(ptr_ty, buf_int, 0);
                b.ins().call(ex.sprintf, &[p_int, ex.fmt_i64_dot0, trunc]);
                let buf_f = b.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    64,
                    0,
                ));
                let p_f = b.ins().stack_addr(ptr_ty, buf_f, 0);
                let nd = b.ins().iconst(
                    types::I32,
                    if *real_kind.get(id).unwrap_or(&false) {
                        15
                    } else {
                        6
                    },
                );
                b.ins().call(ex.gcvt, &[fv, nd, p_f]);
                let p_sel = b.ins().select(is_int, p_int, p_f);
                b.ins().call(ex.printf, &[ex.fmt_s, p_sel]);
            } else if let Some(ss) = bools.get(id) {
                let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                let v = b.ins().load(types::I8, MemFlags::new(), a, 0);
                let z = b.ins().iconst(types::I8, 0);
                let t = b.ins().icmp(ir::condcodes::IntCC::NotEqual, v, z);
                let sel = b.ins().select(t, ex.p_true, ex.p_false);
                b.ins().call(ex.printf, &[ex.fmt_s, sel]);
            } else if let Some((ss, _len)) = chars.get(id) {
                let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                b.ins().call(ex.printf, &[ex.fmt_s, a]);
            } else {
                let z = b.ins().iconst(types::I64, 0);
                let p = int_to_str(b, ex, ptr_ty, ex.sprintf, z);
                b.ins().call(ex.printf, &[ex.fmt_s, p]);
            }
        }
        IExpr::Bin(op, l, r) => {
            if let crate::ast::BinOp::Concat = op {
                if let (IExpr::Str(ls), IExpr::Str(rs)) = (&**l, &**r) {
                    let mut bytes = Vec::with_capacity(ls.len() + rs.len() + 1);
                    bytes.extend_from_slice(ls.as_bytes());
                    bytes.extend_from_slice(rs.as_bytes());
                    bytes.push(0);
                    let id_num = GLOBAL_STR_ID.fetch_add(1, Ordering::Relaxed);
                    let name = format!(".str{}.data", id_num);
                    let id = module.declare_data(&name, Linkage::Local, false, false)?;
                    let mut dd = DataDescription::new();
                    dd.define(bytes.into_boxed_slice());
                    module.define_data(id, &dd)?;
                    let gv = module.declare_data_in_func(id, b.func);
                    let p = b.ins().global_value(ptr_ty, gv);
                    b.ins().call(ex.printf, &[ex.fmt_s, p]);
                    return Ok(());
                }
            }
            let val = eval_expr(b, ex, ptr_ty, e, ints, reals, bools, func_meta);
            match val {
                CgVal::I64(x) => {
                    let p = int_to_str(b, ex, ptr_ty, ex.sprintf, x);
                    b.ins().call(ex.printf, &[ex.fmt_s, p]);
                }
                CgVal::F64(x) => {
                    let trunc = b.ins().fcvt_to_sint(types::I64, x);
                    let back = b.ins().fcvt_from_sint(types::F64, trunc);
                    let is_int = b.ins().fcmp(FloatCC::Equal, x, back);
                    let buf_int = b.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        64,
                        0,
                    ));
                    let p_int = b.ins().stack_addr(ptr_ty, buf_int, 0);
                    b.ins().call(ex.sprintf, &[p_int, ex.fmt_i64_dot0, trunc]);
                    let buf_f = b.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        64,
                        0,
                    ));
                    let p_f = b.ins().stack_addr(ptr_ty, buf_f, 0);
                    let nd = b.ins().iconst(types::I32, 6);
                    b.ins().call(ex.gcvt, &[x, nd, p_f]);
                    let p_sel = b.ins().select(is_int, p_int, p_f);
                    b.ins().call(ex.printf, &[ex.fmt_s, p_sel]);
                }
                CgVal::Bool(b1) => {
                    let sel = b.ins().select(b1, ex.p_true, ex.p_false);
                    b.ins().call(ex.printf, &[ex.fmt_s, sel]);
                }
            }
        }
        IExpr::Un(_, _) => {
            let val = eval_expr(b, ex, ptr_ty, e, ints, reals, bools, func_meta);
            match val {
                CgVal::I64(x) => {
                    let p = int_to_str(b, ex, ptr_ty, ex.sprintf, x);
                    b.ins().call(ex.printf, &[ex.fmt_s, p]);
                }
                CgVal::F64(x) => {
                    let trunc = b.ins().fcvt_to_sint(types::I64, x);
                    let back = b.ins().fcvt_from_sint(types::F64, trunc);
                    let is_int = b.ins().fcmp(FloatCC::Equal, x, back);
                    let buf_int = b.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        64,
                        0,
                    ));
                    let p_int = b.ins().stack_addr(ptr_ty, buf_int, 0);
                    b.ins().call(ex.sprintf, &[p_int, ex.fmt_i64_dot0, trunc]);
                    let buf_f = b.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        64,
                        0,
                    ));
                    let p_f = b.ins().stack_addr(ptr_ty, buf_f, 0);
                    let nd = b.ins().iconst(types::I32, 6);
                    b.ins().call(ex.gcvt, &[x, nd, p_f]);
                    let p_sel = b.ins().select(is_int, p_int, p_f);
                    b.ins().call(ex.printf, &[ex.fmt_s, p_sel]);
                }
                CgVal::Bool(b1) => {
                    let sel = b.ins().select(b1, ex.p_true, ex.p_false);
                    b.ins().call(ex.printf, &[ex.fmt_s, sel]);
                }
            }
        }
        IExpr::Call(name, args) => {
            // handle a few character intrinsics directly for printing
            let lname = name.to_ascii_lowercase();
            if (lname == "trim" || lname == "adjustl" || lname == "adjustr") && args.len() == 1 {
                use ir::condcodes::IntCC;
                // allow nested calls like trim(adjustl(str)) by resolving to the base ident;
                // if nested, fall back to printing the base pointer (temporary, correct but
                // doesn't perform all transforms). Full chained transform lowering can be
                // implemented later.
                let mut cur_expr = &args[0];
                let mut chain: Vec<String> = Vec::new();
                while let IExpr::Call(nm, a) = cur_expr {
                    chain.push(nm.to_ascii_lowercase());
                    if a.is_empty() {
                        break;
                    }
                    cur_expr = &a[0];
                }
                match cur_expr {
                    IExpr::Ident(id) => {
                        if let Some((ss, len)) = chars.get(id) {
                            // base pointer to the character field
                            let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                            if !chain.is_empty() {
                                // fallback: print the base pointer directly for nested calls
                                b.ins().call(ex.printf, &[ex.fmt_s, a]);
                                return Ok(());
                            }
                            // implement three behaviors by creating a temporary buffer
                            // of size (len + 1), filling it appropriately, null-terminating,
                            // and calling printf("%s", tmp).
                            let tmp_slot = b.create_sized_stack_slot(ir::StackSlotData::new(
                                ir::StackSlotKind::ExplicitSlot,
                                (*len + 1) as u32,
                                0,
                            ));
                            let tmp_ptr = b.ins().stack_addr(ptr_ty, tmp_slot, 0);

                            // Common constants
                            let zero_i64 = b.ins().iconst(ir::types::I64, 0);
                            let one_i64 = b.ins().iconst(ir::types::I64, 1);
                            let len_i64 = b.ins().iconst(ir::types::I64, *len as i64);
                            let space_i8 = b.ins().iconst(ir::types::I8, 32);
                            let term_i8 = b.ins().iconst(ir::types::I8, 0);
                            let zero_i8 = b.ins().iconst(ir::types::I8, 0);

                            // Helper: store null terminator at tmp[len]
                            b.ins()
                                .store(ir::MemFlags::new(), term_i8, tmp_ptr, *len as i32);

                            if lname == "trim" {
                                // Find last non-space index from the front by scanning up to len
                                let idx_slot = b.create_sized_stack_slot(ir::StackSlotData::new(
                                    ir::StackSlotKind::ExplicitSlot,
                                    8,
                                    0,
                                ));
                                let last_slot = b.create_sized_stack_slot(ir::StackSlotData::new(
                                    ir::StackSlotKind::ExplicitSlot,
                                    8,
                                    0,
                                ));
                                // init idx=0, last = -1
                                let idx_addr = b.ins().stack_addr(ptr_ty, idx_slot, 0);
                                b.ins().store(ir::MemFlags::new(), zero_i64, idx_addr, 0);
                                let neg1 = b.ins().iconst(ir::types::I64, -1);
                                let last_addr = b.ins().stack_addr(ptr_ty, last_slot, 0);
                                b.ins().store(ir::MemFlags::new(), neg1, last_addr, 0);

                                let loop_blk = b.create_block();
                                let body_blk = b.create_block();
                                let after_blk = b.create_block();
                                // start loop
                                b.ins().jump(loop_blk, &[]);
                                b.switch_to_block(loop_blk);
                                let cur =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), idx_addr, 0);
                                let done =
                                    b.ins().icmp(IntCC::SignedGreaterThanOrEqual, cur, len_i64);
                                b.ins().brif(done, after_blk, &[], body_blk, &[]);
                                b.switch_to_block(body_blk);
                                let p_cur = b.ins().iadd(a, cur);
                                let ch = b.ins().load(ir::types::I8, ir::MemFlags::new(), p_cur, 0);
                                let is_null = b.ins().icmp(IntCC::Equal, ch, term_i8);
                                // if null, exit; otherwise continue in cont_blk
                                let cont_blk = b.create_block();
                                b.ins().brif(is_null, after_blk, &[], cont_blk, &[]);
                                b.switch_to_block(cont_blk);
                                // if not null and not space -> update last index
                                let is_space = b.ins().icmp(IntCC::Equal, ch, space_i8);
                                // if not space, store cur to last
                                let neg_cmp = b.ins().icmp(IntCC::Equal, is_space, zero_i8);
                                // implement via explicit blocks
                                let then_blk = b.create_block();
                                let cont_blk = b.create_block();
                                b.ins().brif(neg_cmp, then_blk, &[], cont_blk, &[]);
                                b.switch_to_block(then_blk);
                                b.ins().store(ir::MemFlags::new(), cur, last_addr, 0);
                                b.ins().jump(cont_blk, &[]);
                                b.switch_to_block(cont_blk);
                                // idx += 1
                                let nxt = b.ins().iadd(cur, one_i64);
                                b.ins().store(ir::MemFlags::new(), nxt, idx_addr, 0);
                                b.ins().jump(loop_blk, &[]);
                                b.switch_to_block(after_blk);

                                // compute copy length = last+1 if last>=0 else 0
                                let lastv =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), last_addr, 0);
                                let is_neg_last =
                                    b.ins().icmp(IntCC::SignedLessThan, lastv, zero_i64);
                                // if negative, nothing to copy -> call printf with empty tmp
                                let copy_blk = b.create_block();
                                let done_copy_blk = b.create_block();
                                b.ins().brif(is_neg_last, done_copy_blk, &[], copy_blk, &[]);
                                // copy loop: for i in 0..=lastv copy a[i] -> tmp[i]
                                b.switch_to_block(copy_blk);
                                let i_slot = b.create_sized_stack_slot(ir::StackSlotData::new(
                                    ir::StackSlotKind::ExplicitSlot,
                                    8,
                                    0,
                                ));
                                let i_addr = b.ins().stack_addr(ptr_ty, i_slot, 0);
                                b.ins().store(ir::MemFlags::new(), zero_i64, i_addr, 0);
                                let loop2 = b.create_block();
                                let body2 = b.create_block();
                                let after2 = b.create_block();
                                b.ins().jump(loop2, &[]);
                                b.switch_to_block(loop2);
                                let iv =
                                    b.ins().load(ir::types::I64, ir::MemFlags::new(), i_addr, 0);
                                let cmp2 = b.ins().icmp(IntCC::SignedGreaterThan, iv, lastv);
                                b.ins().brif(cmp2, after2, &[], body2, &[]);
                                b.switch_to_block(body2);
                                let p_src = b.ins().iadd(a, iv);
                                let ch2 =
                                    b.ins().load(ir::types::I8, ir::MemFlags::new(), p_src, 0);
                                let p_dst = b.ins().iadd(tmp_ptr, iv);
                                b.ins().store(ir::MemFlags::new(), ch2, p_dst, 0);
                                let ivn = b.ins().iadd(iv, one_i64);
                                b.ins().store(ir::MemFlags::new(), ivn, i_addr, 0);
                                b.ins().jump(loop2, &[]);
                                b.switch_to_block(after2);
                                b.ins().jump(done_copy_blk, &[]);
                                b.switch_to_block(done_copy_blk);
                                // tmp already null-terminated at tmp[len], but ensure byte after last is 0
                                // call printf with tmp_ptr
                                b.ins().call(ex.printf, &[ex.fmt_s, tmp_ptr]);
                                return Ok(());
                            } else if lname == "adjustl" {
                                // find first non-space index (start) same as earlier
                                let idx_slot = b.create_sized_stack_slot(ir::StackSlotData::new(
                                    ir::StackSlotKind::ExplicitSlot,
                                    8,
                                    0,
                                ));
                                let idx_addr2 = b.ins().stack_addr(ptr_ty, idx_slot, 0);
                                b.ins().store(ir::MemFlags::new(), zero_i64, idx_addr2, 0);
                                let loop_blk = b.create_block();
                                let body_blk = b.create_block();
                                let after_blk = b.create_block();
                                b.ins().jump(loop_blk, &[]);
                                b.switch_to_block(loop_blk);
                                let cur =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), idx_addr2, 0);
                                let done =
                                    b.ins().icmp(IntCC::SignedGreaterThanOrEqual, cur, len_i64);
                                b.ins().brif(done, after_blk, &[], body_blk, &[]);
                                b.switch_to_block(body_blk);
                                let p_cur = b.ins().iadd(a, cur);
                                let ch = b.ins().load(ir::types::I8, ir::MemFlags::new(), p_cur, 0);
                                let is_null = b.ins().icmp(IntCC::Equal, ch, term_i8);
                                let cont_blk2 = b.create_block();
                                b.ins().brif(is_null, after_blk, &[], cont_blk2, &[]);
                                b.switch_to_block(cont_blk2);
                                let is_space = b.ins().icmp(IntCC::Equal, ch, space_i8);
                                // if space -> idx +=1 and loop, else finish
                                let next_blk = b.create_block();
                                b.ins().brif(is_space, next_blk, &[], after_blk, &[]);
                                b.switch_to_block(next_blk);
                                let nxt = b.ins().iadd(cur, one_i64);
                                b.ins().store(ir::MemFlags::new(), nxt, idx_addr2, 0);
                                b.ins().jump(loop_blk, &[]);
                                // after_blk contains the start index in idx_slot
                                b.switch_to_block(after_blk);
                                let start =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), idx_addr2, 0);
                                // copy from a+start into tmp_ptr starting at 0 until null or len-start
                                let cnt_slot = b.create_sized_stack_slot(ir::StackSlotData::new(
                                    ir::StackSlotKind::ExplicitSlot,
                                    8,
                                    0,
                                ));
                                let cnt_addr = b.ins().stack_addr(ptr_ty, cnt_slot, 0);
                                b.ins().store(ir::MemFlags::new(), zero_i64, cnt_addr, 0);
                                let loop_c = b.create_block();
                                let body_c = b.create_block();
                                let after_c = b.create_block();
                                b.ins().jump(loop_c, &[]);
                                b.switch_to_block(loop_c);
                                let cur_cnt =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), cnt_addr, 0);
                                let pos = b.ins().iadd(start, cur_cnt);
                                let cmp_end =
                                    b.ins().icmp(IntCC::SignedGreaterThanOrEqual, pos, len_i64);
                                b.ins().brif(cmp_end, after_c, &[], body_c, &[]);
                                b.switch_to_block(body_c);
                                let p_src = b.ins().iadd(a, pos);
                                let ch = b.ins().load(ir::types::I8, ir::MemFlags::new(), p_src, 0);
                                let is_null = b.ins().icmp(IntCC::Equal, ch, term_i8);
                                b.ins().brif(is_null, after_c, &[], after_c, &[]);
                                let p_dst = b.ins().iadd(tmp_ptr, cur_cnt);
                                b.ins().store(ir::MemFlags::new(), ch, p_dst, 0);
                                let nxt = b.ins().iadd(cur_cnt, one_i64);
                                b.ins().store(ir::MemFlags::new(), nxt, cnt_addr, 0);
                                b.ins().jump(loop_c, &[]);
                                b.switch_to_block(after_c);
                                // ensure null terminator at tmp[cur_cnt]
                                let curc =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), cnt_addr, 0);
                                let p_term = b.ins().iadd(tmp_ptr, curc);
                                b.ins().store(ir::MemFlags::new(), term_i8, p_term, 0);
                                b.ins().call(ex.printf, &[ex.fmt_s, tmp_ptr]);
                                return Ok(());
                            } else {
                                // adjustr: right-justify into field width
                                // find first non-space (start) and content length
                                let idx_slot = b.create_sized_stack_slot(ir::StackSlotData::new(
                                    ir::StackSlotKind::ExplicitSlot,
                                    8,
                                    0,
                                ));
                                let idx_addr3 = b.ins().stack_addr(ptr_ty, idx_slot, 0);
                                b.ins().store(ir::MemFlags::new(), zero_i64, idx_addr3, 0);
                                // find start
                                let loop_blk = b.create_block();
                                let body_blk = b.create_block();
                                let after_blk = b.create_block();
                                b.ins().jump(loop_blk, &[]);
                                b.switch_to_block(loop_blk);
                                let cur =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), idx_addr3, 0);
                                let done =
                                    b.ins().icmp(IntCC::SignedGreaterThanOrEqual, cur, len_i64);
                                b.ins().brif(done, after_blk, &[], body_blk, &[]);
                                b.switch_to_block(body_blk);
                                let p_cur = b.ins().iadd(a, cur);
                                let ch = b.ins().load(ir::types::I8, ir::MemFlags::new(), p_cur, 0);
                                let is_null = b.ins().icmp(IntCC::Equal, ch, term_i8);
                                let cont_blk3 = b.create_block();
                                b.ins().brif(is_null, after_blk, &[], cont_blk3, &[]);
                                b.switch_to_block(cont_blk3);
                                let is_space = b.ins().icmp(IntCC::Equal, ch, space_i8);
                                let next_blk = b.create_block();
                                b.ins().brif(is_space, next_blk, &[], after_blk, &[]);
                                b.switch_to_block(next_blk);
                                let nxt = b.ins().iadd(cur, one_i64);
                                b.ins().store(ir::MemFlags::new(), nxt, idx_addr3, 0);
                                b.ins().jump(loop_blk, &[]);
                                b.switch_to_block(after_blk);
                                let start =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), idx_addr3, 0);
                                // count content length from start
                                let cnt_slot = b.create_sized_stack_slot(ir::StackSlotData::new(
                                    ir::StackSlotKind::ExplicitSlot,
                                    8,
                                    0,
                                ));
                                let cnt_addr2 = b.ins().stack_addr(ptr_ty, cnt_slot, 0);
                                b.ins().store(ir::MemFlags::new(), zero_i64, cnt_addr2, 0);
                                let loop_c = b.create_block();
                                let body_c = b.create_block();
                                let after_c = b.create_block();
                                b.ins().jump(loop_c, &[]);
                                b.switch_to_block(loop_c);
                                let cur_cnt =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), cnt_addr2, 0);
                                let pos = b.ins().iadd(start, cur_cnt);
                                let cmp_end =
                                    b.ins().icmp(IntCC::SignedGreaterThanOrEqual, pos, len_i64);
                                b.ins().brif(cmp_end, after_c, &[], body_c, &[]);
                                b.switch_to_block(body_c);
                                let p_src = b.ins().iadd(a, pos);
                                let ch = b.ins().load(ir::types::I8, ir::MemFlags::new(), p_src, 0);
                                let is_null = b.ins().icmp(IntCC::Equal, ch, term_i8);
                                b.ins().brif(is_null, after_c, &[], after_c, &[]);
                                let p_dst = b.ins().iadd(tmp_ptr, cur_cnt);
                                b.ins().store(ir::MemFlags::new(), ch, p_dst, 0);
                                let nxt = b.ins().iadd(cur_cnt, one_i64);
                                b.ins().store(ir::MemFlags::new(), nxt, cnt_addr2, 0);
                                b.ins().jump(loop_c, &[]);
                                b.switch_to_block(after_c);
                                // cur_cnt now content length
                                let content_len =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), cnt_addr2, 0);
                                // compute shift = len - content_len
                                let shift = b.ins().isub(len_i64, content_len);
                                // create shifted buffer: first write 'shift' spaces, then copy content at tmp[shift..]
                                // fill leading spaces
                                let i_slot = b.create_sized_stack_slot(ir::StackSlotData::new(
                                    ir::StackSlotKind::ExplicitSlot,
                                    8,
                                    0,
                                ));
                                let i_addr2 = b.ins().stack_addr(ptr_ty, i_slot, 0);
                                b.ins().store(ir::MemFlags::new(), zero_i64, i_addr2, 0);
                                let loop_s = b.create_block();
                                let body_s = b.create_block();
                                let after_s = b.create_block();
                                b.ins().jump(loop_s, &[]);
                                b.switch_to_block(loop_s);
                                let iv =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), i_addr2, 0);
                                let cmp_s =
                                    b.ins().icmp(IntCC::SignedGreaterThanOrEqual, iv, shift);
                                b.ins().brif(cmp_s, after_s, &[], body_s, &[]);
                                b.switch_to_block(body_s);
                                let p_dst = b.ins().iadd(tmp_ptr, iv);
                                b.ins().store(ir::MemFlags::new(), space_i8, p_dst, 0);
                                let ivn = b.ins().iadd(iv, one_i64);
                                b.ins().store(ir::MemFlags::new(), ivn, i_addr2, 0);
                                b.ins().jump(loop_s, &[]);
                                b.switch_to_block(after_s);
                                // now copy content_len bytes from tmp[0..content_len] (we earlier copied content starting at tmp[0]) to tmp[shift..]
                                // We'll do the copy backwards to allow overlapping copy
                                let copy_idx = b.create_sized_stack_slot(ir::StackSlotData::new(
                                    ir::StackSlotKind::ExplicitSlot,
                                    8,
                                    0,
                                ));
                                // init copy_idx = content_len - 1
                                let one_i64_pos = b.ins().iconst(ir::types::I64, 1);
                                let cl_minus1 = b.ins().isub(content_len, one_i64_pos);
                                let copy_addr = b.ins().stack_addr(ptr_ty, copy_idx, 0);
                                b.ins().store(ir::MemFlags::new(), cl_minus1, copy_addr, 0);
                                let loop_cp = b.create_block();
                                let body_cp = b.create_block();
                                let after_cp = b.create_block();
                                b.ins().jump(loop_cp, &[]);
                                b.switch_to_block(loop_cp);
                                let ci =
                                    b.ins()
                                        .load(ir::types::I64, ir::MemFlags::new(), copy_addr, 0);
                                let cmp_cp = b.ins().icmp(IntCC::SignedLessThan, ci, zero_i64);
                                b.ins().brif(cmp_cp, after_cp, &[], body_cp, &[]);
                                b.switch_to_block(body_cp);
                                // src = tmp_ptr + ci
                                let srcp = b.ins().iadd(tmp_ptr, ci);
                                let ch = b.ins().load(ir::types::I8, ir::MemFlags::new(), srcp, 0);
                                // dest = tmp_ptr + shift + ci
                                let dest_index = b.ins().iadd(shift, ci);
                                let destp = b.ins().iadd(tmp_ptr, dest_index);
                                b.ins().store(ir::MemFlags::new(), ch, destp, 0);
                                // decrement ci
                                let dec = b.ins().isub(ci, one_i64_pos);
                                b.ins().store(ir::MemFlags::new(), dec, copy_addr, 0);
                                b.ins().jump(loop_cp, &[]);
                                b.switch_to_block(after_cp);
                                // ensure null at tmp[len]
                                let p_term = b.ins().iadd(tmp_ptr, len_i64);
                                b.ins().store(ir::MemFlags::new(), term_i8, p_term, 0);
                                b.ins().call(ex.printf, &[ex.fmt_s, tmp_ptr]);
                                return Ok(());
                            }
                        }
                    }
                    _ => {}
                }
            }
            if let Some(meta) = func_meta.get(name) {
                let mut argv = Vec::new();
                for (i, a) in args.iter().enumerate() {
                    let v = eval_expr(b, ex, ptr_ty, a, ints, reals, bools, func_meta);
                    match meta.param_kinds.get(i).copied().unwrap_or(ParamKind::I64) {
                        ParamKind::I64 => argv.push(to_i64(b, v)),
                        ParamKind::F64 => argv.push(to_f64(b, v)),
                    }
                }
                let inst = b.ins().call(meta.fref, &argv);
                let res = b.inst_results(inst);
                match meta.ret {
                    RetKind::Void => { /* nothing printed */ }
                    RetKind::I64 => {
                        if !res.is_empty() {
                            let p = int_to_str(b, ex, ptr_ty, ex.sprintf, res[0]);
                            b.ins().call(ex.printf, &[ex.fmt_s, p]);
                        }
                    }
                    RetKind::F64 => {
                        if !res.is_empty() {
                            let x = res[0];
                            let trunc = b.ins().fcvt_to_sint(ir::types::I64, x);
                            let back = b.ins().fcvt_from_sint(ir::types::F64, trunc);
                            let is_int = b.ins().fcmp(ir::condcodes::FloatCC::Equal, x, back);
                            let buf_int = b.create_sized_stack_slot(ir::StackSlotData::new(
                                ir::StackSlotKind::ExplicitSlot,
                                64,
                                0,
                            ));
                            let p_int = b.ins().stack_addr(ptr_ty, buf_int, 0);
                            b.ins().call(ex.sprintf, &[p_int, ex.fmt_i64_dot0, trunc]);
                            let buf_f = b.create_sized_stack_slot(ir::StackSlotData::new(
                                ir::StackSlotKind::ExplicitSlot,
                                64,
                                0,
                            ));
                            let p_f = b.ins().stack_addr(ptr_ty, buf_f, 0);
                            let nd = b.ins().iconst(ir::types::I32, 6);
                            b.ins().call(ex.gcvt, &[x, nd, p_f]);
                            let p_sel = b.ins().select(is_int, p_int, p_f);
                            b.ins().call(ex.printf, &[ex.fmt_s, p_sel]);
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

fn gen_stmts(
    b: &mut FunctionBuilder,
    ex: &Externs,
    ptr_ty: ir::Type,
    module: &mut ObjectModule,
    dc: &mut u32,
    stmts: &[IStmt],
    ints: &mut HashMap<String, ir::StackSlot>,
    reals: &mut HashMap<String, ir::StackSlot>,
    bools: &mut HashMap<String, ir::StackSlot>,
    chars: &mut HashMap<String, (ir::StackSlot, usize)>,
    real_kind: &mut HashMap<String, bool>,
    i128_vars: &mut HashSet<String>,
    i128_last: &mut HashMap<String, String>,
    func_meta: &HashMap<String, FuncMeta>,
    current_fn: Option<&str>,
) -> Result<bool> {
    use ir::{types, MemFlags};
    for st in stmts {
        match st {
            IStmt::Print(items) => {
                for e in items.iter() {
                    print_expr(
                        b, ex, ptr_ty, module, dc, ints, reals, bools, chars, real_kind, i128_last,
                        func_meta, e,
                    )?;
                }
                b.ins().call(ex.printf, &[ex.fmt_s, ex.p_nl]);
            }
            IStmt::DeclVar(kind, name) => {
                if ints.contains_key(name)
                    || reals.contains_key(name)
                    || bools.contains_key(name)
                    || chars.contains_key(name)
                    || i128_vars.contains(name)
                {
                    continue;
                }
                match kind {
                    crate::ast::TypeSpec::Integer(Some(16)) => {
                        i128_vars.insert(name.clone());
                        i128_last.insert(name.clone(), "0".into());
                    }
                    crate::ast::TypeSpec::Integer(_) => {
                        let ss = b.create_sized_stack_slot(ir::StackSlotData::new(
                            ir::StackSlotKind::ExplicitSlot,
                            8,
                            0,
                        ));
                        ints.insert(name.clone(), ss);
                    }
                    crate::ast::TypeSpec::Real => {
                        let ss = b.create_sized_stack_slot(ir::StackSlotData::new(
                            ir::StackSlotKind::ExplicitSlot,
                            8,
                            0,
                        ));
                        reals.insert(name.clone(), ss);
                        real_kind.insert(name.clone(), false);
                    }
                    crate::ast::TypeSpec::DoublePrecision => {
                        let ss = b.create_sized_stack_slot(ir::StackSlotData::new(
                            ir::StackSlotKind::ExplicitSlot,
                            8,
                            0,
                        ));
                        reals.insert(name.clone(), ss);
                        real_kind.insert(name.clone(), true);
                    }
                    crate::ast::TypeSpec::Character(len_opt) => {
                        let len = len_opt.unwrap_or(20);
                        let ss = b.create_sized_stack_slot(ir::StackSlotData::new(
                            ir::StackSlotKind::ExplicitSlot,
                            (len + 1) as u32,
                            0,
                        ));
                        chars.insert(name.clone(), (ss, len));
                    }
                    crate::ast::TypeSpec::Logical => {
                        let ss = b.create_sized_stack_slot(ir::StackSlotData::new(
                            ir::StackSlotKind::ExplicitSlot,
                            1,
                            0,
                        ));
                        bools.insert(name.clone(), ss);
                    }
                }
            }
            IStmt::AssignStr(name, s) => {
                if let Some((ss, len)) = chars.get(name) {
                    let bytes = s.as_bytes();
                    let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                    for i in 0..*len {
                        let byte = if i < bytes.len() { bytes[i] } else { 0 };
                        let v = b.ins().iconst(ir::types::I8, byte as i64);
                        b.ins().store(MemFlags::new(), v, a, i as i32);
                    }
                    let term = b.ins().iconst(ir::types::I8, 0);
                    b.ins().store(MemFlags::new(), term, a, *len as i32);
                }
            }
            IStmt::AssignIntLit(name, s) => {
                if i128_vars.contains(name) {
                    i128_last.insert(name.clone(), s.clone());
                }
                if let Some(ss) = ints.get(name) {
                    if let Ok(v128) = i128::from_str_radix(s, 10) {
                        let v = i64::try_from(v128).unwrap_or(0);
                        let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                        let iv = b.ins().iconst(types::I64, v);
                        b.ins().store(MemFlags::new(), iv, a, 0);
                    }
                }
            }
            IStmt::AssignRealLit(name, s) => {
                if let Some(ss) = reals.get(name) {
                    if let Ok(f) = s.parse::<f64>() {
                        let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                        let fv = b.ins().f64const(f);
                        b.ins().store(MemFlags::new(), fv, a, 0);
                    }
                }
            }
            IStmt::AssignBool(name, vb) => {
                if let Some(ss) = bools.get(name) {
                    let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                    let v = b.ins().iconst(ir::types::I8, if *vb { 1 } else { 0 });
                    b.ins().store(MemFlags::new(), v, a, 0);
                }
            }
            IStmt::AssignExpr(name, e) => {
                let v = eval_expr(b, ex, ptr_ty, e, ints, reals, bools, func_meta);
                if let Some(ss) = ints.get(name) {
                    let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                    let iv = to_i64(b, v);
                    b.ins().store(MemFlags::new(), iv, a, 0);
                } else if let Some(ss) = reals.get(name) {
                    let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                    let fv = to_f64(b, v);
                    b.ins().store(MemFlags::new(), fv, a, 0);
                } else if let Some(ss) = bools.get(name) {
                    let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                    let bv = to_bool(b, v);
                    let one = b.ins().iconst(ir::types::I8, 1);
                    let zero = b.ins().iconst(ir::types::I8, 0);
                    let as8 = b.ins().select(bv, one, zero);
                    b.ins().store(MemFlags::new(), as8, a, 0);
                }
            }
            IStmt::AssignIdent(dst, src) => {
                if let Some(dss) = ints.get(dst) {
                    if let Some(ss) = ints.get(src) {
                        let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                        let v = b.ins().load(types::I64, MemFlags::new(), a, 0);
                        let da = b.ins().stack_addr(ptr_ty, *dss, 0);
                        b.ins().store(MemFlags::new(), v, da, 0);
                    } else if let Some(ss) = reals.get(src) {
                        let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                        let fv = b.ins().load(ir::types::F64, MemFlags::new(), a, 0);
                        let iv = b.ins().fcvt_to_sint(types::I64, fv);
                        let da = b.ins().stack_addr(ptr_ty, *dss, 0);
                        b.ins().store(MemFlags::new(), iv, da, 0);
                    }
                } else if let Some(dss) = reals.get(dst) {
                    if let Some(ss) = reals.get(src) {
                        let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                        let fv = b.ins().load(ir::types::F64, MemFlags::new(), a, 0);
                        let da = b.ins().stack_addr(ptr_ty, *dss, 0);
                        b.ins().store(MemFlags::new(), fv, da, 0);
                    } else if let Some(ss) = ints.get(src) {
                        let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                        let iv = b.ins().load(types::I64, MemFlags::new(), a, 0);
                        let fv = b.ins().fcvt_from_sint(ir::types::F64, iv);
                        let da = b.ins().stack_addr(ptr_ty, *dss, 0);
                        b.ins().store(MemFlags::new(), fv, da, 0);
                    }
                } else if let Some(dss) = bools.get(dst) {
                    if let Some(ss) = bools.get(src) {
                        let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                        let bv = b.ins().load(ir::types::I8, MemFlags::new(), a, 0);
                        let da = b.ins().stack_addr(ptr_ty, *dss, 0);
                        b.ins().store(MemFlags::new(), bv, da, 0);
                    }
                }
            }
            IStmt::Return(expr_opt) => {
                if let Some(e) = expr_opt {
                    let v = eval_expr(b, ex, ptr_ty, e, ints, reals, bools, func_meta);
                    match v {
                        CgVal::I64(x) => {
                            b.ins().return_(&[x]);
                        }
                        CgVal::F64(x) => {
                            b.ins().return_(&[x]);
                        }
                        CgVal::Bool(x) => {
                            let one = b.ins().iconst(ir::types::I64, 1);
                            let zero = b.ins().iconst(ir::types::I64, 0);
                            let as_i = b.ins().select(x, one, zero);
                            b.ins().return_(&[as_i]);
                        }
                    }
                } else {
                    if let Some(fname) = current_fn {
                        use ir::MemFlags;
                        if let Some(ss) = ints.get(fname) {
                            let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                            let rv = b.ins().load(ir::types::I64, MemFlags::new(), a, 0);
                            b.ins().return_(&[rv]);
                        } else if let Some(ss) = reals.get(fname) {
                            let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                            let rv = b.ins().load(ir::types::F64, MemFlags::new(), a, 0);
                            b.ins().return_(&[rv]);
                        } else if let Some(ss) = bools.get(fname) {
                            let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                            let bv = b.ins().load(ir::types::I8, MemFlags::new(), a, 0);
                            let z = b.ins().iconst(ir::types::I8, 0);
                            let t = b.ins().icmp(ir::condcodes::IntCC::NotEqual, bv, z);
                            let one = b.ins().iconst(ir::types::I64, 1);
                            let zero = b.ins().iconst(ir::types::I64, 0);
                            let as_i = b.ins().select(t, one, zero);
                            b.ins().return_(&[as_i]);
                        } else {
                            b.ins().return_(&[]);
                        }
                    } else {
                        b.ins().return_(&[]);
                    }
                }
                return Ok(true);
            }
            IStmt::If {
                cond,
                then_body,
                else_body,
            } => {
                let c = eval_expr(b, ex, ptr_ty, cond, ints, reals, bools, func_meta);
                let cb = to_bool(b, c);
                let then_blk = b.create_block();
                let else_blk = b.create_block();
                let cont_blk = b.create_block();
                b.ins().brif(cb, then_blk, &[], else_blk, &[]);
                b.seal_block(then_blk);
                b.seal_block(else_blk);
                b.switch_to_block(then_blk);
                let tr = gen_stmts(
                    b, ex, ptr_ty, module, dc, then_body, ints, reals, bools, chars, real_kind,
                    i128_vars, i128_last, func_meta, current_fn,
                )?;
                if tr {
                    return Ok(true);
                }
                b.ins().jump(cont_blk, &[]);
                b.switch_to_block(else_blk);
                if let Some(eb) = else_body {
                    let er = gen_stmts(
                        b, ex, ptr_ty, module, dc, eb, ints, reals, bools, chars, real_kind,
                        i128_vars, i128_last, func_meta, current_fn,
                    )?;
                    if er {
                        return Ok(true);
                    }
                }
                b.ins().jump(cont_blk, &[]);
                b.seal_block(cont_blk);
                b.switch_to_block(cont_blk);
            }
            IStmt::Do {
                var,
                start,
                end,
                body,
            } => {
                if let Some(ss) = ints.get(var) {
                    let sv = eval_expr(b, ex, ptr_ty, start, ints, reals, bools, func_meta);
                    let ev = eval_expr(b, ex, ptr_ty, end, ints, reals, bools, func_meta);
                    let si = to_i64(b, sv);
                    let ei = to_i64(b, ev);
                    let a = b.ins().stack_addr(ptr_ty, *ss, 0);
                    b.ins().store(MemFlags::new(), si, a, 0);
                    let loop_blk = b.create_block();
                    let body_blk = b.create_block();
                    let after_blk = b.create_block();
                    b.ins().jump(loop_blk, &[]);
                    b.switch_to_block(loop_blk);
                    let cur = b.ins().load(types::I64, MemFlags::new(), a, 0);
                    let cmp = b
                        .ins()
                        .icmp(ir::condcodes::IntCC::SignedGreaterThan, cur, ei);
                    b.ins().brif(cmp, after_blk, &[], body_blk, &[]);
                    b.seal_block(after_blk);
                    b.switch_to_block(body_blk);
                    b.seal_block(body_blk);
                    let br = gen_stmts(
                        b, ex, ptr_ty, module, dc, body, ints, reals, bools, chars, real_kind,
                        i128_vars, i128_last, func_meta, current_fn,
                    )?;
                    if br {
                        return Ok(true);
                    }
                    let cur2 = b.ins().load(types::I64, MemFlags::new(), a, 0);
                    let one = b.ins().iconst(types::I64, 1);
                    let next = b.ins().iadd(cur2, one);
                    b.ins().store(MemFlags::new(), next, a, 0);
                    b.ins().jump(loop_blk, &[]);
                    b.seal_block(loop_blk);
                    b.switch_to_block(after_blk);
                }
            }
            IStmt::Call(name, args) => {
                if let Some(meta) = func_meta.get(name) {
                    let mut argv = Vec::new();
                    for (i, a) in args.iter().enumerate() {
                        let v = eval_expr(b, ex, ptr_ty, a, ints, reals, bools, func_meta);
                        match meta.param_kinds.get(i).copied().unwrap_or(ParamKind::I64) {
                            ParamKind::I64 => argv.push(to_i64(b, v)),
                            ParamKind::F64 => argv.push(to_f64(b, v)),
                        }
                    }
                    b.ins().call(meta.fref, &argv);
                }
            }
            IStmt::Read(args) => {
                // Lower READ statement: for each target ident, call scanf and store
                for a in args {
                    if let IExpr::Ident(id) = a {
                        if let Some(ss) = ints.get(id) {
                            let buf = b.create_sized_stack_slot(ir::StackSlotData::new(
                                ir::StackSlotKind::ExplicitSlot,
                                16,
                                0,
                            ));
                            let p = b.ins().stack_addr(ptr_ty, buf, 0);
                            b.ins().call(ex.scanf, &[ex.fmt_i64, p]);
                            let val = b.ins().load(ir::types::I64, MemFlags::new(), p, 0);
                            let dst = b.ins().stack_addr(ptr_ty, *ss, 0);
                            b.ins().store(MemFlags::new(), val, dst, 0);
                        } else if let Some(ss) = reals.get(id) {
                            let buf = b.create_sized_stack_slot(ir::StackSlotData::new(
                                ir::StackSlotKind::ExplicitSlot,
                                32,
                                0,
                            ));
                            let p = b.ins().stack_addr(ptr_ty, buf, 0);
                            b.ins().call(ex.scanf, &[ex.fmt_f64, p]);
                            let sf = b.ins().load(ir::types::F64, MemFlags::new(), p, 0);
                            let dst = b.ins().stack_addr(ptr_ty, *ss, 0);
                            b.ins().store(MemFlags::new(), sf, dst, 0);
                        } else if let Some(ss) = bools.get(id) {
                            let buf = b.create_sized_stack_slot(ir::StackSlotData::new(
                                ir::StackSlotKind::ExplicitSlot,
                                8,
                                0,
                            ));
                            let p = b.ins().stack_addr(ptr_ty, buf, 0);
                            b.ins().call(ex.scanf, &[ex.fmt_s, p]);
                            let v = b.ins().load(ir::types::I8, MemFlags::new(), p, 0);
                            let dst = b.ins().stack_addr(ptr_ty, *ss, 0);
                            b.ins().store(MemFlags::new(), v, dst, 0);
                        } else if let Some((ss, _len)) = chars.get(id) {
                            let dst = b.ins().stack_addr(ptr_ty, *ss, 0);
                            b.ins().call(ex.scanf, &[ex.fmt_s, dst]);
                        }
                    }
                }
            }
            IStmt::SelectCase {
                selector,
                cases,
                default,
            } => {
                // Simple, conservative Select Case lowering:
                // - supports integer selector (ints map) and character selector (chars map)
                // - sequentially compares case items and jumps to case blocks on match
                // - no jump-table optimization yet
                let cont_blk = b.create_block();

                // Prepare case blocks
                let mut case_blocks: Vec<ir::Block> = Vec::new();
                for _ in cases.iter() {
                    case_blocks.push(b.create_block());
                }
                let default_blk = if default.is_some() {
                    Some(b.create_block())
                } else {
                    None
                };

                // Attempt to handle selector forms
                // collect temporary 'next' blocks created during sequential compares
                let mut cmp_next_blocks: Vec<ir::Block> = Vec::new();
                match selector {
                    IExpr::Ident(sid) => {
                        // integer selector
                        if let Some(ss) = ints.get(sid) {
                            let sel_addr = b.ins().stack_addr(ptr_ty, *ss, 0);
                            for (ci, icase) in cases.iter().enumerate() {
                                // each case may have multiple items
                                for item in &icase.items {
                                    match item {
                                        IcCaseItem::Single(e) => {
                                            if let IExpr::IntLit(s) = e {
                                                if let Ok(v128) =
                                                    i128::from_str_radix(s.as_str(), 10)
                                                {
                                                    let v = i64::try_from(v128).unwrap_or(0);
                                                    let lit = b.ins().iconst(ir::types::I64, v);
                                                    let selv = b.ins().load(
                                                        ir::types::I64,
                                                        ir::MemFlags::new(),
                                                        sel_addr,
                                                        0,
                                                    );
                                                    let cmp = b.ins().icmp(
                                                        ir::condcodes::IntCC::Equal,
                                                        selv,
                                                        lit,
                                                    );
                                                    // branch to case block on equal, else continue
                                                    let next = b.create_block();
                                                    cmp_next_blocks.push(next);
                                                    b.ins().brif(
                                                        cmp,
                                                        case_blocks[ci],
                                                        &[],
                                                        next,
                                                        &[],
                                                    );
                                                    b.switch_to_block(next);
                                                }
                                            } else if let IExpr::Ident(id2) = e {
                                                if let Some(ss2) = ints.get(id2) {
                                                    let a2 = b.ins().stack_addr(ptr_ty, *ss2, 0);
                                                    let v2 = b.ins().load(
                                                        ir::types::I64,
                                                        ir::MemFlags::new(),
                                                        a2,
                                                        0,
                                                    );
                                                    let selv = b.ins().load(
                                                        ir::types::I64,
                                                        ir::MemFlags::new(),
                                                        sel_addr,
                                                        0,
                                                    );
                                                    let cmp = b.ins().icmp(
                                                        ir::condcodes::IntCC::Equal,
                                                        selv,
                                                        v2,
                                                    );
                                                    let next = b.create_block();
                                                    cmp_next_blocks.push(next);
                                                    b.ins().brif(
                                                        cmp,
                                                        case_blocks[ci],
                                                        &[],
                                                        next,
                                                        &[],
                                                    );
                                                    b.switch_to_block(next);
                                                }
                                            }
                                        }
                                        IcCaseItem::Range(l, r) => {
                                            if let (IExpr::IntLit(ls), IExpr::IntLit(rs)) = (l, r) {
                                                if let (Ok(lv128), Ok(rv128)) = (
                                                    i128::from_str_radix(ls.as_str(), 10),
                                                    i128::from_str_radix(rs.as_str(), 10),
                                                ) {
                                                    let lv = i64::try_from(lv128).unwrap_or(0);
                                                    let rv = i64::try_from(rv128).unwrap_or(0);
                                                    let lit_l = b.ins().iconst(ir::types::I64, lv);
                                                    let lit_r = b.ins().iconst(ir::types::I64, rv);
                                                    let selv = b.ins().load(
                                                        ir::types::I64,
                                                        ir::MemFlags::new(),
                                                        sel_addr,
                                                        0,
                                                    );
                                                    let ge = b.ins().icmp(ir::condcodes::IntCC::SignedGreaterThanOrEqual, selv, lit_l);
                                                    let le = b.ins().icmp(
                                                        ir::condcodes::IntCC::SignedLessThanOrEqual,
                                                        selv,
                                                        lit_r,
                                                    );
                                                    let both = b.ins().band(ge, le);
                                                    let next = b.create_block();
                                                    cmp_next_blocks.push(next);
                                                    b.ins().brif(
                                                        both,
                                                        case_blocks[ci],
                                                        &[],
                                                        next,
                                                        &[],
                                                    );
                                                    b.switch_to_block(next);
                                                }
                                            }
                                        }
                                        IcCaseItem::Multi(es) => {
                                            for e in es.iter() {
                                                if let IExpr::IntLit(s) = e {
                                                    if let Ok(v128) =
                                                        i128::from_str_radix(s.as_str(), 10)
                                                    {
                                                        let v = i64::try_from(v128).unwrap_or(0);
                                                        let lit = b.ins().iconst(ir::types::I64, v);
                                                        let selv = b.ins().load(
                                                            ir::types::I64,
                                                            ir::MemFlags::new(),
                                                            sel_addr,
                                                            0,
                                                        );
                                                        let cmp = b.ins().icmp(
                                                            ir::condcodes::IntCC::Equal,
                                                            selv,
                                                            lit,
                                                        );
                                                        let next = b.create_block();
                                                        cmp_next_blocks.push(next);
                                                        b.ins().brif(
                                                            cmp,
                                                            case_blocks[ci],
                                                            &[],
                                                            next,
                                                            &[],
                                                        );
                                                        b.switch_to_block(next);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } else if let Some((ss, _len)) = chars.get(sid) {
                            // character selector: compare against literal items
                            let _selptr = b.ins().stack_addr(ptr_ty, *ss, 0);
                            for (ci, icase) in cases.iter().enumerate() {
                                for item in &icase.items {
                                    match item {
                                        IcCaseItem::Single(e) => {
                                            if let IExpr::Str(lit) = e {
                                                // materialize literal into data and compare byte-by-byte
                                                // materialize literal and compare using strcmp
                                                let bytes = lit.as_bytes();
                                                let litp = make_data(module, b, ptr_ty, dc, bytes)?;
                                                let selp = b.ins().stack_addr(ptr_ty, *ss, 0);
                                                let call = b.ins().call(ex.strcmp, &[selp, litp]);
                                                let res = b.inst_results(call)[0];
                                                let zero = b.ins().iconst(ir::types::I32, 0);
                                                let eq = b.ins().icmp(
                                                    ir::condcodes::IntCC::Equal,
                                                    res,
                                                    zero,
                                                );
                                                let next = b.create_block();
                                                cmp_next_blocks.push(next);
                                                b.ins().brif(eq, case_blocks[ci], &[], next, &[]);
                                                b.switch_to_block(next);
                                                // helper blocks from earlier string-compare implementation
                                                // were removed when switching to strcmp; nothing to seal here
                                            }
                                        }
                                        IcCaseItem::Multi(es) => {
                                            for e in es.iter() {
                                                if let IExpr::Str(lit) = e {
                                                    // reuse single-item string compare logic per element
                                                    let bytes = lit.as_bytes();
                                                    let litp =
                                                        make_data(module, b, ptr_ty, dc, bytes)?;
                                                    let selp = b.ins().stack_addr(ptr_ty, *ss, 0);
                                                    let call =
                                                        b.ins().call(ex.strcmp, &[selp, litp]);
                                                    let res = b.inst_results(call)[0];
                                                    let zero = b.ins().iconst(ir::types::I32, 0);
                                                    let eq = b.ins().icmp(
                                                        ir::condcodes::IntCC::Equal,
                                                        res,
                                                        zero,
                                                    );
                                                    let next = b.create_block();
                                                    cmp_next_blocks.push(next);
                                                    b.ins().brif(
                                                        eq,
                                                        case_blocks[ci],
                                                        &[],
                                                        next,
                                                        &[],
                                                    );
                                                    b.switch_to_block(next);
                                                    // helper blocks from earlier string-compare implementation
                                                    // were removed when switching to strcmp; nothing to seal here
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                    _ => {
                        // Fallback: unsupported selector shape - fall through to default
                    }
                }

                // If no match by explicit compares, jump to default or continue
                if let Some(def_blk) = default_blk {
                    b.ins().jump(def_blk, &[]);
                }

                // Seal any temporary compare continuation blocks we created
                for nb in cmp_next_blocks.iter() {
                    b.seal_block(*nb);
                }

                // Emit case bodies
                for (ci, icase) in cases.iter().enumerate() {
                    b.switch_to_block(case_blocks[ci]);
                    let tr = gen_stmts(
                        b,
                        ex,
                        ptr_ty,
                        module,
                        dc,
                        &icase.body,
                        ints,
                        reals,
                        bools,
                        chars,
                        real_kind,
                        i128_vars,
                        i128_last,
                        func_meta,
                        current_fn,
                    )?;
                    if tr {
                        return Ok(true);
                    }
                    b.ins().jump(cont_blk, &[]);
                    // seal this case block now that its predecessors are emitted
                    b.seal_block(case_blocks[ci]);
                }

                if let Some(def_blk) = default_blk {
                    b.switch_to_block(def_blk);
                    if let Some(dbody) = default {
                        let tr = gen_stmts(
                            b, ex, ptr_ty, module, dc, dbody, ints, reals, bools, chars, real_kind,
                            i128_vars, i128_last, func_meta, current_fn,
                        )?;
                        if tr {
                            return Ok(true);
                        }
                    }
                    b.ins().jump(cont_blk, &[]);
                    b.seal_block(def_blk);
                }

                b.seal_block(cont_blk);
                b.switch_to_block(cont_blk);
            }
        } // end match
    } // end for
    Ok(false)
}

struct FuncMeta {
    fref: ir::FuncRef,
    ret: RetKind,
    param_kinds: Vec<ParamKind>,
}

impl Backend for CraneliftBackend {
    fn compile(&self, module: &Module) -> Result<Vec<u8>> {
        use crate::ast::TypeSpec;
        use cranelift_module::FuncId;
        use ir::{types, AbiParam};
        let triple = self.triple.clone().unwrap_or_else(Triple::host);
        let isa = isa::lookup(triple.clone())?.finish(self.flags.clone())?;
        let builder = ObjectBuilder::new(
            isa.clone(),
            "f90c",
            cranelift_module::default_libcall_names(),
        )?;
        let mut obj = ObjectModule::new(builder);
        let mut func_ids: HashMap<String, FuncId> = HashMap::new();
        let mut eff_ret: HashMap<String, crate::ast::TypeSpec> = HashMap::new();
        let mut param_kinds_map: HashMap<String, Vec<ParamKind>> = HashMap::new();
        let mut param_double_flags: HashMap<String, Vec<bool>> = HashMap::new();
        for f in module.funcs.iter() {
            let mut rt = f.return_type.clone();
            if rt.is_none() {
                for st in &f.body {
                    if let IStmt::DeclVar(ts, n) = st {
                        if n == &f.name {
                            rt = Some(ts.clone());
                            break;
                        }
                    }
                }
            }
            let rt_final = rt.unwrap_or(crate::ast::TypeSpec::Integer(None));
            eff_ret.insert(f.name.clone(), rt_final.clone());
            let mut pk: Vec<ParamKind> = Vec::new();
            let mut pdf: Vec<bool> = Vec::new();
            for pname in &f.params {
                let mut found = None;
                for st in &f.body {
                    if let IStmt::DeclVar(ts, n) = st {
                        if n == pname {
                            found = Some(ts.clone());
                            break;
                        }
                    }
                }
                let ts = found.unwrap_or(crate::ast::TypeSpec::Integer(None));
                match ts {
                    crate::ast::TypeSpec::Real => {
                        pk.push(ParamKind::F64);
                        pdf.push(false);
                    }
                    crate::ast::TypeSpec::DoublePrecision => {
                        pk.push(ParamKind::F64);
                        pdf.push(true);
                    }
                    _ => {
                        pk.push(ParamKind::I64);
                        pdf.push(false);
                    }
                }
            }
            param_kinds_map.insert(f.name.clone(), pk);
            param_double_flags.insert(f.name.clone(), pdf);
        }
        let defined_names: HashSet<String> = module
            .funcs
            .iter()
            .skip(1)
            .map(|f| f.name.clone())
            .collect();
        fn collect_calls_expr(e: &IExpr, out: &mut Vec<(String, usize)>) {
            match e {
                // IExpr::Read handled at statement level; ignore here
                IExpr::Call(n, args) => {
                    out.push((n.clone(), args.len()));
                    for a in args {
                        collect_calls_expr(a, out);
                    }
                }
                IExpr::Bin(_, l, r) => {
                    collect_calls_expr(l, out);
                    collect_calls_expr(r, out);
                }
                IExpr::Un(_, x) => collect_calls_expr(x, out),
                _ => {}
            }
        }
        fn collect_calls_body(sts: &[IStmt], out: &mut Vec<(String, usize)>) {
            for st in sts {
                match st {
                    IStmt::Call(n, args) => out.push((n.clone(), args.len())),
                    IStmt::AssignExpr(_, e) => collect_calls_expr(e, out),
                    IStmt::Return(Some(e)) => collect_calls_expr(e, out),
                    IStmt::Print(items) => {
                        for e in items {
                            collect_calls_expr(e, out)
                        }
                    }
                    IStmt::Read(args) => {
                        for e in args {
                            collect_calls_expr(e, out);
                        }
                    }
                    IStmt::If {
                        cond,
                        then_body,
                        else_body,
                    } => {
                        collect_calls_expr(cond, out);
                        collect_calls_body(then_body, out);
                        if let Some(eb) = else_body {
                            collect_calls_body(eb, out);
                        }
                    }
                    IStmt::Do {
                        start, end, body, ..
                    } => {
                        collect_calls_expr(start, out);
                        collect_calls_expr(end, out);
                        collect_calls_body(body, out);
                    }
                    _ => {}
                }
            }
        }
        let mut call_list = Vec::new();
        for f in &module.funcs {
            collect_calls_body(&f.body, &mut call_list);
        }
        let mut import_arity: HashMap<String, usize> = HashMap::new();
        for (n, a) in call_list {
            if !defined_names.contains(&n) {
                let e = import_arity.entry(n).or_insert(0);
                *e = max(*e, a);
            }
        }
        // Heuristic: prepopulate signatures for well-known Fortran intrinsics so
        // imports use the correct parameter/return kinds instead of defaulting
        // to integer. This is a pragmatic, best-effort mapping (case-insensitive).
        // The user requested to "add all of them"; we map numeric intrinsics to
        for name in import_arity.keys().cloned().collect::<Vec<_>>() {
            let lname = name.to_ascii_lowercase();
            match lname.as_str() {
                // floating-point / numeric functions -> return real, params real
                "abs" | "aimag" | "aint" | "anint" | "ceiling" | "cmplx" | "conjg" | "dble"
                | "dim" | "dprod" | "floor" | "db" | "sqrt" | "exp" | "log" | "log10" | "sin"
                | "cos" | "tan" | "asin" | "acos" | "atan" | "atan2" | "sinh" | "cosh" | "tanh"
                | "scale" | "set_exponent" | "nearest" | "fraction" | "exponent" | "spacing"
                | "rrspacing" | "precision" | "radix" | "range" | "tiny" | "epsilon" | "huge"
                | "max" | "min" | "sum" | "dot_product" | "matmul" | "maxval" | "minval"
                | "product" => {
                    eff_ret.insert(name.clone(), crate::ast::TypeSpec::Real);
                    let ar = *import_arity.get(&name).unwrap_or(&1usize);
                    param_kinds_map.insert(name.clone(), vec![ParamKind::F64; ar]);
                    param_double_flags.insert(name.clone(), vec![false; ar]);
                }
                // integer-returning numeric functions
                "int" | "nint" | "ichar" | "iachar" | "len" | "len_trim" | "count" | "size"
                | "lbound" | "ubound" | "maxloc" | "minloc" | "index" | "scan" | "verify" => {
                    eff_ret.insert(name.clone(), crate::ast::TypeSpec::Integer(None));
                    let ar = *import_arity.get(&name).unwrap_or(&1usize);
                    // parameters are typically mixed; assume F64 for numeric, else I64
                    param_kinds_map.insert(name.clone(), vec![ParamKind::F64; ar]);
                    param_double_flags.insert(name.clone(), vec![false; ar]);
                }
                // logical results
                "all" | "any" | "allocated" | "lge" | "lgt" | "lle" | "llt" => {
                    eff_ret.insert(name.clone(), crate::ast::TypeSpec::Logical);
                    let ar = *import_arity.get(&name).unwrap_or(&1usize);
                    param_kinds_map.insert(name.clone(), vec![ParamKind::F64; ar]);
                    param_double_flags.insert(name.clone(), vec![false; ar]);
                }
                // bitwise / integer ops
                "btest" | "iand" | "ibclr" | "ibits" | "ibset" | "ieor" | "ior" | "ishft"
                | "ishftc" | "not" | "mod" | "modulo" => {
                    eff_ret.insert(name.clone(), crate::ast::TypeSpec::Integer(None));
                    let ar = *import_arity.get(&name).unwrap_or(&1usize);
                    param_kinds_map.insert(name.clone(), vec![ParamKind::I64; ar]);
                    param_double_flags.insert(name.clone(), vec![false; ar]);
                }
                // character/string helpers - return integer or character; default to integer
                "achar" | "char" | "repeat" | "trim" | "adjustl" | "adjustr" | "reshape"
                | "transpose" | "spread" | "pack" | "unpack" | "merge" => {
                    eff_ret.insert(name.clone(), crate::ast::TypeSpec::Integer(None));
                    let ar = *import_arity.get(&name).unwrap_or(&1usize);
                    param_kinds_map.insert(name.clone(), vec![ParamKind::I64; ar]);
                    param_double_flags.insert(name.clone(), vec![false; ar]);
                }
                _ => {
                    // leave defaults; unspecified intrinsics will be imported as i64 params/return
                }
            }
        }
        let entry_func_name = module
            .funcs
            .first()
            .map(|f| f.name.clone())
            .unwrap_or_else(|| "main".into());
        let is_real_main = entry_func_name == "main";
        for (idx, f) in module.funcs.iter().enumerate() {
            if idx == 0 && is_real_main {
                continue;
            }
            let sig_rt = eff_ret.get(&f.name).unwrap();
            let pk = param_kinds_map.get(&f.name).unwrap();
            let mut ctx = obj.make_context();
            for k in pk {
                match k {
                    ParamKind::I64 => ctx
                        .func
                        .signature
                        .params
                        .push(ir::AbiParam::new(ir::types::I64)),
                    ParamKind::F64 => ctx
                        .func
                        .signature
                        .params
                        .push(ir::AbiParam::new(ir::types::F64)),
                }
            }
            match sig_rt {
                crate::ast::TypeSpec::Integer(_) | crate::ast::TypeSpec::Logical => ctx
                    .func
                    .signature
                    .returns
                    .push(ir::AbiParam::new(ir::types::I64)),
                crate::ast::TypeSpec::Real | crate::ast::TypeSpec::DoublePrecision => ctx
                    .func
                    .signature
                    .returns
                    .push(ir::AbiParam::new(ir::types::F64)),
                crate::ast::TypeSpec::Character(_) => {}
            }
            let fid = obj.declare_function(&f.name, Linkage::Export, &ctx.func.signature)?;
            obj.clear_context(&mut ctx);
            func_ids.insert(f.name.clone(), fid);
        }
        for (name, arity) in &import_arity {
            if eff_ret.contains_key(name) {
                continue;
            }
            if func_ids.contains_key(name) {
                continue;
            }
            let mut ctx = obj.make_context();
            for _ in 0..*arity {
                ctx.func
                    .signature
                    .params
                    .push(ir::AbiParam::new(ir::types::I64));
            }
            ctx.func
                .signature
                .returns
                .push(ir::AbiParam::new(ir::types::I64));
            let fid = obj.declare_function(name, Linkage::Import, &ctx.func.signature)?;
            obj.clear_context(&mut ctx);
            func_ids.insert(name.clone(), fid);
            eff_ret.insert(name.clone(), crate::ast::TypeSpec::Integer(None));
            param_kinds_map.insert(name.clone(), vec![ParamKind::I64; *arity]);
            param_double_flags.insert(name.clone(), vec![false; *arity]);
            if !func_ids.contains_key(name) {
                let mut ctx = obj.make_context();
                for _ in 0..*arity {
                    ctx.func
                        .signature
                        .params
                        .push(ir::AbiParam::new(ir::types::I64));
                }
                ctx.func
                    .signature
                    .returns
                    .push(ir::AbiParam::new(ir::types::I64));
                let fid = obj.declare_function(name, Linkage::Import, &ctx.func.signature)?;
                obj.clear_context(&mut ctx);
                func_ids.insert(name.clone(), fid);
                eff_ret.insert(name.clone(), crate::ast::TypeSpec::Integer(None));
                param_kinds_map.insert(name.clone(), vec![ParamKind::I64; *arity]);
                param_double_flags.insert(name.clone(), vec![false; *arity]);
            }
        }
        let mut ret_kinds: HashMap<String, RetKind> = HashMap::new();
        for (name, rt) in &eff_ret {
            ret_kinds.insert(
                name.clone(),
                match rt {
                    crate::ast::TypeSpec::Integer(_) | crate::ast::TypeSpec::Logical => {
                        RetKind::I64
                    }
                    crate::ast::TypeSpec::Real | crate::ast::TypeSpec::DoublePrecision => {
                        RetKind::F64
                    }
                    crate::ast::TypeSpec::Character(_) => RetKind::Void,
                },
            );
        }
        for (idx, f) in module.funcs.iter().enumerate() {
            if idx == 0 && is_real_main {
                continue;
            }
            let rt = eff_ret.get(&f.name).unwrap().clone();
            let pk = param_kinds_map.get(&f.name).unwrap().clone();
            let pdf = param_double_flags.get(&f.name).unwrap().clone();
            let mut ctx_f = obj.make_context();
            for k in &pk {
                match k {
                    ParamKind::I64 => ctx_f.func.signature.params.push(AbiParam::new(types::I64)),
                    ParamKind::F64 => ctx_f.func.signature.params.push(AbiParam::new(types::F64)),
                }
            }
            match rt {
                TypeSpec::Integer(_) | TypeSpec::Logical => {
                    ctx_f.func.signature.returns.push(AbiParam::new(types::I64))
                }
                TypeSpec::Real | TypeSpec::DoublePrecision => {
                    ctx_f.func.signature.returns.push(AbiParam::new(types::F64))
                }
                TypeSpec::Character(_) => {}
            }
            let mut fbctx = FunctionBuilderContext::new();
            {
                let mut b = FunctionBuilder::new(&mut ctx_f.func, &mut fbctx);
                let entry = b.create_block();
                b.append_block_params_for_function_params(entry);
                b.switch_to_block(entry);
                b.seal_block(entry);
                let ptr_ty = isa.pointer_type();
                let ex = declare_externs(&mut obj, &mut b, ptr_ty)?;
                let mut ints = HashMap::new();
                let mut reals = HashMap::new();
                let mut bools = HashMap::new();
                let mut chars = HashMap::new();
                let mut real_kind = HashMap::new();
                let mut i128_vars = HashSet::new();
                let mut i128_last = HashMap::new();
                let mut dc = 0u32;
                let mut func_meta: HashMap<String, FuncMeta> = HashMap::new();
                for (name, fid) in &func_ids {
                    let fr = obj.declare_func_in_func(*fid, b.func);
                    let rk = *ret_kinds.get(name).unwrap_or(&RetKind::Void);
                    let pkv = param_kinds_map.get(name).cloned().unwrap_or_default();
                    func_meta.insert(
                        name.clone(),
                        FuncMeta {
                            fref: fr,
                            ret: rk,
                            param_kinds: pkv,
                        },
                    );
                }
                for (idx, pname) in f.params.iter().enumerate() {
                    let slot = b.create_sized_stack_slot(ir::StackSlotData::new(
                        ir::StackSlotKind::ExplicitSlot,
                        8,
                        0,
                    ));
                    let pv = b.block_params(entry)[idx];
                    let addr = b.ins().stack_addr(ptr_ty, slot, 0);
                    match pk[idx] {
                        ParamKind::I64 => {
                            b.ins().store(ir::MemFlags::new(), pv, addr, 0);
                            ints.insert(pname.clone(), slot);
                        }
                        ParamKind::F64 => {
                            b.ins().store(ir::MemFlags::new(), pv, addr, 0);
                            reals.insert(pname.clone(), slot);
                            real_kind.insert(pname.clone(), pdf[idx]);
                        }
                    }
                }
                enum RetSlot {
                    I(ir::StackSlot),
                    F64(ir::StackSlot),
                }
                let ret_slot: Option<RetSlot> = match rt {
                    TypeSpec::Integer(_) | TypeSpec::Logical => {
                        let ss = b.create_sized_stack_slot(ir::StackSlotData::new(
                            ir::StackSlotKind::ExplicitSlot,
                            8,
                            0,
                        )); // zero init
                        let addr = b.ins().stack_addr(ptr_ty, ss, 0);
                        let z = b.ins().iconst(types::I64, 0);
                        b.ins().store(ir::MemFlags::new(), z, addr, 0);
                        ints.insert(f.name.clone(), ss);
                        Some(RetSlot::I(ss))
                    }
                    TypeSpec::Real => {
                        let ss = b.create_sized_stack_slot(ir::StackSlotData::new(
                            ir::StackSlotKind::ExplicitSlot,
                            8,
                            0,
                        ));
                        let addr = b.ins().stack_addr(ptr_ty, ss, 0);
                        let z = b.ins().f64const(0.0);
                        b.ins().store(ir::MemFlags::new(), z, addr, 0);
                        reals.insert(f.name.clone(), ss);
                        real_kind.insert(f.name.clone(), false);
                        Some(RetSlot::F64(ss))
                    }
                    TypeSpec::DoublePrecision => {
                        let ss = b.create_sized_stack_slot(ir::StackSlotData::new(
                            ir::StackSlotKind::ExplicitSlot,
                            8,
                            0,
                        ));
                        let addr = b.ins().stack_addr(ptr_ty, ss, 0);
                        let z = b.ins().f64const(0.0);
                        b.ins().store(ir::MemFlags::new(), z, addr, 0);
                        reals.insert(f.name.clone(), ss);
                        real_kind.insert(f.name.clone(), true);
                        Some(RetSlot::F64(ss))
                    }
                    TypeSpec::Character(_) => None,
                };
                let did_ret = gen_stmts(
                    &mut b,
                    &ex,
                    ptr_ty,
                    &mut obj,
                    &mut dc,
                    &f.body,
                    &mut ints,
                    &mut reals,
                    &mut bools,
                    &mut chars,
                    &mut real_kind,
                    &mut i128_vars,
                    &mut i128_last,
                    &func_meta,
                    Some(&f.name),
                )?;
                if !did_ret {
                    if let Some(rs) = ret_slot {
                        match rs {
                            RetSlot::I(ss) => {
                                let addr = b.ins().stack_addr(ptr_ty, ss, 0);
                                let rv = b.ins().load(types::I64, ir::MemFlags::new(), addr, 0);
                                b.ins().return_(&[rv]);
                            }
                            RetSlot::F64(ss) => {
                                let addr = b.ins().stack_addr(ptr_ty, ss, 0);
                                let rv = b.ins().load(types::F64, ir::MemFlags::new(), addr, 0);
                                b.ins().return_(&[rv]);
                            }
                        }
                    } else {
                        b.ins().return_(&[]);
                    }
                }
                b.finalize();
            }
            let fid = *func_ids.get(&f.name).unwrap();
            obj.define_function(fid, &mut ctx_f)?;
            obj.clear_context(&mut ctx_f);
        }
        let entry_func_name = module
            .funcs
            .first()
            .map(|f| f.name.clone())
            .unwrap_or_else(|| "main".into());
        let is_real_main = entry_func_name == "main";
        if is_real_main {
            let mut ctx_main = obj.make_context();
            let ptr_ty = isa.pointer_type();
            ctx_main
                .func
                .signature
                .params
                .push(AbiParam::new(types::I32));
            ctx_main.func.signature.params.push(AbiParam::new(ptr_ty));
            ctx_main
                .func
                .signature
                .returns
                .push(AbiParam::new(types::I32));
            let mut fbctx = FunctionBuilderContext::new();
            {
                let mut b = FunctionBuilder::new(&mut ctx_main.func, &mut fbctx);
                let entry = b.create_block();
                b.append_block_params_for_function_params(entry);
                b.switch_to_block(entry);
                b.seal_block(entry);
                let ex = declare_externs(&mut obj, &mut b, ptr_ty)?;
                let mut ints = HashMap::new();
                let mut reals = HashMap::new();
                let mut bools = HashMap::new();
                let mut chars = HashMap::new();
                let mut real_kind = HashMap::new();
                let mut i128_vars = HashSet::new();
                let mut i128_last = HashMap::new();
                let mut dc = 0u32;
                let mut func_meta: HashMap<String, FuncMeta> = HashMap::new();
                for (name, fid) in &func_ids {
                    let fr = obj.declare_func_in_func(*fid, b.func);
                    let rk = *ret_kinds.get(name).unwrap_or(&RetKind::Void);
                    let pkv = param_kinds_map.get(name).cloned().unwrap_or_default();
                    func_meta.insert(
                        name.clone(),
                        FuncMeta {
                            fref: fr,
                            ret: rk,
                            param_kinds: pkv,
                        },
                    );
                }
                let _ = gen_stmts(
                    &mut b,
                    &ex,
                    ptr_ty,
                    &mut obj,
                    &mut dc,
                    &module.funcs[0].body,
                    &mut ints,
                    &mut reals,
                    &mut bools,
                    &mut chars,
                    &mut real_kind,
                    &mut i128_vars,
                    &mut i128_last,
                    &func_meta,
                    None,
                )?;
                let z = b.ins().iconst(types::I32, 0);
                b.ins().return_(&[z]);
                b.finalize();
            }
            let main_id =
                obj.declare_function("main", Linkage::Export, &ctx_main.func.signature)?;
            obj.define_function(main_id, &mut ctx_main)?;
            obj.clear_context(&mut ctx_main);
        }
        let objf = obj.finish();
        Ok(objf.emit()?)
    }
}
