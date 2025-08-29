#[derive(Debug, Clone)]
pub struct Program {
    #[allow(dead_code)]
    pub name: String,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum TypeSpec {
    Integer(Option<u8>),      // kind: 1,2,4,8,16
    Real,                     // single precision
    DoublePrecision,          // more precise float
    Character(Option<usize>), // len
    Logical,                  // boolean
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Print {
        items: Vec<Expr>,
    },
    VarDecl {
        kind: TypeSpec,
        names: Vec<String>,
    },
    // explicit array declaration with dimensions, e.g. `integer :: a(5)`
    ArrayDecl {
        kind: TypeSpec,
        name: String,
        dims: Vec<Expr>,
    },
    Assign {
        name: String,
        value: Expr,
    },
    // assignment to array element: a(1) = expr
    AssignIndex {
        name: String,
        indices: Vec<Expr>,
        value: Expr,
    },
    If {
        cond: Expr,
        then_body: Vec<Stmt>,
        else_body: Option<Vec<Stmt>>,
    },
    Do {
        var: String,
        start: Expr,
        end: Expr,
        body: Vec<Stmt>,
    },
    DoWhile {
        cond: Expr,
        body: Vec<Stmt>,
    },
    Function {
        name: String,
        params: Vec<String>,
        return_type: Option<TypeSpec>,
        body: Vec<Stmt>,
    },
    Subroutine {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
    },
    Return(Option<Expr>),
    CallSub {
        name: String,
        args: Vec<Expr>,
    },
    Read {
        args: Vec<Expr>,
    },
    Use {
        module: String,
    },
    Module {
        name: String,
        body: Vec<Stmt>,
    },
    ImplicitNone, // new variant
    SelectCase {
        expr: Expr,
        cases: Vec<CaseBlock>,
        default: Option<Vec<Stmt>>,
    },
    Block {
        body: Vec<Stmt>,
    },
    Exit,
    Cycle,
}

#[derive(Debug, Clone)]
pub struct CaseBlock {
    pub items: Vec<CaseItem>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum CaseItem {
    Range(Expr, Expr),
    Single(Expr),
}

#[derive(Debug, Clone)]
pub enum CaseOrDefault {
    Case(CaseBlock),
    Default(Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Eqv,
    Neqv,
    Concat,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Str(String),
    IntLit(String),
    RealLit(String),
    Logical(bool),
    Ident(String),
    // indexing: a(1) or mat(i,j)
    #[allow(dead_code)]
    Index(String, Vec<Expr>),
    Bin(BinOp, Box<Expr>, Box<Expr>),
    Un(UnOp, Box<Expr>),
    Call(String, Vec<Expr>),
}

impl Expr {
    pub fn add(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Add, Box::new(l), Box::new(r))
    }
    pub fn sub(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Sub, Box::new(l), Box::new(r))
    }
    pub fn mul(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Mul, Box::new(l), Box::new(r))
    }
    pub fn div(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Div, Box::new(l), Box::new(r))
    }
    pub fn pow(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Pow, Box::new(l), Box::new(r))
    }

    pub fn eq(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Eq, Box::new(l), Box::new(r))
    }
    pub fn ne(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Ne, Box::new(l), Box::new(r))
    }
    pub fn lt(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Lt, Box::new(l), Box::new(r))
    }
    pub fn gt(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Gt, Box::new(l), Box::new(r))
    }
    pub fn le(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Le, Box::new(l), Box::new(r))
    }
    pub fn ge(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Ge, Box::new(l), Box::new(r))
    }

    pub fn and(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::And, Box::new(l), Box::new(r))
    }
    pub fn or(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Or, Box::new(l), Box::new(r))
    }
    pub fn eqv(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Eqv, Box::new(l), Box::new(r))
    }
    pub fn neqv(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Neqv, Box::new(l), Box::new(r))
    }

    pub fn concat(l: Expr, r: Expr) -> Expr {
        Expr::Bin(BinOp::Concat, Box::new(l), Box::new(r))
    }

    pub fn neg(e: Expr) -> Expr {
        Expr::Un(UnOp::Neg, Box::new(e))
    }
    pub fn not(e: Expr) -> Expr {
        Expr::Un(UnOp::Not, Box::new(e))
    }
}
