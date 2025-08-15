use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum TokenKind {
    #[regex(r"[ \t\r\n]+", logos::skip)]
    _WS,
    #[regex(r"![^\n]*", logos::skip)]
    _COMMENT,
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),
    #[regex(r#"\"([^"\\]|\\.)*\""#, |lex| {
        let s = lex.slice();
        let inner = &s[1..s.len()-1];
        inner.replace("\\\"", "\"")
    })]
    Str(String),
    #[regex(r"([0-9]+\.[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+", |lex| lex.slice().to_string())]
    Float(String),
    #[regex(r"[0-9]+", |lex| lex.slice().to_string())]
    Integer(String),
    #[token(".true.", ignore(ascii_case))]
    True,
    #[token(".false.", ignore(ascii_case))]
    False,
    #[token("program", ignore(ascii_case))]
    KwProgram,
    #[token("end", ignore(ascii_case))]
    KwEnd,
    #[token("function", ignore(ascii_case))]
    KwFunction,
    #[token("return", ignore(ascii_case))]
    KwReturn,
    #[token("implicit", ignore(ascii_case))]
    KwImplicit,
    #[token("none", ignore(ascii_case))]
    KwNone,
    #[token("print", ignore(ascii_case))]
    KwPrint,
    #[token("read", ignore(ascii_case))]
    KwRead,
    #[token("if", ignore(ascii_case))]
    KwIf,
    #[token("then", ignore(ascii_case))]
    KwThen,
    #[token("else", ignore(ascii_case))]
    KwElse,
    #[token("do", ignore(ascii_case))]
    KwDo,
    #[token("integer", ignore(ascii_case))]
    KwInteger,
    #[token("real", ignore(ascii_case))]
    KwReal,
    #[token("double", ignore(ascii_case))]
    KwDouble,
    #[token("precision", ignore(ascii_case))]
    KwPrecision,
    #[token("character", ignore(ascii_case))]
    KwCharacter,
    #[token("logical", ignore(ascii_case))]
    KwLogical,
    #[token("len", ignore(ascii_case))]
    KwLen,
    #[token("subroutine", ignore(ascii_case))]
    KwSubroutine,
    #[token("call", ignore(ascii_case))]
    KwCall,
    #[token("contains", ignore(ascii_case))]
    KwContains,
    #[token("use", ignore(ascii_case))]
    KwUse,
    #[token("module", ignore(ascii_case))]
    KwModule,
    #[token("select", ignore(ascii_case))]
    KwSelect,
    #[token("case", ignore(ascii_case))]
    KwCase,
    #[token("default", ignore(ascii_case))]
    KwDefault,
    #[token("**")]
    Pow,
    #[token("//")]
    Concat,
    #[token("==")]
    EqEq,
    #[token("/=")]
    Ne,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,
    #[token("::")]
    DColon,
    #[token("=")]
    Eq,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token(".and.", ignore(ascii_case))]
    And,
    #[token(".or.", ignore(ascii_case))]
    Or,
    #[token(".not.", ignore(ascii_case))]
    Not,
    #[token(".eqv.", ignore(ascii_case))]
    Eqv,
    #[token(".neqv.", ignore(ascii_case))]
    Neqv,
    Error(String),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: std::ops::Range<usize>,
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut lexer = TokenKind::lexer(input);
    let mut tokens = Vec::new();
    while let Some(res) = lexer.next() {
        match res {
            Ok(kind) => {
                if matches!(kind, TokenKind::_WS | TokenKind::_COMMENT) {
                    continue;
                }
                let span = lexer.span();
                tokens.push(Token { kind, span });
            }
            Err(_) => {
                let span = lexer.span();
                let text = input.get(span.clone()).unwrap_or("").to_string();
                tokens.push(Token {
                    kind: TokenKind::Error(text),
                    span,
                });
            }
        }
    }
    tokens
}
