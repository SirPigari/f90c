use anyhow::Result;

use crate::ast::Program;
use crate::errors::{CompileError, CompileErrorKind};
use crate::lexer::{Token, TokenKind};

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub fortran);

fn human_expected(name: &str) -> String {
    match name {
        "KW_PROGRAM" => "keyword program".into(),
        "KW_END" => "keyword end".into(),
        "KW_IMPLICIT" => "keyword implicit".into(),
        "KW_NONE" => "keyword none".into(),
        "KW_PRINT" => "keyword print".into(),
        "KW_IF" => "keyword if".into(),
        "KW_THEN" => "keyword then".into(),
        "KW_ELSE" => "keyword else".into(),
        "KW_INTEGER" => "keyword integer".into(),
        "KW_REAL" => "keyword real".into(),
        "KW_DOUBLE" => "keyword double".into(),
        "KW_PRECISION" => "keyword precision".into(),
        "KW_CHARACTER" => "keyword character".into(),
        "KW_LOGICAL" => "keyword logical".into(),
        "KW_LEN" => "keyword len".into(),
        "ID" => "identifier".into(),
        "STR" => "string literal".into(),
        "FLOAT" => "real literal".into(),
        "INT" => "integer literal".into(),
        "TRUE" => "logical .true.".into(),
        "FALSE" => "logical .false.".into(),
        "COMMA" => "','".into(),
        "STAR" => "'*'".into(),
        "EQ" => "'='".into(),
        "LPAREN" => "'('".into(),
        "RPAREN" => "')'".into(),
        other => other.to_lowercase(),
    }
}

fn human_token(tok: &TokenKind) -> String {
    match tok {
        TokenKind::KwProgram => "keyword program".into(),
        TokenKind::KwEnd => "keyword end".into(),
        TokenKind::KwImplicit => "keyword implicit".into(),
        TokenKind::KwNone => "keyword none".into(),
        TokenKind::KwPrint => "keyword print".into(),
        TokenKind::KwRead => "keyword read".into(),
        TokenKind::KwIf => "keyword if".into(),
        TokenKind::KwThen => "keyword then".into(),
        TokenKind::KwElse => "keyword else".into(),
        TokenKind::KwInteger => "keyword integer".into(),
        TokenKind::KwReal => "keyword real".into(),
        TokenKind::KwDouble => "keyword double".into(),
        TokenKind::KwPrecision => "keyword precision".into(),
        TokenKind::KwCharacter => "keyword character".into(),
        TokenKind::KwLogical => "keyword logical".into(),
        TokenKind::KwLen => "keyword len".into(),
        TokenKind::KwDo => "keyword do".into(),
        TokenKind::KwWhile => "keyword while".into(),
        TokenKind::KwSubroutine => "keyword subroutine".into(),
        TokenKind::KwCall => "keyword call".into(),
        TokenKind::KwContains => "keyword contains".into(),
        TokenKind::KwUse => "keyword use".into(),
        TokenKind::KwModule => "keyword module".into(),
        TokenKind::KwSelect => "keyword select".into(),
        TokenKind::KwCase => "keyword case".into(),
        TokenKind::KwDefault => "keyword default".into(),
        TokenKind::Ident(s) => format!("identifier {}", s),
        TokenKind::Str(s) => format!("string literal {:?}", s),
        TokenKind::Float(x) => format!("real literal {}", x),
        TokenKind::Integer(i) => format!("integer literal {}", i),
        TokenKind::True => ".true.".into(),
        TokenKind::False => ".false.".into(),
        TokenKind::Comma => "','".into(),
        TokenKind::Star => "'*'".into(),
        TokenKind::LParen => "'('".into(),
        TokenKind::RParen => "')'".into(),
        TokenKind::DColon => "'::'".into(),
    TokenKind::Colon => "':'".into(),
        TokenKind::Eq => "'='".into(),
        TokenKind::Plus => "'+'".into(),
        TokenKind::Minus => "'-'".into(),
        TokenKind::Slash => "'/'".into(),
        TokenKind::Pow => "'**'".into(),
        TokenKind::Concat => "'//'".into(),
        TokenKind::EqEq => "'=='".into(),
        TokenKind::Ne => "'/='".into(),
        TokenKind::Le => "'<='".into(),
        TokenKind::Ge => "'>='".into(),
        TokenKind::Lt => "'<'".into(),
        TokenKind::Gt => "'>'".into(),
        TokenKind::And => "'.and.'".into(),
        TokenKind::Or => "'.or.'".into(),
        TokenKind::Not => "'.not.'".into(),
        TokenKind::Eqv => "'.eqv.'".into(),
        TokenKind::Neqv => "'.neqv.'".into(),
        TokenKind::KwFunction => "keyword function".into(),
        TokenKind::KwReturn => "keyword return".into(),
        TokenKind::KwBlock => "keyword block".into(),
        TokenKind::KwExit => "keyword exit".into(),
        TokenKind::KwCycle => "keyword cycle".into(),
        TokenKind::KwStop => "keyword stop".into(),
        TokenKind::_WS => "whitespace".into(),
        TokenKind::_COMMENT => "comment".into(),
        TokenKind::Error(e) => format!("error: {}", e),
    }
}

pub fn parse_with_src(
    src: &str,
    tokens: &[Token],
    filename: &str,
) -> Result<Program, Vec<CompileError>> {
    use codespan_reporting::diagnostic::{Diagnostic, Label};
    use codespan_reporting::files::SimpleFile;
    use codespan_reporting::term::{
        emit,
        termcolor::{ColorChoice, StandardStream},
        Config,
    };

    let mut errs: Vec<CompileError> = Vec::new();

    let file = SimpleFile::new(filename, src);
    let mut stderr = StandardStream::stderr(ColorChoice::Auto);

    for t in tokens {
        if let TokenKind::Error(text) = &t.kind {
            let msg = format!("unrecognized token `{}`", text);
            let span = t.span.clone();
            let diag = Diagnostic::error()
                .with_message(&msg)
                .with_labels(vec![Label::primary((), span.clone())]);
            let _ = emit(&mut stderr, &Config::default(), &file, &diag);
            errs.push(CompileError::new(CompileErrorKind::Lex, msg, span));
        }
    }

    let triples: Vec<(usize, TokenKind, usize)> = tokens
        .iter()
        .map(|t| (t.span.start, t.kind.clone(), t.span.end))
        .collect();

    let parsed: Result<Program, Vec<CompileError>> = match fortran::ProgramParser::new()
        .parse(triples.into_iter())
    {
        Ok(p) => Ok(p),
        Err(e) => {
            let (span, msg) = match e {
                lalrpop_util::ParseError::InvalidToken { location } => {
                    ((location)..(location + 1), "invalid token".to_string())
                }
                lalrpop_util::ParseError::UnrecognizedEof { expected, .. } => {
                    let human: Vec<String> = expected.iter().map(|s| human_expected(s)).collect();
                    (
                        (src.len())..(src.len()),
                        format!(
                            "unexpected end of input, expected one of: {}",
                            human.join(", ")
                        ),
                    )
                }
                lalrpop_util::ParseError::UnrecognizedToken {
                    token: (start, tok, end),
                    expected,
                } => {
                    let human: Vec<String> = expected.iter().map(|s| human_expected(s)).collect();
                    (
                        (start)..(end),
                        format!(
                            "unexpected {}: expected one of: {}",
                            human_token(&tok),
                            human.join(", ")
                        ),
                    )
                }
                lalrpop_util::ParseError::ExtraToken {
                    token: (start, tok, end),
                } => ((start)..(end), format!("extra token {}", human_token(&tok))),
                lalrpop_util::ParseError::User { error } => ((0)..(0), format!("{}", error)),
            };
            let diag = Diagnostic::error()
                .with_message(&msg)
                .with_labels(vec![Label::primary((), span.clone())]);
            let _ = emit(&mut stderr, &Config::default(), &file, &diag);
            errs.push(CompileError::new(CompileErrorKind::Parse, msg, span));
            Ok(crate::ast::Program {
                name: "<error>".into(),
                body: vec![],
            })
        }
    };

    match parsed {
        Ok(p) => {
            if errs.is_empty() {
                Ok(p)
            } else {
                Err(errs)
            }
        }
        Err(_) => Err(errs),
    }
}
