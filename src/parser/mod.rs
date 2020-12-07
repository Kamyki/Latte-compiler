lalrpop_mod!(#[allow(clippy::all)] pub instant, "/parser/instant.rs");
use crate::model::ast::{IStmt, Stmt, Block, Program, Spanned};
use self::instant::programParser;
use crate::parser::CommentState::{OutStr, InStr, SingleLine, MultiLine};
use crate::error_handling::{CheckerResult, FrontendError, Code};
use crate::error_handling::FrontendError::{InvalidComment, ParsingError};



fn wrap_in_block(stmt: Stmt) -> Block {
    match stmt.item {
        IStmt::Block(b) => b,
        _ => {
            let span = stmt.span;
            Block { stmts: vec![stmt], span }
        }
    }
}

pub fn new_spanned<T>(l: usize, item: T, r: usize) -> Spanned<T> {
    Spanned {
        item,
        span: (l, r),
    }
}

pub fn parse(input: &Code) -> CheckerResult<Program> {
    let code = remove_comments(input.source())?;
    let mut errors = Vec::new();
    let result = programParser::new().parse(&mut errors, &code);
    match result {
        Ok(program) => if errors.is_empty() {
            Ok(program)
        } else {
            Err(errors)
        }
        Err(_) => if errors.is_empty() {
            Err(FrontendError::add_done(ParsingError, (0, code.len()), "Unrecognised input"))
        } else {
            Err(errors)
        }
    }
}

#[derive(Eq, PartialEq)]
enum CommentState {
    OutStr(char),
    InStr(char),
    SingleLine(char),
    MultiLine(char),
}

fn remove_comments(input: &str) -> CheckerResult<String> {
    let mut result = String::new();
    let mut state = OutStr('\0');

    for ch in input.chars() {
        state = match (&state, ch) {
            (OutStr(_), '"') => {
                result.push(ch);
                InStr(ch)
            }
            (OutStr(_), '#') => {
                result.push(' ');
                SingleLine(ch)
            }
            (OutStr('/'), '/') => {
                result.pop();
                result.push(' ');
                result.push(' ');
                SingleLine(ch)
            }
            (OutStr('/'), '*') => {
                result.pop();
                result.push(' ');
                result.push(' ');
                MultiLine(ch)
            }
            (OutStr(_), _) => {
                result.push(ch);
                OutStr(ch)
            },
            (SingleLine(_), '\n') => {
                result.push(ch);
                OutStr(ch)
            }
            (SingleLine(_), _) => {
                result.push(' ');
                SingleLine(ch)
            }
            (MultiLine('*'), '/') => {
                result.push(' ');
                OutStr(ch)
            }
            (MultiLine(_), _) => {
                result.push(' ');
                MultiLine(ch)
            }
            (InStr('\\'), '"') => {
                result.push(ch);
                InStr(ch)
            }
            (InStr(_), '"') => {
                result.push(ch);
                OutStr(ch)
            }
            (InStr(_), _) => {
                result.push(ch);
                InStr(ch)
            }
        }
    }
    match state {
        MultiLine(_) => Err(FrontendError::add_done(InvalidComment, (input.len() - 1, input.len()), "Multiline comment isn't closed")),
        _ => Ok(result)
    }
}