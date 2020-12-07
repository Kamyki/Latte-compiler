use std::iter::{FromIterator, Map};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use crate::model::ast::Span;

pub type CheckerResult<T> = Result<T, Vec<Error>>;

pub type Code<'a, 'b> = SimpleFile<&'a str, &'b str>;

#[derive(Clone)]
pub enum FrontendError {
    DoubleDeclaration,
    WrongReturnType,
    GlobalVariable,
    LocalFunction,
    InvalidComment,
    ParsingError,
    UndefinedVariable,
    UndefinedFunction,
    MismatchedTypes,
    FunctionCall,
    WrongConditionType,
    ArithmeticError,
    FieldAccess,
    UnknownClass,
    CastingError,
    WrongExtend,
    CircularClassHierarchy,
    VirtualMethod,
    MissingMain
}

impl From<&FrontendError> for String {
    fn from(e: &FrontendError) -> Self {
        match e {
            FrontendError::DoubleDeclaration => "Double declaration of element",
            FrontendError::WrongReturnType => "Mismatch of return type",
            FrontendError::GlobalVariable => "Global variable declaration",
            FrontendError::LocalFunction => "Local function definition",
            FrontendError::InvalidComment => "Invalid comment",
            FrontendError::ParsingError => "Parsing Error",
            FrontendError::UndefinedVariable => "Undefined variable",
            FrontendError::UndefinedFunction => "Undefined function",
            FrontendError::MismatchedTypes => "Mismatched types",
            FrontendError::FunctionCall => "Function call",
            FrontendError::WrongConditionType => "Wrong condition type",
            FrontendError::ArithmeticError => "Division by zero",
            FrontendError::FieldAccess => "Incorrect field access",
            FrontendError::UnknownClass => "Unknown class",
            FrontendError::CastingError => "Casting error",
            FrontendError::WrongExtend => "Class extending error",
            FrontendError::CircularClassHierarchy => "Detected circular class hierarchy",
            FrontendError::VirtualMethod => "Virtual method not supported",
            FrontendError::MissingMain => "Couldn't find main"
        }.to_string()
    }
}

impl FrontendError {
    pub fn add_done<S: Into<String>>(self, span: Span, msg: S) -> Vec<Error> {
        vec![Error {
            e_type: self,
            over_span: None,
            msgs: vec![(span, msg.into())],
        }]
    }

    pub fn add<S: Into<String>>(self, span: Span, msg: S) -> Error {
        Error {
            e_type: self,
            over_span: None,
            msgs: vec![(span, msg.into())],
        }
    }
}

pub struct Error {
    pub e_type: FrontendError,
    pub over_span: Option<Span>,
    pub msgs: Vec<(Span, String)>,
}

impl Error {
    pub fn add<S: Into<String>>(mut self, span: Span, msg: S) -> Error {
        self.msgs.push((span, msg.into()));
        self
    }

    pub fn add_over_span(mut self, span: Span) -> Error {
        self.over_span = Some(span);
        self
    }

    pub fn done(self) -> Vec<Self> {
        vec![self]
    }
}

pub trait AccErrors<K> {
    fn acc(self) -> CheckerResult<K>;
}

pub trait FoldErrors<K, A, FF>
    where
        FF: FnMut(K, A) -> K
{
    fn acc_fold(self, i: K, f: FF) -> CheckerResult<K>;
}

impl<K, A, B, FF, F> FoldErrors<K, A, FF> for Map<B, F>
    where
        B: Iterator + Sized,
        F: FnMut(B::Item) -> CheckerResult<A>,
        FF: FnMut(K, A) -> K,
{
    fn acc_fold(self, i: K, f: FF) -> CheckerResult<K> {
        let r = self.fold(Ok(vec![]), |a, r| match r {
            Ok(x) => match a {
                Ok(mut v) => {
                    v.push(x);
                    Ok(v)
                }
                Err(_) => a,
            }
            Err(mut err1) => match a {
                Ok(_) => Err(err1),
                Err(err2) => {
                    err1.extend(err2);
                    Err(err1)
                }
            }
        });
        match r {
            Ok(v) => Ok(v.into_iter().fold(i, f)),
            Err(x) => Err(x)
        }
    }
}

impl<B, F, T, K> AccErrors<K> for Map<B, F>
    where
        B: Iterator + Sized,
        F: FnMut(B::Item) -> CheckerResult<T>,
        K: FromIterator<T>,
{
    fn acc(self) -> CheckerResult<K> {
        let r = self.fold(Ok(vec![]), |a, r| match r {
            Ok(x) => match a {
                Ok(mut v) => {
                    v.push(x);
                    Ok(v)
                }
                Err(_) => a,
            }
            Err(mut err1) => match a {
                Ok(_) => Err(err1),
                Err(err2) => {
                    err1.extend(err2);
                    Err(err1)
                }
            }
        });
        match r {
            Ok(v) => Ok(v.into_iter().collect()),
            Err(x) => Err(x)
        }
    }
}

impl<K> AccErrors<K> for Vec<CheckerResult<K>> where
    K: FromIterator<K>,
{
    fn acc(self) -> CheckerResult<K> {
        self.into_iter().map(|x| x).acc()
    }
}


pub fn map_file<'a, 'b>(name: &'a str, source: &'b str) -> Code<'a, 'b> {
    SimpleFile::new(name, source)
}

pub fn print_errors(file: &Code, errors: &[Error]) -> () {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    for Error { e_type, over_span, msgs } in errors {
        let mut labels = vec![];
        match over_span {
            Some(s) => labels.push(Label::secondary((), s.0..s.1)),
            None => {}
        };
        for (span, msg) in msgs {
            if span.0 <= span.1 {
                labels.push(Label::primary((), span.0..span.1).with_message(msg));
            }
        }
        let diagnostic = Diagnostic::error()
            .with_message(e_type)
            .with_labels(labels);
        term::emit(&mut writer.lock(), &config, file, &diagnostic).unwrap();
    }
}
