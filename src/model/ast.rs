use core::fmt;
use std::fmt::Display;

use serde::export::Formatter;

#[derive(Debug)]
pub struct Program {
    pub defs: Vec<TopDef>
}

pub type Span = (usize, usize);

#[derive(Debug)]
pub enum TopDef {
    Function(Function),
    Class(Class),
}

#[derive(Debug)]
pub struct Class {
    pub id: Id,
    pub fields: Vec<Arg>,
}

#[derive(Debug)]
pub struct Function {
    pub ret_type: Type,
    pub id: Id,
    pub args: Vec<Arg>,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Arg(pub Type, pub Id);

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

pub type Stmt = Spanned<IStmt>;

#[derive(Debug)]
pub enum IStmt {
    Empty,
    Block(Block),
    Decl {
        t: Type,
        items: Vec<Item>,

    },
    Asg {
        i: Target,
        e: Expr,
    },
    Incr(Target),
    Decr(Target),
    Ret(Expr),
    VRet,
    Cond {
        c: Expr,
        if_true: Block,
    },
    CondElse {
        c: Expr,
        if_true: Block,
        if_false: Block,
    },
    While {
        c: Expr,
        while_true: Block,
    },
    Expr(Expr),
}

#[derive(Debug)]
pub enum Target {
    Id(Id),
    Field(Field),
}

pub type Type = Spanned<IType>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IType {
    Int,
    String,
    Boolean,
    Void,
    Class(String),
}

impl Display for IType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            IType::Int => "int",
            IType::String => "string",
            IType::Boolean => "boolean",
            IType::Void => "void",
            IType::Class(c) => c,
        })
    }
}

#[derive(Debug)]
pub enum Item {
    NoInit(Id),
    Init {
        i: Id,
        e: Expr,
    },
}

pub type Expr = Spanned<IExpr>;

#[derive(Debug)]
pub enum IExpr {
    Unary {
        o: UnOp,
        e: Box<Expr>,
    },
    Binary {
        o: BinOp,
        l: Box<Expr>,
        r: Box<Expr>,
    },
    Var(Id),
    Int(i32),
    Bool(bool),
    Null,
    FunCall {
        name: Id,
        args: Vec<Expr>,
    },
    String(String),
    Paren(Box<Expr>),
    Field(Field),
    Object(Type),
    Cast {
        t: Type,
        e: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct Field {
    pub e: Box<Expr>,
    pub id: Id,
}

#[derive(Debug)]
pub enum UnOp {
    IntNegation,
    BoolNegation,
}

#[derive(Debug)]
pub enum BinOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
    And,
    Or,
}

pub type Id = Spanned<String>;
