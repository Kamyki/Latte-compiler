use crate::model::ast::*;
use std::collections::HashMap;
use crate::error_handling::{FrontendError, CheckerResult, AccErrors, FoldErrors};
use crate::error_handling::FrontendError::*;
use crate::frontend::global_analyser::{GlobalAnalyser, FunctionSignature};


pub enum FunctionAnalyser<'a> {
    Root(GlobalAnalyser<'a>),
    Frame {
        parent: &'a FunctionAnalyser<'a>,
        global: &'a GlobalAnalyser<'a>,
        locals: HashMap<String, Type>,
        ret_type: &'a Type,
    },
}

impl<'a> FunctionAnalyser<'a> {
    pub fn new(context: GlobalAnalyser<'a>) -> Self {
        FunctionAnalyser::Root(context)
    }

    pub fn new_frame(parent: &'a FunctionAnalyser<'a>, ret_type: &'a Type) -> Self {
        let global = match parent {
            FunctionAnalyser::Root(global) => global,
            FunctionAnalyser::Frame { global, .. } => global
        };
        FunctionAnalyser::Frame { parent, global, locals: HashMap::new(), ret_type }
    }

    pub fn insert_var(&mut self, id: Id, t: Type) -> CheckerResult<()> {
        let span = id.span;
        match self {
            FunctionAnalyser::Root(_) => Err(FrontendError::add_done(GlobalVariable, id.span, "This cannot be defined here")),
            FunctionAnalyser::Frame { ref mut locals, .. } => {
                match locals.insert(id.item, t) {
                    None => Ok(()),
                    Some(_) => Err(DoubleDeclaration.add_done(span, "There is another variable with that name")),
                }
            }
        }
    }

    pub fn find_var(&self, id: &Id) -> Option<&Type> {
        match self {
            FunctionAnalyser::Root(_) => None,
            FunctionAnalyser::Frame { parent, locals, .. } => {
                match locals.get(id.item.as_str()) {
                    None => parent.find_var(id),
                    Some(a) => Some(a)
                }
            }
        }
    }

    pub fn find_function(&self, id: &Id) -> Option<&FunctionSignature> {
        match self {
            FunctionAnalyser::Root(ga) => ga.functions.get(id.item.as_str()),
            FunctionAnalyser::Frame { global, .. } => global.functions.get(id.item.as_str()),
        }
    }

    pub fn ret_type(&self) -> &'a Type {
        match self {
            FunctionAnalyser::Root(_) => panic!("It shouldn't happen: Checking return type in global context"),
            FunctionAnalyser::Frame { ret_type, .. } => ret_type
        }
    }
}

impl<'a> FunctionAnalyser<'a> {

    pub fn check_function(&self, top_def: &TopDef) -> CheckerResult<()> {
        let mut function_context = FunctionAnalyser::new_frame(self, &top_def.ret_type);
        top_def.args.iter()
            .map(|a| function_context.insert_var(a.1.clone(), a.0.clone()))
            .acc()
            .and_then(|_: ()| function_context.check_block(&top_def.block))
            .and_then(|r| match (r, &top_def.ret_type.item) {
                (true, _) | (false, IType::Void) => Ok(()),
                _ => Err(WrongReturnType.add_done(top_def.block.span, "Not all execution branches return expected type"))
            })
    }

    fn check_block(&self, block: &Block) -> CheckerResult<bool> {
        let mut block_context = FunctionAnalyser::new_frame(self, self.ret_type());
        block.stmts.iter()
            .map(|stmt| block_context.check_stmt(stmt))
            .acc_fold(false, |a, b| a || b)
            .map_err(|mut err| {
                for mut e in &mut err {
                    e.over_span = Some(block.span);
                }
                err
            })
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> CheckerResult<bool> {
        match &stmt.item {
            IStmt::Empty => Ok(false),
            IStmt::Block(b) => self.check_block(b),
            IStmt::Decl { t, items } => {
                items.iter().map(|it| match it {
                    Item::NoInit(i) => self.insert_var(i.clone(), t.clone()),
                    Item::Init { i, e } => self.insert_var(i.clone(), t.clone())
                        .and(self.check_expr(e).and_then(|e_type| Self::match_type(t, &e_type)))
                }).acc().and_then(|_: ()| Ok(false))
            }
            IStmt::Asg { i, e } => {
                let t1 = self.find_var(i);
                self.check_expr(e)
                    .and_then(|t2| match t1 {
                        None => Err(UndefinedVariable.add_done(stmt.span, "Assignment to undefined variable")),
                        Some(tt1) if tt1.item == t2.item => Ok(false),
                        Some(tt1) => Err(MismatchedTypes.add(stmt.span, "Wrong type in assigment")
                            .add(tt1.span, "Should be this type")
                            .done())
                    })
            }
            IStmt::Incr(_) => Ok(false),
            IStmt::Decr(_) => Ok(false),
            IStmt::Ret(e) => self.check_expr(e)
                .and_then(|t| Self::match_type(self.ret_type(), &t))
                .and(Ok(true)),
            IStmt::VRet => Self::match_type(self.ret_type(), &Type { item: IType::Void, span: stmt.span })
                .and(Ok(true)),
            IStmt::Cond { c, if_true } => self.check_cond(c).and_then(|cond| {
                self.check_block(if_true).and_then(|t1| match cond {
                    Some(true) => Ok(t1),
                    _ => Ok(false),
                })
            }),
            IStmt::CondElse { c, if_true, if_false } => {
                self.check_cond(c).and_then(|cond| {
                    self.check_block(if_true).and_then(|t1|
                        self.check_block(if_false).and_then(|t2| match cond {
                            Some(true) => Ok(t1),
                            Some(false) => Ok(t2),
                            _ => Ok(t1 && t2),
                        }))
                })
            }
            IStmt::While { c, while_true } => self.check_cond(c).and_then(|cond| match cond {
                Some(false) => self.check_block(while_true).and(Ok(false)),
                _ => self.check_block(while_true)
            }),
            IStmt::Expr(e) => self.check_expr(e).and(Ok(false)),
        }
    }

    fn check_expr(&self, expr: &Expr) -> CheckerResult<Type> {
        match &expr.item {
            IExpr::Unary { o, e } => {
                self.check_expr(e)
                    .and_then(|t| self.check_un_op(o, &t))
            }
            IExpr::Binary { o, l, r } => {
                self.check_expr(l)
                    .and_then(|lt| self.check_expr(r)
                        .and_then(|rt|
                            self.check_bin_op(o, &lt, &rt)
                        )
                    )
            }
            IExpr::Var(i) => match self.find_var(i) {
                None => Err(UndefinedVariable.add_done(expr.span, "Use of undefined variable")),
                Some(t) => Ok(t.clone())
            },
            IExpr::Int(_) => Ok(Type { item: IType::Int, span: expr.span }),
            IExpr::Bool(_) => Ok(Type { item: IType::Boolean, span: expr.span }),
            IExpr::FunCall { name, args } => match self.find_function(name) {
                None => Err(UndefinedFunction.add_done(expr.span, "Call to undefined function")),
                Some(fg) => {
                    args.iter().map(|a| self.check_expr(a)).acc()
                        .and_then(|args| fg.check_call(args, expr.span))
                        .and(Ok(Type { item: fg.ret_type.item.clone(), span: expr.span }))
                }
            }
            IExpr::String(_) => Ok(Type { item: IType::String, span: expr.span }),
            IExpr::Paren(e) => self.check_expr(e),
        }
    }

    fn match_type(t1: &Type, t2: &Type) -> CheckerResult<()> {
        if t1.item == t2.item {
            Ok(())
        } else {
            Err(MismatchedTypes.add(t1.span, "Type mismatch here")
                .add(t2.span, "and here")
                .done())
        }
    }

    fn check_cond(&self, expr: &Expr) -> CheckerResult<Option<bool>> {
        self.check_expr(expr).and(
            match expr.item {
                IExpr::Bool(b) => Ok(Some(b)),
                _ => Ok(None)
            }
        )
    }

    fn check_un_op(&self, o: &UnOp, t: &Type) -> CheckerResult<Type> {
        match o {
            UnOp::IntNegation => {
                if t.item == IType::Int {
                    Ok(Type { item: IType::Int, span: t.span })
                } else {
                    Err(MismatchedTypes.add_done(t.span, "Numerical negation works only for int"))
                }
            }
            UnOp::BoolNegation => {
                if t.item == IType::Boolean {
                    Ok(Type { item: IType::Boolean, span: t.span })
                } else {
                    Err(MismatchedTypes.add_done(t.span, "Logical negation works only for booleans"))
                }
            }
        }
    }

    fn check_bin_op(&self, o: &BinOp, l: &Type, r: &Type) -> CheckerResult<Type> {
        match o {
            BinOp::Mul => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Int, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Multiplication works only for ints"))
                }
            }
            BinOp::Div => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Int, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Division works only for ints"))
                }
            }
            BinOp::Mod => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Int, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Modulo works only for ints"))
                }
            }
            BinOp::Add => {
                if l.item == r.item && (r.item == IType::Int || r.item == IType::String) {
                    Ok(Type { item: r.item.clone(), span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Addition works only for arguments od the smae type (int or string)"))
                }
            }
            BinOp::Sub => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Int, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Subtraction works only for ints"))
                }
            }
            BinOp::LT => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Relation less_than works only for ints"))
                }
            }
            BinOp::LE => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Relation less_equal works only for ints"))
                }
            }
            BinOp::GT => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Relation greater_than works only for ints"))
                }
            }
            BinOp::GE => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Relation greater_equal works only for ints"))
                }
            }
            BinOp::EQ => {
                if l.item == r.item {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Relation equal works for the same types"))
                }
            }
            BinOp::NE => {
                if l.item == r.item {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Relation not_equal works for the same types"))
                }
            }
            BinOp::And => {
                if l.item == r.item && r.item == IType::Boolean {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Logical and works for booleans"))
                }
            }
            BinOp::Or => {
                if l.item == r.item && r.item == IType::Boolean {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done((l.span.1, r.span.0), "Logical or works for booleans"))
                }
            }
        }
    }
}