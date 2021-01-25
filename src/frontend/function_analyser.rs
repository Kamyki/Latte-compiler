use crate::model::ast::*;
use std::collections::HashMap;
use crate::error_handling::{CheckerResult, AccErrors, FoldErrors};
use crate::error_handling::FrontendError::*;
use crate::frontend::global_analyser::{GlobalAnalyser, FunctionSignature, ClassSignature};


pub enum FunctionAnalyser<'a> {
    Root(&'a GlobalAnalyser, HashMap<String, Type>),
    Frame {
        parent: &'a FunctionAnalyser<'a>,
        global: &'a GlobalAnalyser,
        locals: HashMap<String, Type>,
        ret_type: &'a Type,
    },
}

impl<'a> FunctionAnalyser<'a> {
    pub fn new(context: &'a GlobalAnalyser) -> Self {
        FunctionAnalyser::Root(context, HashMap::new())
    }

    pub fn new_frame(parent: &'a FunctionAnalyser<'a>, ret_type: &'a Type) -> Self {
        let global = match parent {
            FunctionAnalyser::Root(global, ..) => global,
            FunctionAnalyser::Frame { global, .. } => global
        };
        FunctionAnalyser::Frame { parent, global, locals: HashMap::new(), ret_type }
    }

    pub fn insert_var(&mut self, id: Id, t: Type) -> CheckerResult<()> {
        let span = id.span;
        match t.item {
            IType::Void => Err(VoidVariable.add_done(id.span, "Void variable arn't allowed")),
            _ => Ok(()),
        }.and(
            match self {
                FunctionAnalyser::Root(_, globals) => match globals.insert(id.item, t) {
                    None => Ok(()),
                    Some(_) => Err(DoubleDeclaration.add_done(span, "There is another field with that name"))
                },
                FunctionAnalyser::Frame { ref mut locals, .. } => match locals.insert(id.item, t) {
                    None => Ok(()),
                    Some(_) => Err(DoubleDeclaration.add_done(span, "There is another variable with that name")),
                }
            })
    }

    fn find_var(&self, id: &Id) -> Option<&Type> {
        match self {
            FunctionAnalyser::Root(_, globals) => globals.get(id.item.as_str()),
            FunctionAnalyser::Frame { parent, locals, .. } => {
                match locals.get(id.item.as_str()) {
                    None => parent.find_var(id),
                    Some(a) => Some(a)
                }
            }
        }
    }

    fn find_function(&self, id: &Id) -> Option<&FunctionSignature> {
        match self {
            FunctionAnalyser::Root(ga, ..) => ga.functions.get(id.item.as_str()),
            FunctionAnalyser::Frame { global, .. } => global.functions.get(id.item.as_str()),
        }
    }

    fn find_class(&self, id: &str) -> Option<&ClassSignature> {
        match self {
            FunctionAnalyser::Root(ga, ..) => ga.classes.get(id),
            FunctionAnalyser::Frame { global, .. } => global.classes.get(id),
        }
    }

    fn ret_type(&self) -> &'a Type {
        match self {
            FunctionAnalyser::Root(..) => panic!("It shouldn't happen: Checking return type in global context"),
            FunctionAnalyser::Frame { ret_type, .. } => ret_type
        }
    }

    fn extends(&self, cs: &ClassSignature, t: &IType) -> bool {
        match self {
            FunctionAnalyser::Root(ga, ..) => ga.extends(cs, t),
            FunctionAnalyser::Frame { global, .. } => global.extends(cs, t),
        }
    }
}

impl<'a> FunctionAnalyser<'a> {
    pub fn check_function(&self, top_def: &Function) -> CheckerResult<()> {
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

    fn check_target(&self, target: &Target) -> CheckerResult<Type> {
        match target {
            Target::Id(id) => self.find_var(id).ok_or(
                UndefinedVariable.add_done(id.span, "Assignment to undefined variable")
            ).map(|t| t.clone()),
            Target::Field(f) => self.check_field(f)
        }
    }

    fn check_field(&self, field: &Field) -> CheckerResult<Type> {
        self.check_expr(&field.e).and_then(|(t, _)| {
            if let IType::Class(c) = t.item {
                self.find_class(&c).ok_or(UnknownClass.add_done(field.id.span, "Use of unknown field"))
                    .and_then(|cs| cs.find_var(&field.id).ok_or(
                        UndefinedVariable.add_done(field.id.span, "Assignment to unknown field in class")
                    ).map(|r| r.clone()))
            } else {
                Err(FieldAccess.add_done(field.e.span, "Accessing field of not class type"))
            }
        })
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> CheckerResult<bool> {
        match &stmt.item {
            IStmt::Empty => Ok(false),
            IStmt::Block(b) => self.check_block(b),
            IStmt::Decl { t, items } => {
                items.iter().map(|it| match it {
                    Item::NoInit(i) => self.insert_var(i.clone(), t.clone()),
                    Item::Init { i, e } => self.check_expr(e).and_then(|(e_type, _)| self.insert_var(i.clone(), t.clone()).and(self.match_type(t, &e_type)))
                }).acc().and_then(|_: ()| Ok(false))
            }
            IStmt::Asg { i, e } => self.check_target(i)
                .and_then(|t1| self.check_expr(e)
                    .and_then(|(t2, err)| if t1.item == t2.item {
                        Ok(err)
                    } else if let IType::Class(c) = t2.item {
                        if self.extends(self.find_class(c.as_str()).unwrap(), &t1.item) {
                            Ok(false)
                        } else {
                            Err(MismatchedTypes.add(stmt.span, "Wrong type in assigment")
                                .add(t1.span, "Tried to cast to this type")
                                .done())
                        }
                    } else {
                        Err(MismatchedTypes.add(e.span, "Wrong type in assigment")
                            .add(t1.span, "Should be this type")
                            .done())
                    })),

            IStmt::Incr(t) |
            IStmt::Decr(t) => self.check_target(t).and_then(|tt| if tt.item == IType::Int {
                Ok(false)
            } else {
                Err(ArithmeticError.add_done(stmt.span, "++ and -- allowed with ints only"))
            }),
            IStmt::Ret(e) => self.check_expr(e)
                .and_then(|(t, _)| self.match_type(self.ret_type(), &t))
                .and(Ok(true)),
            IStmt::VRet => self.match_type(self.ret_type(), &Type { item: IType::Void, span: stmt.span })
                .and(Ok(true)),
            IStmt::Cond { c, if_true } => self.check_cond(c).and_then(|(cond, err)| {
                self.check_block(if_true).and_then(|t1| match cond {
                    Some(true) => Ok(t1 || err),
                    _ => Ok(err),
                })
            }),
            IStmt::CondElse { c, if_true, if_false } => {
                self.check_cond(c).and_then(|(cond, err)| {
                    self.check_block(if_true).and_then(|t1|
                        self.check_block(if_false).and_then(|t2| match cond {
                            Some(true) => Ok(t1 || err),
                            Some(false) => Ok(t2 || err),
                            _ => Ok((t1 && t2) || err),
                        }))
                })
            }
            IStmt::While { c, while_true } => self.check_cond(c).and_then(|(cond, err)| match cond {
                Some(false) => self.check_block(while_true).and(Ok(err)),
                _ => self.check_block(while_true).map(|b| b || err)
            }),
            IStmt::Expr(e) => self.check_expr(e).and_then(|(_, err)| Ok(err)),
        }
    }

    fn check_expr(&self, expr: &Expr) -> CheckerResult<(Type, bool)> {
        match &expr.item {
            IExpr::Unary { o, e } => {
                self.check_expr(e)
                    .and_then(|(t,f)| self.check_un_op(o, &t).map(|t| (t, f)))
            }
            IExpr::Binary { o, l, r } => {
                self.check_expr(l)
                    .and_then(|(lt, lf)| self.check_expr(r)
                        .and_then(|(rt, rf)|
                            self.check_bin_op(o, &lt, &rt).map(|t| (t, lf || rf))
                        )
                    )
            }
            IExpr::Var(i) => match self.find_var(i) {
                None => Err(UndefinedVariable.add_done(expr.span, "Use of undefined variable")),
                Some(t) => Ok((t.clone(), false))
            },
            IExpr::Int(_) => Ok((Type { item: IType::Int, span: expr.span }, false)),
            IExpr::Bool(_) => Ok((Type { item: IType::Boolean, span: expr.span }, false)),
            IExpr::FunCall { name, args } => match name {
                Target::Id(id) => {
                    self.find_function(id).ok_or(UndefinedFunction.add_done(expr.span, "Call to undefined function"))
                }
                Target::Field(Field { e, id }) => self.check_expr(e).and_then(|(t, _)| if let IType::Class(c) = t.item {
                    self.find_class(c.as_str()).ok_or(
                        UndefinedVariable.add_done(e.span, "Unknown class type")
                    ).and_then(|o| o.find_method(id).ok_or(UndefinedFunction.add_done(id.span, "Use of undefined class method")))
                } else {
                    Err(FieldAccess.add_done(expr.span, "Accessing method on non object"))
                })
            }.and_then(|fg| args.iter().map(|a| self.check_expr(a)).acc()
                .and_then(|args_err: Vec<(Type, bool)>| {
                    let args = args_err.iter().map(|(a,_)| a).cloned().collect();
                    let err = args_err.iter().map(|(_,b)| b).cloned().fold(false, |a, b| a || b);
                    fg.check_call(args, expr.span).and(Ok(err))
                })
                .and_then(|err|
                    match name {
                        Target::Id(id) if id.item == "error" => Ok((Type { item: fg.ret_type.item.clone(), span: expr.span }, true)),
                        _ => Ok((Type { item: fg.ret_type.item.clone(), span: expr.span }, err))
                    })),
            IExpr::String(_) => Ok((Type { item: IType::String, span: expr.span }, false)),
            IExpr::Paren(e) => self.check_expr(e),
            IExpr::Field(f) => self.check_field(f).map(|t| (t, false)),
            IExpr::Object(o) => Ok((o.clone(), false)),
            IExpr::Null => Ok((Type { span: expr.span, item: IType::Null }, false)),
            IExpr::Cast { t, e } => if let IType::Class(_) = &t.item {
                self.check_expr(e).and_then(|(et, f)| if et.item == t.item || et.item == IType::Null {
                    Ok((t.clone(), f))
                } else {
                    Err(CastingError.add_done(e.span, "Cannot cast expresion to intended type"))
                })
            } else {
                Err(CastingError.add_done(t.span, "Casting possible only with class types"))
            }
        }
    }

    fn match_type(&self, t1: &Type, t2: &Type) -> CheckerResult<()> {
        match (&t1.item, &t2.item) {
            (v, r) if v == r => Ok(()),
            (IType::Class(c), IType::Class(v)) =>  {
                let mut b = self.find_class(v).unwrap();
                while let Some(id) = &b.super_class {
                    if c == &id.item {
                        return Ok(())
                    }
                    b = self.find_class(&id.item).unwrap();
                }
                Err(MismatchedTypes.add(t1.span, "Type mismatch here")
                    .add(t2.span, "and here")
                    .done())
            }
            _ => Err(MismatchedTypes.add(t1.span, "Type mismatch here")
                .add(t2.span, "and here")
                .done())
        }
    }

    fn check_cond(&self, expr: &Expr) -> CheckerResult<(Option<bool>, bool)> {
        self.check_expr(expr).and_then(|(t, err)|
            if t.item == IType::Boolean {
                Ok(())
            } else {
                Err(WrongConditionType.add_done(expr.span, "Not boolean condition expression"))
            }.and(
                match expr.item {
                    IExpr::Bool(b) => Ok((Some(b), err)),
                    _ => Ok((None, err))
                })
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
        match o.item {
            IBinOp::Mul => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Int, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Multiplication works only for ints"))
                }
            }
            IBinOp::Div => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Int, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Division works only for ints"))
                }
            }
            IBinOp::Mod => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Int, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Modulo works only for ints"))
                }
            }
            IBinOp::Add => {
                if l.item == r.item && (r.item == IType::Int || r.item == IType::String) {
                    Ok(Type { item: r.item.clone(), span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Addition works only for arguments od the smae type (int or string)"))
                }
            }
            IBinOp::Sub => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Int, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Subtraction works only for ints"))
                }
            }
            IBinOp::LT => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Relation less_than works only for ints"))
                }
            }
            IBinOp::LE => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Relation less_equal works only for ints"))
                }
            }
            IBinOp::GT => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Relation greater_than works only for ints"))
                }
            }
            IBinOp::GE => {
                if l.item == r.item && r.item == IType::Int {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Relation greater_equal works only for ints"))
                }
            }
            IBinOp::EQ => {
                if l.item == r.item {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Relation equal works for the same types"))
                }
            }
            IBinOp::NE => {
                if l.item == r.item {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Relation not_equal works for the same types"))
                }
            }
            IBinOp::And => {
                if l.item == r.item && r.item == IType::Boolean {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Logical and works for booleans"))
                }
            }
            IBinOp::Or => {
                if l.item == r.item && r.item == IType::Boolean {
                    Ok(Type { item: IType::Boolean, span: (l.span.0, r.span.1) })
                } else {
                    Err(MismatchedTypes.add_done(o.span, "Logical or works for booleans"))
                }
            }
        }
    }
}