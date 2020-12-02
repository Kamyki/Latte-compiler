use crate::model::ast::*;
use crate::error_handling::{CheckerResult, AccErrors};
use crate::error_handling::FrontendError::ArithmeticError;

pub struct Simplifier;

impl Simplifier {
    pub fn new() -> Self {
        Self {}
    }

    pub fn simplify_expressions_in_ast(&self, ast: &mut Program) -> CheckerResult<()> {
        let mut res = vec![];

        for top_def in &mut ast.defs {
            res.push(self.simplify_block(&mut top_def.block));
        }
        res.acc()
    }

    fn simplify_block(&self, block: &mut Block) -> CheckerResult<()> {
        let mut res = vec![];
        for stmt in &mut block.stmts {
            let a = match &mut stmt.item {
                IStmt::Empty => Ok(()),
                IStmt::Block(b) => self.simplify_block(b),
                IStmt::Decl { items, .. } => {
                    for item in items {
                        let b = match item {
                            Item::NoInit(_) => Ok(()),
                            Item::Init { e, .. } => self.simplify_expression(e),
                        };
                        res.push(b);
                    }
                    Ok(())
                }
                IStmt::Asg { e, .. } => self.simplify_expression(e),
                IStmt::Incr(_) => Ok(()),
                IStmt::Decr(_) => Ok(()),
                IStmt::Ret(e) => self.simplify_expression(e),
                IStmt::VRet => Ok(()),
                IStmt::Cond { c, if_true } => {
                    res.push(self.simplify_expression(c));
                    self.simplify_block(if_true)
                }
                IStmt::CondElse { c, if_true, if_false } => {
                    res.push(self.simplify_expression(c));
                    res.push(self.simplify_block(if_true));
                    self.simplify_block(if_false)
                }
                IStmt::While { c, while_true } => {
                    res.push(self.simplify_expression(c));
                    self.simplify_block(while_true)
                }
                IStmt::Expr(e) => self.simplify_expression(e),
            };
            res.push(a);
        }
        res.acc()
    }

    fn simplify_expression(&self, expr: &mut Expr) -> CheckerResult<()> {
        match &mut expr.item {
            IExpr::Unary { o, e } => match (o, &e.item) {
                (UnOp::IntNegation, IExpr::Int(v)) => {
                    expr.item = IExpr::Int(-v);
                    Ok(())
                }
                (UnOp::BoolNegation, IExpr::Bool(b)) => {
                    expr.item = IExpr::Bool(!b);
                    Ok(())
                }
                _ => Ok(()),
            },
            IExpr::Binary { o, l, r } => match (o, &l.item, &r.item) {
                (BinOp::Mul, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Int(v1 * v2);
                    Ok(())
                }
                (BinOp::Add, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Int(v1 + v2);
                    Ok(())
                }
                (BinOp::Sub, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Int(v1 - v2);
                    Ok(())
                }
                (BinOp::Div, IExpr::Int(_), IExpr::Int(0)) => Err(ArithmeticError.add_done(expr.span, "Division by zero here")),
                (BinOp::Div, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Int(v1 / v2);
                    Ok(())
                }
                (BinOp::Mod, IExpr::Int(_), IExpr::Int(0)) => Err(ArithmeticError.add_done(expr.span, "Modulo by zero here")),
                (BinOp::Mod, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Int(v1 % v2);
                    Ok(())
                }
                (BinOp::LT, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Bool(v1 < v2);
                    Ok(())
                }
                (BinOp::LE, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Bool(v1 <= v2);
                    Ok(())
                }
                (BinOp::GT, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Bool(v1 > v2);
                    Ok(())
                }
                (BinOp::GE, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Bool(v1 >= v2);
                    Ok(())
                }
                (BinOp::And, IExpr::Bool(v1), IExpr::Bool(v2)) => {
                    expr.item = IExpr::Bool(*v1 && *v2);
                    Ok(())
                }
                (BinOp::Or, IExpr::Bool(v1), IExpr::Bool(v2)) => {
                    expr.item = IExpr::Bool(*v1 || *v2);
                    Ok(())
                }
                (BinOp::EQ, IExpr::Bool(v1), IExpr::Bool(v2)) => {
                    expr.item = IExpr::Bool(v1 == v2);
                    Ok(())
                }
                (BinOp::EQ, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Bool(v1 == v2);
                    Ok(())
                }
                (BinOp::EQ, IExpr::String(v1), IExpr::String(v2)) => {
                    expr.item = IExpr::Bool(v1 == v2);
                    Ok(())
                }
                (BinOp::NE, IExpr::Bool(v1), IExpr::Bool(v2)) => {
                    expr.item = IExpr::Bool(v1 != v2);
                    Ok(())
                }
                (BinOp::NE, IExpr::Int(v1), IExpr::Int(v2)) => {
                    expr.item = IExpr::Bool(v1 != v2);
                    Ok(())
                }
                (BinOp::NE, IExpr::String(v1), IExpr::String(v2)) => {
                    expr.item = IExpr::Bool(v1 != v2);
                    Ok(())
                }
                _ => Ok(()),
            },
            IExpr::Var(_) => Ok(()),
            IExpr::Int(_) => Ok(()),
            IExpr::Bool(_) => Ok(()),
            IExpr::FunCall { args, .. } => {
                let mut res = vec![];
                for arg in args {
                    res.push(self.simplify_expression(arg));
                }
                res.acc()
            }
            IExpr::String(_) => Ok(()),
            IExpr::Paren(e) => self.simplify_expression(e),
        }
    }
}