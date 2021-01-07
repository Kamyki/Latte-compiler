use std::collections::HashMap;
use crate::model::ast::*;
use crate::error_handling::CheckerResult;

pub struct StringMapper {
    pub strings: HashMap<String, u32>,

}

impl StringMapper {
    pub fn new() -> Self {
        Self {
            strings: HashMap::new()
        }
    }

    fn insert_string(&mut self, str: String) {
        if !self.strings.contains_key(str.as_str()) {
            self.strings.insert(str, self.strings.len() as u32);
        }
    }
}

impl StringMapper {
    pub fn add_strings(&mut self, ast: &Program) -> CheckerResult<()> {
        ast.defs.iter()
            .for_each(|top_def| match top_def {
                TopDef::Function(Function { block, .. }) => self.add_block(block),
                TopDef::Class(Class { methods, .. }) => {
                    methods.iter().for_each(|Function{block, ..}| self.add_block(block))
                }
            });
        Ok(())
    }

    fn add_block(&mut self, block: &Block) {
        block.stmts.iter().for_each(|stmt| match &stmt.item {
            IStmt::Empty => {},
            IStmt::Block(b) => self.add_block(b),
            IStmt::Decl {items, .. } => items.iter().for_each(|i| match i {
                Item::NoInit(_) => {}
                Item::Init { e, .. } => self.add_expr(e)
            }),
            IStmt::Asg { e, .. } => self.add_expr(e),
            IStmt::Incr(_) => {}
            IStmt::Decr(_) => {}
            IStmt::Ret(e) => self.add_expr(e),
            IStmt::VRet => {}
            IStmt::Cond { c, if_true } => {
                self.add_expr(c);
                self.add_block(if_true);
            }
            IStmt::CondElse { c, if_true, if_false } => {
                self.add_expr(c);
                self.add_block(if_true);
                self.add_block(if_false);
            }
            IStmt::While { c, while_true } => {
                self.add_expr(c);
                self.add_block(while_true)
            }
            IStmt::Expr(e) => self.add_expr(e),
        })
    }

    fn add_expr(&mut self, expr: &Expr) {
        match &expr.item {
            IExpr::Unary { .. } => {}
            IExpr::Binary { l, r, .. } => {
                self.add_expr(l);
                self.add_expr(r);
            }
            IExpr::Var(_) => {}
            IExpr::Int(_) => {}
            IExpr::Bool(_) => {}
            IExpr::Null => {}
            IExpr::FunCall {args, .. } => args.iter().for_each(|a| self.add_expr(a)),
            IExpr::String(s) => {
                self.insert_string(s.clone())
            }
            IExpr::Paren(e) => self.add_expr(e),
            IExpr::Field(_) => {}
            IExpr::Object(_) => {}
            IExpr::Cast { .. } => {}
        }
    }
}