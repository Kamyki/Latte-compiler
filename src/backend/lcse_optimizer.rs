use crate::model::quadruple_code::{ControlFlowGraph, SimpleBlock, Reg, Value, BinOp, Instr, If, Label, UnOp};
use std::collections::HashMap;
use crate::model::ast::IType;


#[derive(Debug, Clone, Eq, PartialEq)]
enum Def {
    Bin(Value, BinOp, Value),
    Un(UnOp, Value),
    Copy(Value),
}

impl From<Instr> for Option<(Reg, Def)> {
    fn from(i: Instr) -> Self {
        match i {
            Instr::Asg2(r, a, o, b) => Some((r, Def::Bin(a, o, b))),
            Instr::Asg1(r,o, a) => Some((r, Def::Un(o, a))),
            Instr::Copy(r, v) => Some((r, Def::Copy(v))),
            _ => None,
        }
    }
}

pub struct LCSEOptimizer {
    constants: HashMap<Reg, Value>,
    definitions: HashMap<Def, Reg>,

}

impl LCSEOptimizer {
    pub fn new() -> Self {
        Self { constants: HashMap::new(), definitions: HashMap::new() }
    }
}

impl LCSEOptimizer {
    pub fn optimize(&mut self, graph: &mut ControlFlowGraph) {
        let blocks: Vec<(Label, SimpleBlock)> = graph.blocks.drain().collect();
        for (l, block) in blocks {
            let new_block = self.optimize_block(&block);
            graph.blocks.insert(l, new_block);
        }
    }

    fn optimize_block(&mut self, block: &SimpleBlock) -> SimpleBlock {
        let (mut res, mut new_block) = self.const_optimize(block);
        while res {
            let (res1, new_block1) = self.const_optimize(&new_block);
            res = res1;
            new_block = new_block1;
        }
        new_block
    }

    fn const_optimize(&mut self, block: &SimpleBlock) -> (bool, SimpleBlock) {
        let mut rest = false;
        let mut new_block = SimpleBlock::new();

        for old_i in block.code.iter() {
            match &old_i.0 {
                Instr::Asg2(_, _, BinOp::Concat, _) => {
                    new_block.code.push(old_i.clone());
                    todo!();
                }
                Instr::Asg2(r, a, o, b) => {
                    let new_a = if let Value::Register(reg1) = a {
                        self.constants.get(reg1).unwrap_or(a)
                    } else {
                        a
                    };
                    let new_b = if let Value::Register(reg2) = b {
                        self.constants.get(reg2).unwrap_or(b)
                    } else {
                        b
                    };
                    match o.perform(new_a, new_b) {
                        None if r.itype == IType::String => {
                            todo!();
                        }
                        None => {
                            new_block.code.push(old_i.clone());
                        }
                        Some(i) => {
                            let v = Value::Int(i);
                            self.constants.insert(r.clone(), v.clone());
                            new_block.code.push(Instr::Copy(r.clone(), v.clone()).into());
                            rest = true;
                        }
                    }
                }
                Instr::Asg1(r, o, a) => {
                    let new_a = if let Value::Register(reg1) = a {
                        self.constants.get(reg1).unwrap_or(a)
                    } else {
                        a
                    };
                    match o.perform(new_a) {
                        None => new_block.code.push(old_i.clone()),
                        Some(i) => {
                            let v = Value::Int(i);
                            self.constants.insert(r.clone(), v.clone());
                            new_block.code.push(Instr::Copy(r.clone(), v.clone()).into());
                            rest = true;
                        }
                    }
                }
                Instr::Copy(_, Value::Register(_)) => {
                    new_block.code.push(old_i.clone());
                }
                Instr::Copy(r, a) => {
                    let new_a = if let Value::Register(reg1) = a {
                        self.constants.get(reg1).unwrap_or(a).clone()
                    } else {
                        a.clone()
                    };
                    match new_a {
                        Value::Register(_) => new_block.code.push(old_i.clone()),
                        _ => {
                            self.constants.insert(r.clone(), new_a);
                            rest = true;
                        }
                    }
                }
                Instr::Jump(l) => {
                    new_block.code.push(old_i.clone());
                    new_block.jumps.insert(l.clone());
                }
                Instr::If(If(a, o, b, t, f)) => {
                    let new_a = if let Value::Register(reg1) = a {
                        self.constants.get(reg1).unwrap_or(a)
                    } else {
                        a
                    };
                    let new_b = if let Value::Register(reg2) = b {
                        self.constants.get(reg2).unwrap_or(b)
                    } else {
                        b
                    };
                    match o.perform(new_a, new_b) {
                        Some(b) if b => {
                            new_block.code.push(Instr::Jump(t.clone()).into());
                            new_block.jumps.insert(t.clone());
                            rest = true;
                        }
                        Some(_) => {
                            new_block.code.push(Instr::Jump(f.clone()).into());
                            new_block.jumps.insert(f.clone());
                            rest = true;
                        }
                        None => if new_a == a && new_b == b {
                            new_block.code.push(old_i.clone());
                            new_block.jumps.insert(t.clone());
                            new_block.jumps.insert(f.clone());
                        } else {
                            new_block.code.push(Instr::If(If(new_a.clone(), o.clone(), new_b.clone(), t.clone(), f.clone())).into());
                            new_block.jumps.insert(t.clone());
                            new_block.jumps.insert(f.clone());
                            rest = true;
                        }
                    }
                }
                Instr::Call(r, f, args) => {
                    let mut new_args = vec![];
                    args.iter().for_each(|arg| match arg {
                        Value::Register(reg) => {
                            if let Some(v) = self.constants.get(reg) {
                                new_args.push(v.clone());
                                rest = rest || true;
                            } else {
                                new_args.push(arg.clone());
                            }
                        }
                        _ => new_args.push(arg.clone()),
                    });
                    new_block.code.push(Instr::Call(r.clone(), f.clone(), new_args).into());
                }
                Instr::Return(Value::Register(reg)) => {
                    if let Some(v) = self.constants.get(reg) {
                        new_block.code.push(Instr::Return(v.clone()).into());
                        rest = rest || true;
                    }
                }
                Instr::Return(_) => {
                    new_block.code.push(old_i.clone());
                }
                Instr::VReturn => {
                    new_block.code.push(old_i.clone());
                }
            }
        }
        (rest, new_block)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::ast::IType;

    #[test]
    fn test_lcse() {
        let mut cfg = ControlFlowGraph::new();
        let a1 = Reg::new(IType::Int, "a1".to_string());
        let a2 = Reg::new(IType::Int, "a2".to_string());
        let a3 = Reg::new(IType::Int, "a3".to_string());
        let a4 = Reg::new(IType::Int, "a4".to_string());
        let a5 = Reg::new(IType::Int, "a5".to_string());
        let x1 = Reg::new(IType::Int, "x1".to_string());
        let x2 = Reg::new(IType::Int, "x2".to_string());
        let x3 = Reg::new(IType::Int, "x3".to_string());
        let x4 = Reg::new(IType::Int, "x4".to_string());
        let v = Reg::new(IType::Int, "v".to_string());

        cfg.begin_block("l1".to_string());
        cfg.push(Instr::Copy(a1.clone(), Value::Int(1)));
        cfg.push(Instr::Copy(a2.clone(), Value::Int(1)));
        cfg.push(Instr::Copy(a3.clone(), Value::Int(1)));
        cfg.push(Instr::Copy(a4.clone(), Value::Int(1)));
        cfg.push(Instr::Copy(a5.clone(), Value::Int(1)));

        cfg.push(Instr::Asg2(x1.clone(), Value::Register(a1.clone()), BinOp::Add, Value::Register(a2.clone())));
        cfg.push(Instr::Asg2(x2.clone(), Value::Register(x1.clone()), BinOp::Add, Value::Register(a3.clone())));
        cfg.push(Instr::Asg2(x3.clone(), Value::Register(x2.clone()), BinOp::Add, Value::Register(a4.clone())));
        cfg.push(Instr::Asg2(x4.clone(), Value::Register(x3.clone()), BinOp::Add, Value::Register(a5.clone())));

        cfg.push(Instr::Call(v.clone(), "printInt".to_string(), vec![Value::Register(x4.clone())]));
        cfg.push(Instr::Return(Value::Int(0)));
        cfg.close_block();

        let mut optimizer = LCSEOptimizer::new();
        optimizer.optimize(&mut cfg);

        assert_eq!(cfg.blocks["l1"].code.len(), 2);
    }
}