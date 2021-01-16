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
            Instr::Asg1(r, o, a) => Some((r, Def::Un(o, a))),
            Instr::Copy(r, v) => Some((r, Def::Copy(v))),
            _ => None,
        }
    }
}

pub struct LCSEOptimizer {
    constants: HashMap<Reg, Value>,
    _copies: HashMap<Reg, Reg>,

    _definitions: HashMap<Def, Reg>,
    strings: HashMap<u32, String>,
    string_id: u32,

}

impl LCSEOptimizer {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            _copies: HashMap::new(),
            _definitions: HashMap::new(),
            strings: HashMap::new(),
            string_id: 0,
        }
    }
}

impl LCSEOptimizer {
    pub fn optimize(&mut self, graph: &mut ControlFlowGraph) {
        self.string_id = graph.strings.len() as u32;
        self.strings.extend(graph.strings.drain());
        let blocks: Vec<(Label, SimpleBlock)> = graph.blocks.drain().collect();
        for (l, block) in blocks {
            let new_block = self.optimize_block(block);
            graph.blocks.insert(l, new_block);
        }
    }

    fn optimize_block(&mut self, block: SimpleBlock) -> SimpleBlock {
        let mut v = (true, block);
        let mut v1;
        while v.0 {
            v1 = self.dead_code_optimize(v.1);
            v = (v1.0, v1.1);
            v1 = self.common_expr_optimize(v.1);
            v = (v.0 | v1.0, v1.1);
            v1 = self.const_optimize(v.1);
            v = (v.0 | v1.0, v1.1);
            v1 = self.copy_optimize(v.1);
            v = (v.0 | v1.0, v1.1);
        }
        v.1
    }

    fn dead_code_optimize(&mut self, block: SimpleBlock) -> (bool, SimpleBlock) {
        (false, block)
    }

    fn common_expr_optimize(&mut self, block: SimpleBlock) -> (bool, SimpleBlock) {
        (false, block)
    }

    fn copy_optimize(&mut self, block: SimpleBlock) -> (bool, SimpleBlock) {
        // let mut rest = false;
        // let mut new_block = SimpleBlock::new();
        // self.copies.clear();
        // for old_i in block.code {
        //     match &old_i.0 {
        //         Instr::Asg2(_, _, _, _) => {}
        //         Instr::Asg1(_, _, _) => {}
        //         Instr::Copy(r, Value::Register(reg)) => {
        //             let new_reg = if let Some(n) = self.copies.get(reg) {
        //                 n.clone()
        //             } else {
        //                 reg.clone()
        //             };
        //             self.copies.insert(r.clone(), new_reg);
        //             new_block.code.push(old_i.clone())
        //         }
        //         Instr::Jump(_) => {}
        //         Instr::If(_) => {}
        //         Instr::Call(_, _, _) => {}
        //         Instr::Return(_) => {}
        //         Instr::VReturn => {}
        //     }
        // }
        (false, block)
    }


    fn const_optimize(&mut self, block: SimpleBlock) -> (bool, SimpleBlock) {
        let mut rest = false;
        let mut new_block = SimpleBlock::new();
        self.constants.clear();
        for old_i in block.code.iter() {
            match &old_i.0 {
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
                            if let (Value::String(s1), Value::String(s2)) = (new_a, new_b) {
                                let v = Value::String(self.string_id);
                                let new_string = format!("{}{}", self.strings[s1], self.strings[s2]);
                                self.strings.insert(self.string_id, new_string);
                                self.string_id += 1;
                                self.constants.insert(r.clone(), v.clone());
                                new_block.code.push(Instr::Copy(r.clone(), v.clone()).into());
                                rest = true;
                            } else {
                                new_block.code.push(old_i.clone());
                            }
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
        assert_eq!(cfg.blocks["l1"].code[0].0, Instr::Call(v.clone(), "printInt".to_string(), vec![Value::Int(5)]));
    }

    #[test]
    fn test_string() {
        let mut cfg = ControlFlowGraph::new();
        let a1 = Reg::new(IType::String, "a1".to_string());
        let a2 = Reg::new(IType::String, "a2".to_string());
        let x1 = Reg::new(IType::String, "x1".to_string());
        let v = Reg::new(IType::Int, "v".to_string());
        cfg.strings.extend(vec![(0, "kadabra".to_string()), (1, "abba".to_string())]);

        cfg.begin_block("l1".to_string());
        cfg.push(Instr::Copy(a1.clone(), Value::String(0)));
        cfg.push(Instr::Copy(a2.clone(), Value::String(1)));

        cfg.push(Instr::Asg2(x1.clone(), Value::Register(a1.clone()), BinOp::Concat, Value::Register(a2.clone())));

        cfg.push(Instr::Call(v.clone(), "printString".to_string(), vec![Value::Register(x1.clone())]));
        cfg.push(Instr::Return(Value::Int(0)));
        cfg.close_block();

        let mut optimizer = LCSEOptimizer::new();
        optimizer.optimize(&mut cfg);

        assert_eq!(cfg.blocks["l1"].code.len(), 2);
        assert_eq!(cfg.blocks["l1"].code[0].0, Instr::Call(v.clone(), "printString".to_string(), vec![Value::String(2)]));
    }
}