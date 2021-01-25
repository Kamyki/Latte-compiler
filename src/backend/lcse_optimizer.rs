use crate::model::quadruple_code::{ControlFlowGraph, SimpleBlock, Reg, Value, BinOp, Instr, If, Label, UnOp, Instruction};
use std::collections::HashMap;
use crate::model::ast::IType;


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum Def {
    Bin(Value, BinOp, Value),
    Un(UnOp, Value),
}

impl From<Instr> for (Reg, Def) {
    fn from(i: Instr) -> Self {
        match i {
            Instr::Asg2(r, a, o, b) => (r, Def::Bin(a, o, b)),
            Instr::Asg1(r, o, a) => (r, Def::Un(o, a)),
            _ => panic!("cannot do it"),
        }
    }
}

impl From<(Reg, Def)> for Instr {
    fn from((r, d): (Reg, Def)) -> Self {
        match d {
            Def::Bin(a, o, b) => Instr::Asg2(r, a, o, b),
            Def::Un(o, v) => Instr::Asg1(r, o, v),
        }
    }
}

pub struct LCSEOptimizer {
    constants: HashMap<Reg, Value>,
    copies: HashMap<Reg, Reg>,

    definitions: HashMap<Def, Reg>,
    strings: HashMap<u32, String>,
    string_id: u32,

}

impl LCSEOptimizer {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            copies: HashMap::new(),
            definitions: HashMap::new(),
            strings: HashMap::new(),
            string_id: 0,
        }
    }

    fn get_copy_or(&self, a: Value, res: &mut bool) -> Value {
        if let Value::Register(reg1) = &a {
            if let Some(n) = self.copies.get(reg1) {
                *res = true;
                Value::Register(n.clone())
            } else {
                a
            }
        } else {
            a
        }
    }

    fn remove_copies(&mut self, r: &Reg) {
        self.copies.remove(r);

        let vals: Vec<(Reg, Reg)> = self.copies.clone().into_iter().collect();
        for v in vals.iter().filter_map(|(k, v)| if v == r {
            Some(k)
        } else {
            None
        }) {
            self.copies.remove(&v);
        }
    }

    fn get_const_or(&self, a: Value, res: &mut bool) -> Value {
        if let Value::Register(reg1) = &a {
            if let Some(n) = self.constants.get(reg1) {
                *res = true;
                n.clone()
            } else {
                a
            }
        } else {
            a
        }
    }

    fn update_defs(&mut self, r: &Reg) {
        let vals: Vec<(Def, Reg)> = self.definitions.clone().into_iter().collect();
        for k in vals.iter().filter_map(|(k, v)| match k {
            Def::Bin(Value::Register(a), _, _) if a == r => Some(k),
            Def::Bin(_, _, Value::Register(b)) if b == r => Some(k),
            Def::Un(_, Value::Register(v)) if v == r => Some(k),
            _ if v == r => Some(k),
            _ => None
        }) {
            self.definitions.remove(k);
        }
    }
}

impl LCSEOptimizer {
    pub fn optimize(&mut self, graph: &mut ControlFlowGraph) {
        self.string_id = graph.strings.len() as u32;
        self.strings.extend(graph.strings.drain());

        let keys: Vec<Label> = graph.functions.keys().cloned().collect();
        for f in keys {
            let mut res = true;
            while res {
                res = false;
                graph.compute_liveliness(&f);
                let blocks: Vec<(Label, SimpleBlock)> = graph.iter_fun(&f).map(|(l, b)| (l.clone(), b.clone())).collect();

                for (l, block) in blocks {
                    graph.blocks.remove(&l);
                    let mut new_block = block;
                    let (res1, new_block1) = self.dead_code_optimize(new_block);
                    let (res2, new_block2) = self.common_expr_optimize(new_block1);
                    let (res3, new_block3) = self.const_optimize(new_block2);
                    let (res4, new_block4) = self.copy_optimize(new_block3);
                    res = res || res1 || res2 || res3 || res4;
                    new_block = new_block4;
                    graph.blocks.insert(l, new_block);
                }
            }
        }
        graph.strings = self.strings.clone();
    }

    fn dead_code_optimize(&mut self, mut block: SimpleBlock) -> (bool, SimpleBlock) {
        let mut rest = false;
        let mut new_block = SimpleBlock::new();
        new_block.jumps = block.jumps;

        for Instruction(i, (ins, outs), (defs, used)) in block.code.drain(..) {
            if let Instr::Call(_, _, _) = i {
                new_block.code.push(Instruction(i, (ins, outs), (defs, used)))
            } else if defs.is_empty() || outs.intersection(&defs).next() != None {
                new_block.code.push(Instruction(i, (ins, outs), (defs, used)))
            } else {
                rest = true;
            }
        }
        (rest, new_block)
    }

    fn common_expr_optimize(&mut self, mut block: SimpleBlock) -> (bool, SimpleBlock) {
        let mut rest = false;
        let mut new_block = SimpleBlock::new();
        new_block.jumps = block.jumps;
        self.definitions.clear();

        for old_i in block.code.drain(..) {
            match &old_i.0 {
                Instr::Asg2(_, _, _, _) |
                Instr::Asg1(_, _, _) => {
                    let (r, def): (Reg, Def) = old_i.0.clone().into();
                    if let Some(new_r) = self.definitions.get(&def).cloned() {
                        rest = true;
                        self.update_defs(&r);
                        new_block.code.push(Instr::Copy(r, Value::Register(new_r)).into())
                    } else {
                        self.update_defs(&r);
                        self.definitions.insert(def, r);
                        new_block.code.push(old_i)
                    }

                }
                Instr::Copy(r, _) |
                Instr::Call(r, _, _) => {
                    self.update_defs(r);
                    new_block.code.push(old_i);
                }
                _ => new_block.code.push(old_i),
            }
        }
        (rest, new_block)
    }

    fn copy_optimize(&mut self, mut block: SimpleBlock) -> (bool, SimpleBlock) {
        //    return (false, block);
        let mut rest = false;
        let mut new_block = SimpleBlock::new();
        new_block.jumps = block.jumps;
        self.copies.clear();
        for old_i in block.code.drain(..) {
            match old_i.0 {
                Instr::Asg2(r, a, o, b) => {
                    let new_a = self.get_copy_or(a, &mut rest);
                    let new_b = self.get_copy_or(b, &mut rest);

                    self.remove_copies(&r);

                    new_block.code.push(Instr::Asg2(r, new_a, o, new_b).into());
                }
                Instr::Asg1(r, o, v) => {
                    let new_v = self.get_copy_or(v, &mut rest);
                    self.remove_copies(&r);

                    new_block.code.push(Instr::Asg1(r, o, new_v).into());
                }
                Instr::Copy(r, Value::Register(reg)) => {
                    let new_reg = if let Some(n) = self.copies.get(&reg) {
                        rest = true;
                        n.clone()
                    } else {
                        reg
                    };
                    self.remove_copies(&r);
                    self.copies.insert(r.clone(), new_reg.clone());
                    new_block.code.push(Instr::Copy(r, Value::Register(new_reg)).into())
                }
                Instr::If(If(a, o, b, t, f)) => {
                    let new_a = self.get_copy_or(a, &mut rest);
                    let new_b = self.get_copy_or(b, &mut rest);
                    new_block.code.push(Instr::If(If(new_a, o, new_b, t, f)).into())
                }
                Instr::Call(r, l, mut args) => {
                    let mut new_args = vec![];
                    args.drain(..).for_each(|arg| {
                        let new_arg = self.get_copy_or(arg, &mut rest);
                        new_args.push(new_arg);
                    });
                    self.remove_copies(&r);
                    new_block.code.push(Instr::Call(r, l, new_args).into())
                }
                Instr::Return(v) => {
                    let new_v = self.get_copy_or(v, &mut rest);
                    new_block.code.push(Instr::Return(new_v).into())
                }
                _ => new_block.code.push(old_i),
            }
        }
        (rest, new_block)
    }

    fn const_optimize(&mut self, mut block: SimpleBlock) -> (bool, SimpleBlock) {
        let mut rest = false;
        let mut new_block = SimpleBlock::new();
        self.constants.clear();
        for old_i in block.code.drain(..) {
            match old_i.0 {
                Instr::Asg2(r, a, o, b) => {
                    let new_a = self.get_const_or(a, &mut rest);
                    let new_b = self.get_const_or(b, &mut rest);
                    self.constants.remove(&r);

                    match o.perform(&new_a, &new_b) {
                        None if r.itype == IType::String => {
                            if let (Value::String(s1), Value::String(s2)) = (&new_a, &new_b) {
                                let v = Value::String(self.string_id);
                                let new_string = format!("{}{}", self.strings[s1], self.strings[s2]);
                                self.strings.insert(self.string_id, new_string);
                                self.string_id += 1;
                                self.constants.insert(r.clone(), v.clone());
                                new_block.code.push(Instr::Copy(r.clone(), v.clone()).into());
                                rest = true;
                            } else {
                                new_block.code.push(Instr::Asg2(r, new_a, o, new_b).into());
                            }
                        }
                        None => {
                            new_block.code.push(Instr::Asg2(r, new_a, o, new_b).into());
                        }
                        Some(i) => {
                            let v = Value::Int(i);
                            self.constants.insert(r.clone(), v.clone());
                            new_block.code.push(Instr::Copy(r, v).into());
                            rest = true;
                        }
                    }
                }
                Instr::Asg1(r, o, a) => {
                    let new_a = self.get_const_or(a, &mut rest);
                    self.constants.remove(&r);
                    match o.perform(&new_a) {
                        None if r.itype == IType::Boolean => {
                            if let Value::Bool(bo) = &new_a {
                                new_block.code.push(Instr::Copy(r, Value::Bool(!*bo)).into())
                            } else {
                                new_block.code.push(Instr::Asg1(r, o, new_a).into())
                            }
                        }
                        None => new_block.code.push(Instr::Asg1(r, o.clone(), new_a).into()),
                        Some(i) => {
                            let v = Value::Int(i);
                            self.constants.insert(r.clone(), v.clone());
                            new_block.code.push(Instr::Copy(r.clone(), v.clone()).into());
                            rest = true;
                        }
                    }
                }
                Instr::Copy(r, a) => {
                    let new_a = self.get_const_or(a, &mut rest);
                    if let Value::String(_) | Value::Int(_) | Value::Bool(_) = new_a {
                        self.constants.insert(r.clone(), new_a.clone());
                    }
                    new_block.code.push(Instr::Copy(r.clone(), new_a).into());
                }
                Instr::Jump(l) => {
                    new_block.jumps.insert(l.clone());
                    new_block.code.push(Instr::Jump(l).into());
                }
                Instr::If(If(a, o, b, t, f)) => {
                    let new_a = self.get_const_or(a, &mut rest);
                    let new_b = self.get_const_or(b, &mut rest);
                    match o.perform(&new_a, &new_b) {
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
                        None => {
                            new_block.jumps.insert(t.clone());
                            new_block.jumps.insert(f.clone());
                            new_block.code.push(Instr::If(If(new_a, o, new_b, t, f)).into());
                        }
                    }
                }
                Instr::Call(r, f, mut args) => {
                    let mut new_args = vec![];
                    args.drain(..).for_each(|arg| {
                        let new_arg = self.get_const_or(arg, &mut rest);
                        new_args.push(new_arg);
                    });
                    self.constants.remove(&r);
                    new_block.code.push(Instr::Call(r, f, new_args).into());
                }
                Instr::Return(a) => {
                    let new_a = self.get_const_or(a, &mut rest);
                    new_block.code.push(Instr::Return(new_a).into());
                }
                Instr::VReturn => {
                    new_block.code.push(old_i.clone());
                }
                Instr::Extract(_, _, _) => todo!(),
                Instr::Insert(_, _, _) => todo!(),
                Instr::Cast(_, _) => todo!()
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
    fn test_expr() {
        let mut cfg = ControlFlowGraph::new();
        let r1 = Reg::new(IType::Int, "r1".to_string());
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
        cfg.functions.insert("f1".to_string(), ("l1".to_string(), vec![r1.clone()]));

        cfg.begin_block("l1".to_string());
        cfg.push(Instr::Copy(a1.clone(), Value::Register(r1.clone())));
        cfg.push(Instr::Copy(a2.clone(), Value::Register(a1.clone())));
        cfg.push(Instr::Asg2(a3.clone(), Value::Register(a1.clone()), BinOp::Add, Value::Register(a2.clone())));
        cfg.push(Instr::Copy(a4.clone(), Value::Register(a2.clone())));
        cfg.push(Instr::Asg2(a5.clone(), Value::Register(a2.clone()), BinOp::Add, Value::Register(a4.clone())));


        cfg.push(Instr::Asg2(x1.clone(), Value::Register(a1.clone()), BinOp::Add, Value::Register(a2.clone())));
        cfg.push(Instr::Asg2(x2.clone(), Value::Register(x1.clone()), BinOp::Add, Value::Register(a3.clone())));
        cfg.push(Instr::Asg2(x3.clone(), Value::Register(x2.clone()), BinOp::Add, Value::Register(a4.clone())));
        cfg.push(Instr::Asg2(x4.clone(), Value::Register(x3.clone()), BinOp::Add, Value::Register(a5.clone())));

        cfg.push(Instr::Call(v.clone(), "printInt".to_string(), vec![Value::Register(x4.clone())]));
        cfg.push(Instr::Return(Value::Int(0)));
        cfg.close_block();

        let mut optimizer = LCSEOptimizer::new();
        optimizer.optimize(&mut cfg);

        assert_eq!(cfg.blocks["l1"].code.len(), 6);
        assert_eq!(cfg.blocks["l1"].code[0].0, Instr::Asg2(a3.clone(), Value::Register(r1.clone()), BinOp::Add, Value::Register(r1.clone())));
        assert_eq!(cfg.blocks["l1"].code[3].0, Instr::Asg2(x4.clone(), Value::Register(x3.clone()), BinOp::Add, Value::Register(a3.clone())));
        assert_eq!(cfg.blocks["l1"].code[4].0, Instr::Call(v.clone(), "printInt".to_string(), vec![Value::Register(x4.clone())]));
    }

    #[test]
    fn test_copy() {
        let mut cfg = ControlFlowGraph::new();
        let r1 = Reg::new(IType::Int, "r1".to_string());
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
        cfg.functions.insert("f1".to_string(), ("l1".to_string(), vec![r1.clone()]));

        cfg.begin_block("l1".to_string());
        cfg.push(Instr::Copy(a1.clone(), Value::Register(r1.clone())));
        cfg.push(Instr::Copy(a2.clone(), Value::Register(a1.clone())));
        cfg.push(Instr::Copy(a3.clone(), Value::Register(a2.clone())));
        cfg.push(Instr::Copy(a4.clone(), Value::Register(a3.clone())));
        cfg.push(Instr::Copy(a5.clone(), Value::Register(a4.clone())));

        cfg.push(Instr::Asg2(x1.clone(), Value::Register(a1.clone()), BinOp::Add, Value::Register(a2.clone())));
        cfg.push(Instr::Asg2(x2.clone(), Value::Register(x1.clone()), BinOp::Add, Value::Register(a3.clone())));
        cfg.push(Instr::Asg2(x3.clone(), Value::Register(x2.clone()), BinOp::Add, Value::Register(a4.clone())));
        cfg.push(Instr::Asg2(x4.clone(), Value::Register(x3.clone()), BinOp::Add, Value::Register(a5.clone())));

        cfg.push(Instr::Call(v.clone(), "printInt".to_string(), vec![Value::Register(x4.clone())]));
        cfg.push(Instr::Return(Value::Int(0)));
        cfg.close_block();

        let mut optimizer = LCSEOptimizer::new();
        optimizer.optimize(&mut cfg);

        assert_eq!(cfg.blocks["l1"].code.len(), 6);
        assert_eq!(cfg.blocks["l1"].code[3].0, Instr::Asg2(x4.clone(), Value::Register(x3.clone()), BinOp::Add, Value::Register(r1.clone())));
        assert_eq!(cfg.blocks["l1"].code[4].0, Instr::Call(v.clone(), "printInt".to_string(), vec![Value::Register(x4.clone())]));
    }

    #[test]
    fn test_add() {
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
        cfg.functions.insert("f1".to_string(), ("l1".to_string(), vec![]));

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
        cfg.functions.insert("f1".to_string(), ("l1".to_string(), vec![]));

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