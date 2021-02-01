use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::FromIterator;

use crate::model::ast::{IType, IBinOp};
use std::hash::Hash;

#[derive(Debug)]
pub struct ControlFlowGraph {
    pub blocks: HashMap<Label, SimpleBlock>,
    pub liveness: HashMap<Label, (HashSet<Label>, HashSet<Label>)>,
    pub functions: HashMap<String, (Label, Vec<Reg>)>,
    pub classes: HashMap<String, (Vec<String>, Vec<Reg>)>,
    pub builtin: HashMap<String, (Label, u32)>,
    pub strings: HashMap<u32, String>,

    pub current_block: Vec<(Label, SimpleBlock)>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            blocks: HashMap::new(),
            functions: HashMap::new(),
            current_block: Vec::new(),
            builtin: HashMap::new(),
            strings: HashMap::new(),
            liveness: HashMap::new(),
            classes: HashMap::new(),
        }
    }

    pub fn begin_block(&mut self, label: Label) {
        self.current_block.push((label, SimpleBlock::new()));
    }

    pub fn extend(&mut self, code: Vec<Instr>) {
        if let Some((_, b)) = self.current_block.last_mut() {
            b.code.extend(code.into_iter().map(|i| i.into()));
        }
    }

    pub fn push(&mut self, code: Instr) {
        if let Some((_, b)) = self.current_block.last_mut() {
            if let Instr::Jump(l) = &code {
                b.jumps.insert(l.clone());
            }
            if let Instr::If(If(_, _, _, t, f)) = &code {
                b.jumps.insert(t.clone());
                b.jumps.insert(f.clone());
            }
            b.code.push(code.into());
        }
    }

    pub fn jump(&mut self, label: Label) {
        if let Some((_, b)) = self.current_block.last_mut() {
            b.jumps.insert(label);
        }
    }

    pub fn close_block(&mut self) {
        if let Some((l, b)) = self.current_block.pop() {
            self.blocks.insert(l.clone(), b);
        }
    }

    pub fn locals(&self, function: &String) -> Vec<Reg> {
        let mut reg = HashSet::new();
        for (_, block) in self.iter_fun(function) {
            reg.extend(block.code.iter().filter_map(|Instruction(i, _, _)| match i {
                Instr::Asg2(r, _, _, _) => Some(r),
                Instr::Asg1(r, _, _) => Some(r),
                Instr::Copy(r, _) => Some(r),
                Instr::Jump(_) => None,
                Instr::If(_) => None,
                Instr::Call(r, _, _) => Some(r),
                Instr::Return(_) => None,
                Instr::VReturn => None,
                Instr::Extract(r, _, _) => Some(r),
                Instr::Insert(_, _, _) => None,
                Instr::Cast(r, _) => Some(r),
                Instr::CallM(r, _, _, _) => Some(r),
            }).cloned());
        }
        reg.into_iter().collect()
    }

    pub fn compute_liveliness(&mut self, function: &str) {
        if let None = self.liveness.get(&self.functions[function].0) {
            let labels: Vec<Label> = self.iter_fun(function).map(|(k, _)| k.clone()).collect();
            let mut changed;
            loop {
                changed = false;
                for label in &labels {
                    let block = self.blocks.get_mut(label).unwrap();
                    let prev_in = block.ins();
                    let prev_out = block.outs();
                    block.compute_block_liveliness();
                    changed = changed || self.update_iteration(label, &prev_in, &prev_out);
                }
                if !changed {
                    break;
                }
            }
        }
    }

    fn update_iteration(&mut self, label: &Label, prev_in: &HashSet<Reg>, prev_out: &HashSet<Reg>) -> bool {
        let mut all_ins = vec![];

        for j in self.blocks.get(label).unwrap().jumps.iter() {
            all_ins.push(self.blocks.get(j).unwrap().ins())
        }
        let block = self.blocks.get_mut(label).unwrap();
        for i in all_ins {
            block.code.last_mut().map_or(&mut HashSet::new(), |x| &mut x.1.1).extend(i);
        }
        *prev_in != block.ins() || *prev_out != block.outs()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_analysis() {
        let mut cfg = ControlFlowGraph::new();
        cfg.functions.insert("f1".to_string(), ("l0".to_string(), Vec::new()));

        cfg.begin_block("l0".to_string());
        cfg.push(Instr::Copy(Reg::new(IType::Int, "v0".to_string()), Value::Int(1)));
        cfg.push(Instr::Jump("l1".to_string()));
        cfg.close_block();

        cfg.begin_block("l1".to_string());
        cfg.push(Instr::Asg2(Reg::new(IType::Int, "v1".to_string()), Value::Int(1), BinOp::Add, Value::Register(Reg::new(IType::Int, "v0".to_string()))));
        cfg.push(Instr::Asg2(Reg::new(IType::Int, "v2".to_string()), Value::Int(1), BinOp::Add, Value::Register(Reg::new(IType::Int, "v1".to_string()))));
        cfg.push(Instr::Asg2(Reg::new(IType::Int, "v3".to_string()), Value::Int(1), BinOp::Add, Value::Register(Reg::new(IType::Int, "v2".to_string()))));
        cfg.push(Instr::Jump("l2".to_string()));
        cfg.close_block();

        cfg.begin_block("l2".to_string());
        cfg.push(Instr::Asg2(Reg::new(IType::Int, "v0".to_string()), Value::Int(2), BinOp::Mul, Value::Register(Reg::new(IType::Int, "v3".to_string()))));
        cfg.push(Instr::If(If(Value::Int(2), RelOp::EQ, Value::Register(Reg::new(IType::Int, "v3".to_string())), "l3".to_string(), "l1".to_string())));
        cfg.close_block();

        cfg.begin_block("l3".to_string());
        cfg.push(Instr::Return(Value::Register(Reg::new(IType::Int, "v3".to_string()))));
        cfg.close_block();

        cfg.compute_liveliness("f1");


        assert!(cfg.blocks["l0"].outs().contains(&Reg::new(IType::Int, "v0".to_string())));

        assert!(cfg.blocks["l1"].ins().contains(&Reg::new(IType::Int, "v0".to_string())));
        assert!(cfg.blocks["l1"].outs().contains(&Reg::new(IType::Int, "v3".to_string())));

        assert!(cfg.blocks["l2"].ins().contains(&Reg::new(IType::Int, "v3".to_string())));
        assert!(cfg.blocks["l2"].outs().contains(&Reg::new(IType::Int, "v0".to_string())));
        assert!(cfg.blocks["l2"].outs().contains(&Reg::new(IType::Int, "v3".to_string())));

        assert!(cfg.blocks["l3"].ins().contains(&Reg::new(IType::Int, "v3".to_string())));
        assert!(cfg.blocks["l3"].outs().is_empty());
    }
}

pub struct Iter<'a> {
    visited: HashSet<Label>,
    to_visit: VecDeque<Label>,
    blocks: &'a HashMap<Label, SimpleBlock>,
    starts: Vec<Label>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = (&'a Label, &'a SimpleBlock);

    fn next(&mut self) -> Option<Self::Item> {
        if self.to_visit.is_empty() {
            if let Some(f) = self.starts.pop() {
                self.to_visit.push_back(f);
            }
        }

        while let Some(l) = self.to_visit.pop_front() {
            if !self.visited.contains(l.as_str()) {
                let (label, block) = self.blocks.get_key_value(l.as_str()).unwrap();
                self.to_visit.extend(block.jumps.clone());
                self.visited.insert(l.clone());
                return Some((label, block));
            } else {
                return self.next();
            }
        }
        None
    }
}

impl<'a> ControlFlowGraph {
    pub fn iter(&'a self) -> Iter<'a> {
        let functions = self.functions.values().map(|f| f.0.clone());

        Iter { visited: HashSet::new(), to_visit: VecDeque::new(), starts: Vec::from_iter(functions), blocks: &self.blocks }
    }

    pub fn iter_fun(&'a self, fun: &str) -> Iter<'a> {
        let function = self.functions[fun].0.clone();
        Iter { visited: HashSet::new(), to_visit: VecDeque::new(), starts: vec![function], blocks: &self.blocks }
    }
}

#[derive(Debug, Clone)]
pub struct Instruction(pub Instr, pub (HashSet<Reg>, HashSet<Reg>), pub (HashSet<Reg>, HashSet<Reg>));

impl From<Instr> for Instruction {
    fn from(i: Instr) -> Self {
        let mut defs = HashSet::new();
        let mut vals = HashSet::new();
        match &i {
            Instr::Asg2(r, a, _, b) => {
                defs.insert(r.clone());
                vals.insert(a.clone());
                vals.insert(b.clone());
            }
            Instr::Asg1(r, _, v) => {
                defs.insert(r.clone());
                vals.insert(v.clone());
            }
            Instr::Copy(r, v) => {
                defs.insert(r.clone());
                vals.insert(v.clone());
            }
            Instr::Jump(_) => {}
            Instr::If(If(a, _, b, _, _)) => {
                vals.insert(a.clone());
                vals.insert(b.clone());
            }
            Instr::Call(r, _, vs) => {
                defs.insert(r.clone());
                vals.extend(vs.iter().cloned());
            }
            Instr::Return(v) => {
                vals.insert(v.clone());
            }
            Instr::VReturn => {}
            Instr::Extract(r, obj, _) => {
                defs.insert(r.clone());
                vals.insert(obj.clone());
            }
            Instr::Insert(obj, _, v) => {
                vals.insert(obj.clone());
                vals.insert(v.clone());
            }
            Instr::Cast(r, v) => {
                vals.insert(v.clone());
                defs.insert(r.clone());
            }
            Instr::CallM(r, _, _, vs) => {
                defs.insert(r.clone());
                vals.extend(vs.iter().cloned());
            }
        }
        let used: HashSet<Reg> = vals.into_iter().filter_map(|v| if let Value::Register(r) = v {
            Some(r)
        } else {
            None
        }).collect();
        Instruction(i, (HashSet::new(), HashSet::new()), (defs, used))
    }
}

#[derive(Debug, Clone)]
pub struct SimpleBlock {
    pub code: Vec<Instruction>,
    pub jumps: HashSet<Label>,
}

impl SimpleBlock {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            jumps: HashSet::new(),
        }
    }
}

//liveliness
impl SimpleBlock {
    pub fn ins(&self) -> HashSet<Reg> {
        self.code.first().map_or(HashSet::new(), |x| x.1.0.clone())
    }

    pub fn outs(&self) -> HashSet<Reg> {
        self.code.last().map_or(HashSet::new(), |x| x.1.1.clone())
    }

    pub fn compute_block_liveliness(&mut self) {
        let mut prev_in = None;
        for Instruction(_, (ins, outs), (defs, used)) in self.code.iter_mut().rev() {
            if let Some(p) = prev_in.take() {
                *outs = p;
            }
            *ins = HashSet::from_iter(outs.difference(&defs).cloned());
            ins.extend(used.clone());
            prev_in = Some(ins.clone());
        }
    }
}

#[cfg(test)]
mod block_tests {
    use super::*;

    #[test]
    fn test_liveliness_in_block() {
        let mut block = SimpleBlock::new();
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

        block.code.push(Instr::Copy(a1.clone(), Value::Int(1)).into());
        block.code.push(Instr::Copy(a2.clone(), Value::Int(1)).into());
        block.code.push(Instr::Copy(a3.clone(), Value::Int(1)).into());
        block.code.push(Instr::Copy(a4.clone(), Value::Int(1)).into());
        block.code.push(Instr::Copy(a5.clone(), Value::Int(1)).into());

        block.code.push(Instr::Asg2(x1.clone(), Value::Register(a1.clone()), BinOp::Add, Value::Register(a2.clone())).into());
        block.code.push(Instr::Asg2(x2.clone(), Value::Register(x1.clone()), BinOp::Add, Value::Register(a3.clone())).into());
        block.code.push(Instr::Asg2(x3.clone(), Value::Register(x2.clone()), BinOp::Add, Value::Register(a4.clone())).into());
        block.code.push(Instr::Asg2(x4.clone(), Value::Register(x3.clone()), BinOp::Add, Value::Register(a5.clone())).into());

        block.code.push(Instr::Call(v.clone(), "printInt".to_string(), vec![Value::Register(x4.clone())]).into());
        block.code.push(Instr::Return(Value::Int(0)).into());

        block.compute_block_liveliness();
        assert!(block.ins().is_empty());
        assert!(block.outs().is_empty());
        assert!(block.code[1].1.0.contains(&a1));
        assert!(block.code[1].1.1.contains(&a2));
        assert!(block.code[5].1.0.contains(&a1));
        assert!(block.code[5].1.0.contains(&a2));
        assert!(block.code[5].1.0.contains(&a3));
        assert!(block.code[5].1.0.contains(&a4));
        assert!(block.code[5].1.0.contains(&a5));
        assert!(block.code[5].1.1.contains(&x1));
        assert!(block.code[5].1.1.contains(&a3));
        assert!(block.code[5].1.1.contains(&a4));
        assert!(block.code[5].1.1.contains(&a5));
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Instr {
    Asg2(Reg, Value, BinOp, Value),
    Asg1(Reg, UnOp, Value),
    Copy(Reg, Value),
    Jump(Label),
    If(If),
    Call(Reg, Label, Vec<Value>),
    CallM(Reg, Value, usize, Vec<Value>),
    Extract(Reg, Value, usize),
    Insert(Value, usize, Value),
    Cast(Reg, Value),
    Return(Value),
    VReturn,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct If(pub Value, pub RelOp, pub Value, pub Label, pub Label);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RelOp {
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
    CMP,
}

impl RelOp {
    pub fn perform(&self, a: &Value, b: &Value) -> Option<bool> {
        match (a, self, b) {
            (Value::Int(i), RelOp::EQ, Value::Int(j)) => Some(i == j),
            (Value::String(i), RelOp::EQ, Value::String(j)) => Some(i == j),
            (Value::Bool(i), RelOp::EQ, Value::Bool(j)) => Some(i == j),
            (Value::Int(i), RelOp::NE, Value::Int(j)) => Some(i != j),
            (Value::String(i), RelOp::NE, Value::String(j)) => Some(i != j),
            (Value::Bool(i), RelOp::NE, Value::Bool(j)) => Some(i != j),
            (Value::Int(i), RelOp::LT, Value::Int(j)) => Some(i < j),
            (Value::Int(i), RelOp::LE, Value::Int(j)) => Some(i <= j),
            (Value::Int(i), RelOp::GT, Value::Int(j)) => Some(i > j),
            (Value::Int(i), RelOp::GE, Value::Int(j)) => Some(i >= j),
            _ => None,
        }
    }
}

impl From<IBinOp> for RelOp {
    fn from(o: IBinOp) -> Self {
        match o {
            IBinOp::LT => RelOp::LT,
            IBinOp::LE => RelOp::LE,
            IBinOp::GT => RelOp::GT,
            IBinOp::GE => RelOp::GE,
            IBinOp::EQ => RelOp::EQ,
            IBinOp::NE => RelOp::NE,
            _ => panic!("Cannot convert")
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum BinOp {
    Add,
    Mul,
    Div,
    Mod,
    Sub,
    Concat,
}

impl From<IBinOp> for BinOp {
    fn from(o: IBinOp) -> Self {
        match o {
            IBinOp::Mul => BinOp::Mul,
            IBinOp::Div => BinOp::Div,
            IBinOp::Mod => BinOp::Mod,
            IBinOp::Add => BinOp::Add,
            IBinOp::Sub => BinOp::Sub,
            _ => panic!("Cannot convert")
        }
    }
}

impl BinOp {
    pub(crate) fn perform(&self, a: &Value, b: &Value) -> Option<i32> {
        match (a, self, b) {
            (Value::Int(i), BinOp::Add, Value::Int(j)) => Some(i + j),
            (Value::Int(i), BinOp::Mul, Value::Int(j)) => Some(i * j),
            (Value::Int(i), BinOp::Div, Value::Int(j)) if *j != 0 => Some(i / j),
            (Value::Int(i), BinOp::Mod, Value::Int(j)) if *j != 0 => Some(i % j),
            (Value::Int(i), BinOp::Sub, Value::Int(j)) => Some(i - j),
            _ => None,
        }
    }
}


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum UnOp {
    IntNeg,
    Incr,
    Decr,
}

impl UnOp {
    pub(crate) fn perform(&self, v: &Value) -> Option<i32> {
        match (v, self) {
            (Value::Int(i), UnOp::IntNeg) => Some(-*i),
            (Value::Int(i), UnOp::Incr) => Some(i + 1),
            (Value::Int(i), UnOp::Decr) => Some(i - 1),
            _ => None
        }
    }
}

pub type Label = String;


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Reg {
    pub itype: IType,
    pub name: String,
}


impl Reg {
    pub fn new(itype: IType, name: String) -> Self {
        Self {
            itype,
            name,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Value {
    Register(Reg),
    Int(i32),
    String(u32),
    Bool(bool),
    Null,
    Const(Label),
}

impl Value {
    pub fn get_type(&self) -> IType {
        match self {
            Value::Register(Reg {itype, ..}) => itype.clone(),
            Value::Int(_) => IType::Int,
            Value::String(_) => IType::String,
            Value::Bool(_) => IType::Boolean,
            Value::Null => IType::Null,
            Value::Const(_) => unreachable!()
        }
    }

    pub fn to_class(&self) -> String {
        if let Value::Register(reg) = self {
            if let IType::Class(c) = &reg.itype {
                return c.clone()
            } else {
                unreachable!()
            }
        }
        {
            unreachable!()
        }
    }
}