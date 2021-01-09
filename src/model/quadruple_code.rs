use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::FromIterator;

use crate::model::ast::{IType, IBinOp};

#[derive(Debug)]
pub struct ControlFlowGraph {
    pub blocks: HashMap<Label, SimpleBlock>,
    pub functions: HashMap<String, (Label, Vec<Reg>)>,
    pub builtin: HashMap<String, (Label, u32)>,
    pub strings: HashMap<u32, String>,

    pub current_block: Vec<(Label, SimpleBlock)>
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            blocks: HashMap::new(),
            functions: HashMap::new(),
            current_block: Vec::new(),
            builtin: HashMap::new(),
            strings: HashMap::new(),
        }
    }

    pub fn begin_block(&mut self, label: Label) {
        self.current_block.push((label, SimpleBlock::new()));
    }

    pub fn extend(&mut self, code: Vec<Instr>) {
        if let Some(( _,  b)) = self.current_block.last_mut() {
            b.code.extend(code);
        }
    }

    pub fn push(&mut self, code: Instr) {
        if let Some((_,  b)) = self.current_block.last_mut() {
            if let Instr::Jump(l) = &code {
                b.jumps.insert(l.clone());
            }
            if let Instr::If(_, _, _, t, f) = &code {
                b.jumps.insert(t.clone());
                b.jumps.insert(f.clone());
            }
            b.code.push(code);
        }
    }

    pub fn jump(&mut self, label: Label) {
        if let Some((_,  b)) = self.current_block.last_mut() {
            b.jumps.insert(label);
        }
    }

    pub fn close_block(&mut self) {
        if let Some((l, mut b)) = self.current_block.pop() {
            b.live();
            self.blocks.insert(l.clone(), b);

        }
    }

    pub fn locals(&self, function: &String) -> Vec<Reg> {
        let mut reg = HashSet::new();
        for (_, block) in self.iter_fun(function) {
            reg.extend(block.code.iter().filter_map(|i| match i{
                Instr::Asg2(r, _, _, _) => Some(r),
                Instr::Asg1(r, _, _) => Some(r),
                Instr::Copy(r, _) => Some(r),
                Instr::Jump(_) => None,
                Instr::If(_, _, _, _, _) => None,
                Instr::Call(r, _, _) => Some(r),
                Instr::Return(_) => None,
                Instr::VReturn => None,
            }).cloned());
        }
        reg.into_iter().collect()
    }
}

pub struct Iter<'a> {
    visited: HashSet<Label>,
    to_visit: VecDeque<Label>,
    blocks: &'a HashMap<Label, SimpleBlock>,
    starts: Vec<Label>,
}

impl<'a> Iterator for Iter<'a> {
    type Item =  (Label, &'a SimpleBlock);

    fn next(&mut self) -> Option<Self::Item> {
        if self.to_visit.is_empty() {
            if let Some(f) = self.starts.pop() {
                self.to_visit.push_back(f);
            }
        }

        while let Some(l) =  self.to_visit.pop_front() {
            if !self.visited.contains(l.as_str()) {
                let block = self.blocks.get(l.as_str()).unwrap();
                self.to_visit.extend(block.jumps.clone());
                self.visited.insert(l.clone());
                return Some((l, block))
            } else {
                return self.next()
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

#[derive(Debug)]
pub struct SimpleBlock {
    pub code: Vec<Instr>,
    pub jumps: HashSet<Label>,
    pub ins: HashSet<Reg>,
    pub kill: HashSet<Reg>,
    pub out: HashSet<Reg>,
    pub used: HashSet<Reg>,

}

impl SimpleBlock {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            jumps: HashSet::new(),
            ins: HashSet::new(),
            kill: HashSet::new(),
            out: HashSet::new(),
            used: HashSet::new(),
        }
    }

    pub fn live(&mut self) {
        for i in self.code.iter().rev() {
            self.out = self.ins.clone();
            match i{
                Instr::Asg2(r, a, _, b) => {
                    self.kill.insert(r.clone());
                    match a {
                        Value::Register(r_v) => self.used.insert(r_v.clone()),
                        _ => false,
                    };
                    match b {
                        Value::Register(r_v) => self.used.insert(r_v.clone()),
                        _ => false,
                    };
                },
                Instr::Asg1(r, _, a) => {
                    self.kill.insert(r.clone());
                    match a {
                        Value::Register(r_v) => self.used.insert(r_v.clone()),
                        _ => false,
                    };
                },
                Instr::Copy(r, b) => {
                    self.kill.insert(r.clone());
                    match b {
                        Value::Register(r_v) => self.used.insert(r_v.clone()),
                        _ => false,
                    };
                },
                Instr::Jump(_) => { },
                Instr::If(a, _, b, _, _) => {
                    match a {
                        Value::Register(r_v) => self.used.insert(r_v.clone()),
                        _ => false,
                    };
                    match b {
                        Value::Register(r_v) => self.used.insert(r_v.clone()),
                        _ => false,
                    };
                },
                Instr::Call(r, _, params) => {
                    self.kill.insert(r.clone());
                    for v in params.iter() {
                        match v {
                            Value::Register(r_v) => self.used.insert(r_v.clone()),
                            _ => false,
                        };
                    };
                },
                Instr::Return(v) => {
                    match v {
                        Value::Register(r_v) => self.used.insert(r_v.clone()),
                        _ => false,
                    };
                },
                Instr::VReturn => { },
            }
            self.ins = HashSet::from_iter(self.out.difference(&self.kill).cloned());
            self.ins.extend(self.used.clone());
        }
        self.out = self.ins.clone();
    }
}

#[derive(Debug)]
pub enum Instr {
    Asg2(Reg, Value, BinOp, Value),
    Asg1(Reg, UnOp, Value),
    Copy(Reg, Value),
    Jump(Label),
    If (Value, RelOp, Value, Label, Label,),
    Call(Reg, Label, Vec<Value>),
    Return(Value),
    VReturn,
}

#[derive(Debug)]
pub enum RelOp {
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
}

impl From<IBinOp> for RelOp {
    fn from(o: IBinOp) -> Self {
        match o {
            IBinOp::LT => RelOp::LT,
            IBinOp::LE =>  RelOp::LE,
            IBinOp::GT =>  RelOp::GT,
            IBinOp::GE =>  RelOp::GE,
            IBinOp::EQ =>  RelOp::EQ,
            IBinOp::NE =>  RelOp::NE,
            _ => panic!("Cannot convert")
        }
    }
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Mul,
    Div,
    Mod,
    Sub,

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


#[derive(Debug)]
pub enum UnOp {
    IntNeg,
    BoolNeg,
    Incr,
    Decr,
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
            name
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Value {
    Register(Reg),
    Int(i32),
    String(u32),
    Bool(bool),
}