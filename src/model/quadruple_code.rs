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
        if let Some((l,  b)) = self.current_block.pop() {
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
    blocks: &'a HashMap<Label, SimpleBlock>
}

impl<'a> Iterator for Iter<'a> {
    type Item =  (Label, &'a SimpleBlock);

    fn next(&mut self) -> Option<Self::Item> {

        while let Some(l) =  self.to_visit.pop_front() {
            if !self.visited.contains(l.as_str()) {
                let block = self.blocks.get(l.as_str()).unwrap();
                self.to_visit.extend(block.jumps.clone());
                self.visited.insert(l.clone());
                return Some((l, block))
            }
        }
        None
    }
}

impl<'a> ControlFlowGraph {


    pub fn iter(&'a self) -> Iter<'a> {
        let functions = self.functions.values().map(|f| f.0.clone());

        Iter { visited: HashSet::new(), to_visit: VecDeque::from_iter(functions), blocks: &self.blocks }
    }

    pub fn iter_fun(&'a self, fun: &str) -> Iter<'a> {
        let function = self.functions[fun].0.clone();
        Iter { visited: HashSet::new(), to_visit: VecDeque::from(vec![function]), blocks: &self.blocks }
    }
}

#[derive(Debug)]
pub struct SimpleBlock {
    pub code: Vec<Instr>,
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

#[derive(Debug, Clone)]
pub enum Value {
    Register(Reg),
    Int(i32),
    String(u32),
    Bool(bool),
}