use crate::model::quadruple_code::{ControlFlowGraph, SimpleBlock, Instr, Value, RelOp, Reg, Label, BinOp, UnOp};
use crate::model::assembler::{Opcode, Target, Register, Memory};
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use crate::model::assembler::Register::{ESP, EBP, EAX, EDI, EBX, EDX};
use crate::model::ast::IType;
use crate::backend::assembler_transformer::RegState::{Free, Used, Reserved};


#[derive(Debug, Eq, PartialEq, Clone)]
enum RegState {
    Free,
    Reserved,
    Used,
}

#[derive(Debug, Eq, PartialEq, Hash)]
enum TTarget {
    Reg(Register),
    Mem(Memory),
}

#[derive(Debug)]
pub struct AssemblerTransformer {
    code: Vec<Opcode>,
    to_target: HashMap<Reg, HashSet<TTarget>>,
    to_value: HashMap<Target, HashSet<Reg>>,
    memory: HashMap<Reg, Memory>,
    usage: HashMap<Register, u32>,
    last_usage: u32,
    all_registers: HashMap<Register, RegState>,
    end_label: Option<Label>,
    strings: HashMap<u32, Label>,
}

impl AssemblerTransformer {
    pub fn new(all_registers: HashSet<Register>) -> Self {
        Self {
            code: Vec::new(),
            to_value: HashMap::new(),
            usage: HashMap::from_iter(all_registers.iter().map(|k| (k.clone(), 0))),
            all_registers: HashMap::from_iter(all_registers.iter().map(|k| (k.clone(), Free))),
            memory: HashMap::new(),
            last_usage: 0,
            end_label: None,
            strings: HashMap::new(),
            to_target: HashMap::new(),
        }
    }

    fn get_free_register(&mut self) -> Register {
        match self.all_registers.iter().filter(|(_, v)| **v == Free).next() {
            None => {
                let least = self.get_least_register();
                let least_target = Target::Reg(least.clone());
                self.make_free(&least_target);
                least
            }
            Some((r, _)) => r.clone()
        }
    }

    fn make_free(&mut self, reg: &Target) {
        for var in self.to_value.get(reg).cloned().unwrap_or(HashSet::new()).iter() {
            self.dump_to_memory(var);
        }
        self.free_target(reg.clone());
    }

    fn dump_to_memory(&mut self, val: &Reg) {
        let v_mem = self.memory[val].clone();
        if !self.is_in_correct_memory(val) {
            if let Some(r) = self.maybe_register_target(val) {
                self.move_value(Target::Reg(r), Target::Memory(v_mem.clone()));
            } else if let Some(m) = self.maybe_memory_target(val) {
                self.move_value(Target::Memory(m), Target::Memory(v_mem.clone()));
            }
        }
    }

    fn make_copy_if_needed(&mut self, target: &Target) {
        for var in self.to_value.get(target).cloned().unwrap_or_default().iter() {
            if self.to_target.entry(var.clone()).or_default().len() == 1 {
                self.dump_to_memory(var);
            }
        }
    }

    fn is_in_correct_memory(&self, reg: &Reg) -> bool {
        let v_mem = &self.memory[reg];
        self.to_target[reg].iter().filter(|t| match t {
            TTarget::Mem(m) if m == v_mem => true,
            _ => false
        }).next().is_some()
    }

    fn maybe_register_target(&self, reg: &Reg) -> Option<Register> {
        self.to_target[reg].iter().filter_map(|t| match t {
            TTarget::Reg(r) => Some(r),
            _ => None
        }).next().cloned()
    }

    fn maybe_memory_target(&self, reg: &Reg) -> Option<Memory> {
        self.to_target[reg].iter().filter_map(|t| match t {
            TTarget::Mem(m) => Some(m),
            _ => None
        }).next().cloned()
    }

    fn dump_all(&mut self) {
        let vars: Vec<Reg> = self.to_target.keys().cloned().collect();
        for var in vars {
            self.dump_to_memory(&var);
        }
        for (v, _) in self.memory.iter() {
            assert!(self.is_in_correct_memory(v));
        }
        self.to_target.clear();
        self.to_value.clear();
        self.all_registers.iter_mut().for_each(|(_, v)| *v = Free);
        for (v, m) in self.memory.iter() {
            self.to_target.entry(v.clone()).or_default().insert(TTarget::Mem(m.clone()));
            self.to_value.entry(Target::Memory(m.clone())).or_default().insert(v.clone());
        }
    }

    // it does not clear from
    fn move_value(&mut self, from: Target, to: Target) {
        if from == to {
            panic!("Cannot move to itself");
        }
        let vars = self.to_value.entry(to.clone()).or_default().clone();
        for var in vars.iter().next() {
            if self.to_target.entry(var.clone()).or_default().len() == 1 {
                let var_mem = self.memory[var].clone();
                self.move_value(to.clone(), Target::Memory(var_mem));
            }
        }
        self.free_target(to.clone());
        match (&from, &to) {
            (Target::Memory(_), Target::Memory(_)) => {
                self.code.push(Opcode::Push(Target::Reg(EBX)));
                self.code.push(Opcode::Mov(Target::Reg(EBX), from.clone()));
                self.code.push(Opcode::Mov(to.clone(), Target::Reg(EBX)));
                self.code.push(Opcode::Pop(EBX));
            }
            (_, _) => self.code.push(Opcode::Mov(to.clone(), from.clone())),
        }
        let vars: Vec<Reg> = self.to_value.entry(from).or_default().iter().cloned().collect();
        for var in vars.iter() {
            self.put_into_target(var, &to);
        }
    }

    fn get_least_register(&self) -> Register {
        self.usage.iter().filter(|(r, _)| self.all_registers[r] == Used)
            .min_by_key(|x| *x.1).unwrap().0.clone()
    }

    fn put_into_target(&mut self, var: &Reg, target: &Target) {
        match target {
            Target::Reg(r) => {
                self.to_target.entry(var.clone()).or_default().insert(TTarget::Reg(r.clone()));
                self.all_registers.insert(r.clone(), Used);
                self.last_usage += 1;
                self.usage.insert(r.clone(), self.last_usage);
            }
            Target::Memory(m) => {
                self.to_target.entry(var.clone()).or_default().insert(TTarget::Mem(m.clone()));
            }
            _ => panic!("Invalid target")
        }
        self.to_value.entry(target.clone()).or_default().insert(var.clone());
    }

    fn free_target(&mut self, target: Target) -> Vec<Reg> {
        let vars = self.to_value.entry(target.clone()).or_default();
        let mut res = vec![];
        match &target {
            Target::Reg(r) => {
                for var in vars.drain() {
                    self.to_target.entry(var.clone()).or_default().remove(&TTarget::Reg(r.clone()));
                    res.push(var)
                }
                self.all_registers.insert(r.clone(), Free);
            }
            Target::Memory(m) => {
                for var in vars.drain() {
                    self.to_target.entry(var.clone()).or_default().remove(&TTarget::Mem(m.clone()));
                    res.push(var)
                }
            }
            _ => panic!("Invalid target")
        }
        res
    }

    fn free_value(&mut self, reg: &Reg) {
        for t in self.to_target.entry(reg.clone()).or_default().drain() {
            match t {
                TTarget::Reg(r) => {
                    let vals = self.to_value.entry(Target::Reg(r.clone())).or_default();
                    vals.remove(reg);
                    if vals.is_empty() {
                        self.all_registers.insert(r, Free);
                    }
                }
                TTarget::Mem(m) => {
                    self.to_value.entry(Target::Memory(m)).or_default().remove(reg);
                }
            }
        }
    }

    // get target with location of v
    fn get_target(&mut self, v: &Value) -> Target {
        match v {
            Value::Register(r) => {
                if let Some(reg) = self.maybe_register_target(r) {
                    Target::Reg(reg.clone())
                } else if let Some(mem) = self.maybe_memory_target(r) {
                    Target::Memory(mem.clone())
                } else {
                    panic!("Cannot find this value")
                }
            }
            Value::Int(i) => Target::Imm(*i),
            Value::String(s) => {
                let s_label = self.alloc_string(s);
                Target::Label(s_label)
            }
            Value::Bool(b) => {
                if *b { Target::Imm(1) } else { Target::Imm(0) }
            }
        }
    }

    fn get_reg_target(&mut self, v: &Value) -> Target {
        let t = self.get_target(v);
        match t {
            Target::Reg(_) => t,
            _ => {
                let f_reg = self.get_free_register();
                let f_target = Target::Reg(f_reg.clone());
                self.code.push(Opcode::Mov(f_target.clone(), t));
                self.all_registers.insert(f_reg, Reserved);
                f_target
            }
        }
    }
    fn unreserve(&mut self, target: &Target) {
        match target {
            Target::Reg(r) => {
                self.all_registers.insert(r.clone(), Used);
            }
            _ => unreachable!("cannot ureserve")
        }
    }
}


impl AssemblerTransformer {
    pub fn transform(&mut self, graph: &ControlFlowGraph) -> Vec<Opcode> {
        let bultins: Vec<String> = graph.builtin.keys().cloned().collect();
        self.code.extend(vec![
            Opcode::Special(format!("global _start")),
            Opcode::Special(format!("section .text")),
            Opcode::Special(format!("extern {}, _concatString", bultins.join(", "))),
            Opcode::Label(format!("_start")),
            Opcode::Call(format!("main")),
            Opcode::Mov(Target::Reg(EDI), Target::Reg(EAX)),
            Opcode::Mov(Target::Reg(EAX), Target::Imm(60)),
            Opcode::Special(format!("syscall")),
        ]);

        for (function, (label, args)) in graph.functions.iter() {
            self.to_target.clear();
            self.to_value.clear();
            self.memory.clear();

            self.end_label = Some(format!("{}_endl", label));

            let locals = graph.locals(function);
            let locals_num = locals.len();
            for (i, reg) in locals.into_iter().enumerate() {
                self.memory.insert(reg.clone(), Memory::new(-(1 + i as i32)));
            }
            for (i, reg) in args.iter().enumerate() {
                self.memory.insert(reg.clone(), Memory::new((i + 2) as i32));
                //self.put_into_target(reg, &Target::Memory(Memory::new((i + 2) as i32)));
            }

            self.code.push(Opcode::Label(function.clone()));
            self.code.push(Opcode::Push(Target::Reg(EBP)));
            self.code.push(Opcode::Mov(Target::Reg(EBP), Target::Reg(ESP)));
            self.code.push(Opcode::Sub(Target::Reg(ESP), Target::Imm(8 * locals_num as i32)));
            self.code.push(Opcode::Jmp(label.clone()));


            for (label, block) in graph.iter_fun(function) {
                self.to_target.clear();
                self.to_value.clear();
                for (val, mem) in self.memory.iter() {
                    self.to_target.insert(val.clone(), HashSet::from_iter(vec![TTarget::Mem(mem.clone())]));
                }
                self.code.push(Opcode::Label(label));
                self.transform_block(block)
            }


            let label = self.end_label.take().unwrap();
            self.code.push(Opcode::Label(label));
            if locals_num > 0 {
                self.code.push(Opcode::Add(Target::Reg(ESP), Target::Imm((locals_num * 8) as i32)));
            }
            self.code.push(Opcode::Pop(EBP));
            self.code.push(Opcode::Ret);
        }


        for (id, label) in self.strings.iter() {
            if let Some(s) = graph.strings.get(id) {
                self.code.push(Opcode::Special(format!("{} db '{}',0", label, s)));
            } else {
                unreachable!("Unknown string");
            }
        }

        self.code.push(Opcode::Ret);
        let returned = self.code.drain(..);
        returned.as_slice().windows(2).filter_map(|f| match (f[0].clone(), f[1].clone()) {
            (Opcode::Jmp(a), Opcode::Label(b)) if a == b => None,
            (a, _) => Some(a)
        }).collect()
    }


    fn transform_block(&mut self, block: &SimpleBlock) {
        for instr in &block.code {
            for (_, r) in self.all_registers.iter() {
                assert_ne!(*r, Reserved, );
            }

            match instr {
                Instr::Asg2(ret, a, op, b) => {
                    match (op, &ret.itype) {
                        (BinOp::Add, IType::String) => {
                            let b_target = self.get_target(b);
                            self.code.push(Opcode::Push(b_target));
                            let a_target = self.get_target(a);
                            self.code.push(Opcode::Push(a_target));

                            self.dump_all();
                            self.code.push(Opcode::Call("_concatString".to_string()));
                            self.code.push(Opcode::Add(Target::Reg(ESP), Target::Imm(8 * 2 as i32)));

                            self.free_value(ret);
                            self.put_into_target(ret, &Target::Reg(EAX))
                        }
                        (BinOp::Add, _) => {
                            let a_target = self.get_reg_target(a);
                            self.make_copy_if_needed(&a_target);
                            let b_target = self.get_target(b);
                            self.code.push(Opcode::Add(a_target.clone(), b_target));
                            self.free_target(a_target.clone());

                            self.free_value(ret);
                            self.put_into_target(ret, &a_target);
                        }
                        (BinOp::Mul, _) => {
                            let a_target = self.get_reg_target(a);
                            self.make_copy_if_needed(&a_target);
                            let b_target = self.get_target(b);
                            self.code.push(Opcode::Mul(a_target.clone(), b_target));
                            self.free_target(a_target.clone());
                            self.free_value(ret);
                            self.put_into_target(ret, &a_target);
                        }
                        (BinOp::Div, _) => {
                            let a_target = self.get_target(a);
                            let eax_target = Target::Reg(EAX);
                            if a_target != eax_target {
                                self.move_value(a_target.clone(), eax_target.clone());
                            }
                            self.make_copy_if_needed(&eax_target);

                            let edx_target = Target::Reg(EDX);
                            self.make_free(&edx_target);
                            self.all_registers.insert(EDX, Reserved);

                            self.code.push(Opcode::Special("cqo".to_string()));

                            let b_target = self.get_reg_target(b);
                            self.code.push(Opcode::Div(b_target.clone()));

                            self.free_target(eax_target.clone());
                            self.free_target(edx_target);
                            self.free_value(ret);
                            self.unreserve(&b_target);
                            self.put_into_target(ret, &eax_target);
                        }
                        (BinOp::Mod, _) => {
                            let a_target = self.get_target(a);
                            let eax_target = Target::Reg(EAX);
                            if a_target != eax_target {
                                self.move_value(a_target.clone(), eax_target.clone());
                            }
                            self.make_copy_if_needed(&eax_target);

                            let edx_target = Target::Reg(EDX);
                            self.make_free(&edx_target);
                            self.all_registers.insert(EDX, Reserved);

                            self.code.push(Opcode::Special("cqo".to_string()));

                            let b_target = self.get_reg_target(b);
                            self.code.push(Opcode::Div(b_target.clone()));

                            self.free_target(eax_target.clone());
                            self.free_target(edx_target.clone());
                            self.free_value(ret);
                            self.unreserve(&b_target);
                            self.put_into_target(ret, &edx_target);
                        }
                        (BinOp::Sub, _) => {
                            let a_target = self.get_reg_target(a);
                            self.make_copy_if_needed(&a_target);
                            let b_target = self.get_target(b);
                            self.code.push(Opcode::Sub(a_target.clone(), b_target));
                            self.free_target(a_target.clone());
                            self.free_value(ret);
                            self.put_into_target(ret, &a_target);
                        }
                    }
                }
                Instr::Asg1(r, o, v) => {
                    match o {
                        UnOp::IntNeg => {
                            let v_target = self.get_target(v);
                            let f = self.get_free_register();
                            let f_target = Target::Reg(f);
                            self.code.push(Opcode::Mov(f_target.clone(), v_target));
                            self.code.push(Opcode::Neg(f_target.clone()));
                            self.free_value(r);
                            self.put_into_target(r, &f_target);
                        }
                        UnOp::Incr => {
                            let v_target = self.get_target(v);
                            let f = self.get_free_register();
                            let f_target = Target::Reg(f);
                            self.code.push(Opcode::Mov(f_target.clone(), v_target));
                            self.code.push(Opcode::Add(f_target.clone(), Target::Imm(1)));
                            self.free_value(r);
                            self.put_into_target(r, &f_target);
                        }
                        UnOp::Decr => {
                            let v_target = self.get_target(v);
                            let f = self.get_free_register();
                            let f_target = Target::Reg(f);
                            self.code.push(Opcode::Mov(f_target.clone(), v_target));
                            self.code.push(Opcode::Sub(f_target.clone(), Target::Imm(1)));
                            self.free_value(r);
                            self.put_into_target(r, &f_target);
                        }
                    }
                }
                Instr::Copy(t, v) => {
                    match v {
                        Value::Register(r) => {
                            match (self.maybe_register_target(r), self.maybe_memory_target(r)) {
                                (None, None) => {
                                    panic!("Where is source of assignment?")
                                }
                                (None, Some(mem)) => {
                                    let f_reg = self.get_free_register();
                                    let v_target = Target::Reg(f_reg);
                                    self.free_value(t);
                                    self.put_into_target(t, &v_target);
                                    self.put_into_target(r, &v_target);
                                    self.code.push(Opcode::Mov(v_target, Target::Memory(mem.clone())));
                                }

                                (Some(reg), _) => {
                                    self.free_value(t);
                                    self.put_into_target(t, &Target::Reg(reg));
                                }
                            }
                        }
                        Value::Int(i) => {
                            let f_reg = self.get_free_register();
                            let v_target = Target::Reg(f_reg);
                            self.free_value(t);
                            self.put_into_target(t, &v_target);
                            self.code.push(Opcode::Mov(v_target, Target::Imm(*i)))
                        }
                        Value::String(s) => {
                            let f_reg = self.get_free_register();
                            let v_target = Target::Reg(f_reg);
                            self.free_value(t);
                            self.put_into_target(t, &v_target);
                            let s = self.alloc_string(s);
                            self.code.push(Opcode::Mov(v_target, Target::Label(s)));
                        }
                        Value::Bool(b) => {
                            let f_reg = self.get_free_register();
                            let v_target = Target::Reg(f_reg);
                            self.free_value(t);
                            self.put_into_target(t, &v_target);
                            self.code.push(Opcode::Mov(v_target, Target::Imm(if *b { 1 } else { 0 })))
                        }
                    }
                }
                Instr::Jump(l) => {
                    self.dump_all();
                    self.code.push(Opcode::Jmp(l.clone()))
                }
                Instr::If(x, o, y, t, f) => {
                    self.dump_all();
                    let x_reg = self.get_reg_target(x);
                    let y_reg = self.get_target(y);
                    self.code.push(Opcode::Cmp(x_reg.clone(), y_reg));
                    self.unreserve(&x_reg);
                    match o {
                        RelOp::LT => self.code.push(Opcode::Jlt(t.clone())),
                        RelOp::LE => self.code.push(Opcode::Jle(t.clone())),
                        RelOp::GT => self.code.push(Opcode::Jgt(t.clone())),
                        RelOp::GE => self.code.push(Opcode::Jge(t.clone())),
                        RelOp::EQ => self.code.push(Opcode::Je(t.clone())),
                        RelOp::NE => self.code.push(Opcode::Jne(t.clone())),
                    };
                    self.code.push(Opcode::Jmp(f.clone()));
                }
                Instr::Call(ret, label, args) => {
                    for arg in args.clone().iter().rev() { // add args
                        let a_reg = self.get_target(arg);
                        self.code.push(Opcode::Push(a_reg))
                    }
                    self.dump_all();
                    self.code.push(Opcode::Call(label.clone()));
                    if args.len() > 0 { // remove args
                        self.code.push(Opcode::Add(Target::Reg(ESP), Target::Imm(8 * args.len() as i32)))
                    }
                    if ret.itype != IType::Void {
                        self.free_value(ret);
                        self.put_into_target(ret, &Target::Reg(EAX))
                    }
                }
                Instr::Return(v) => {
                    match v {
                        Value::Register(r) => {
                            match (self.maybe_register_target(r), self.maybe_memory_target(r)) {
                                (Some(reg), _)  if reg == EAX => {}
                                (Some(reg), _) => {
                                    let v_target = Target::Reg(reg.clone());
                                    let ret_target = Target::Reg(EAX);
                                    self.make_free(&ret_target);
                                    self.put_into_target(r, &ret_target);
                                    self.code.push(Opcode::Mov(ret_target, v_target));
                                }
                                (_, Some(mem)) => {
                                    let v_target = Target::Memory(mem.clone());
                                    let ret_target = Target::Reg(EAX);
                                    self.make_free(&ret_target);
                                    self.put_into_target(r, &ret_target);
                                    self.code.push(Opcode::Mov(ret_target, v_target));
                                }
                                (_, _) => panic!("where is return variable?")
                            }
                        }
                        Value::Int(i) => {
                            let ret_target = Target::Reg(EAX);
                            self.make_free(&ret_target);
                            self.code.push(Opcode::Mov(ret_target, Target::Imm(*i)))
                        }
                        Value::String(s) => {
                            let ret_target = Target::Reg(EAX);
                            self.make_free(&ret_target);
                            let s = self.alloc_string(s);
                            self.code.push(Opcode::Mov(ret_target, Target::Label(s)));
                        }
                        Value::Bool(b) => {
                            let ret_target = Target::Reg(EAX);
                            self.make_free(&ret_target);
                            self.code.push(Opcode::Mov(ret_target, Target::Imm(if *b { 1 } else { 0 })))
                        }
                    }
                    self.code.push(Opcode::Jmp(self.get_final_label()));
                }
                Instr::VReturn => {
                    self.code.push(Opcode::Jmp(self.get_final_label()));
                }
            };
        }
    }

    fn alloc_string(&mut self, string_id: &u32) -> Label {
        self.strings.entry(*string_id).or_insert(format!("__string_{}", string_id)).clone()
    }

    fn get_final_label(&self) -> Label {
        if let Some(label) = self.end_label.clone() {
            label
        } else {
            panic!("Not in function")
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::assembler::Register::*;
    use crate::model::ast::IType;

    #[test]
    fn test_move_multiple() {
        let regs_set = HashSet::from_iter(vec![EAX, EBX]);
        let mut assembler_transformer = AssemblerTransformer::new(regs_set.clone());
        let vars = [
            Reg::new(IType::Int, "a1".to_string()),
            Reg::new(IType::Int, "a2".to_string()),
            Reg::new(IType::Int, "a3".to_string()),
            Reg::new(IType::Int, "a4".to_string())];
        let regs = Vec::from_iter(regs_set);
        let mut i = 0;
        for v in vars.iter() {
            assembler_transformer.memory.insert(v.clone(), Memory::new(i));
            i += 4;
        }

        i = 0;
        for v in vars[..3].iter() {
            assembler_transformer.put_into_target(v, &Target::Memory(Memory::new(i + 4)));
            i += 4;
        }
        assembler_transformer.put_into_target(&vars[0], &Target::Reg(regs[0].clone()));
        assembler_transformer.put_into_target(&vars[3], &Target::Reg(regs[1].clone()));

        assembler_transformer.free_target(Target::Reg(regs[0].clone()));
        assembler_transformer.put_into_target(&vars[0], &Target::Reg(regs[0].clone()));

        println!("usage {:?}", assembler_transformer.usage);
        println!("to_target {:?}", assembler_transformer.to_target);
        println!("to_value {:?}", assembler_transformer.to_value);

        let reg = assembler_transformer.get_free_register();

        println!("usage {:?}", assembler_transformer.usage);
        println!("to_target {:?}", assembler_transformer.to_target);
        println!("to_value {:?}", assembler_transformer.to_value);

        assert!(assembler_transformer.is_in_correct_memory(&vars[3]));
        assert!(assembler_transformer.is_in_correct_memory(&vars[2]));
        assert!(assembler_transformer.is_in_correct_memory(&vars[1]));
        assert_eq!(assembler_transformer.maybe_memory_target(&vars[0]), None);
        assert_eq!(assembler_transformer.maybe_register_target(&vars[3]), None);
        assert_eq!(reg, regs[1])
    }

    #[test]
    fn test_move_memory() {
        let regs_set = HashSet::from_iter(vec![EAX, EBX, ECX, EDX]);
        let mut assembler_transformer = AssemblerTransformer::new(regs_set.clone());
        let vars = [
            Reg::new(IType::Int, "a1".to_string()),
            Reg::new(IType::Int, "a2".to_string()),
            Reg::new(IType::Int, "a3".to_string()),
            Reg::new(IType::Int, "a4".to_string())];
        let regs = Vec::from_iter(regs_set);
        let mut i = 0;
        for v in vars.iter() {
            assembler_transformer.memory.insert(v.clone(), Memory::new(i));
            i += 4;
        }

        i = 0;
        for (v, r) in vars[..3].iter().zip(regs.iter()) {
            assembler_transformer.put_into_target(v, &Target::Reg(r.clone()));
            assembler_transformer.put_into_target(v, &Target::Memory(Memory::new(i + 4)));
            i += 4;
        }
        assembler_transformer.put_into_target(&vars[3], &Target::Reg(regs[3].clone()));

        for (v, r) in vars[..3].iter().zip(regs[..3].iter()) {
            assembler_transformer.free_target(Target::Reg(r.clone()));
            assembler_transformer.put_into_target(v, &Target::Reg(r.clone()));
        }

        assert_eq!(assembler_transformer.maybe_register_target(&vars[0]), Some(regs[0].clone()));
        assert!(!assembler_transformer.is_in_correct_memory(&vars[0]));

        let reg = assembler_transformer.get_free_register();

        assert!(assembler_transformer.is_in_correct_memory(&vars[3]));
        assert_eq!(assembler_transformer.maybe_memory_target(&vars[2]), None);
        assert_eq!(assembler_transformer.maybe_register_target(&vars[3]), None);
        assert_eq!(reg, regs[3])
    }

    #[test]
    fn test_free_register() {
        let regs_set = HashSet::from_iter(vec![EAX, EBX, ECX, EDX]);
        let mut assembler_transformer = AssemblerTransformer::new(regs_set.clone());
        let vars = [
            Reg::new(IType::Int, "a1".to_string()),
            Reg::new(IType::Int, "a2".to_string()),
            Reg::new(IType::Int, "a3".to_string()),
            Reg::new(IType::Int, "a4".to_string())];
        let regs = Vec::from_iter(regs_set);

        let mut i = 0;
        for v in vars.iter() {
            assembler_transformer.memory.insert(v.clone(), Memory::new(i));
            i += 4;
        }

        for (v, r) in vars[..3].iter().zip(regs.iter()) {
            assembler_transformer.put_into_target(v, &Target::Reg(r.clone()));
        }
        for (v, r) in vars[1..3].iter().zip(regs.iter()) {
            assembler_transformer.free_target(Target::Reg(r.clone()));
            assembler_transformer.put_into_target(v, &Target::Reg(r.clone()));
        }
        // println!("usage {:?}", assembler_transformer.usage);
        // println!("to_memory {:?}", assembler_transformer.to_memory);
        // println!("to_register {:?}", assembler_transformer.to_register);
        // println!("to_value {:?}", assembler_transformer.to_value);

        let reg = assembler_transformer.get_free_register();
        assert_eq!(reg, regs[3])
    }
}