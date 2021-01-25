use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::FromIterator;

use crate::backend::assembler_transformer::RegState::{Free, Reserved, Used};
use crate::model::assembler::{Memory, Opcode, Register, Target};
use crate::model::assembler::Register::{EAX, EBP, EBX, EDI, EDX, ESP};
use crate::model::ast::IType;
use crate::model::quadruple_code::{BinOp, ControlFlowGraph, Instr, Label, Reg, RelOp, SimpleBlock, UnOp, Value, Instruction, If};

#[derive(Debug, Eq, PartialEq, Clone)]
enum RegState {
    Free,
    Reserved,
    Used,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
enum TTarget {
    Reg(Register),
    Mem(Memory),
    Null,
}

#[derive(Debug)]
pub struct AssemblerTransformer {
    code: Vec<Opcode>,
    to_target: HashMap<Reg, HashSet<TTarget>>,
    to_value: HashMap<Target, HashSet<Reg>>,
    memory: HashMap<Reg, Memory>,
    all_registers: HashMap<Register, RegState>,
    end_label: Option<Label>,
    strings: HashMap<u32, Label>,

    outs: HashSet<Reg>,
    used: VecDeque<HashSet<Reg>>,
}

impl AssemblerTransformer {
    pub fn new(all_registers: HashSet<Register>) -> Self {
        Self {
            code: Vec::new(),
            to_value: HashMap::new(),
            all_registers: HashMap::from_iter(all_registers.iter().map(|k| (k.clone(), Free))),
            memory: HashMap::new(),
            end_label: None,
            strings: HashMap::new(),
            to_target: HashMap::new(),
            outs: HashSet::new(),
            used: VecDeque::new(),
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
            self.dump_to_memory(var, false);
        }
        self.mark_free_target(reg);
    }

    fn dump_to_memory(&mut self, var: &Reg, is_dump_all: bool) {
        let v_mem = self.memory[var].clone();
        if (self.outs.contains(var) || (!is_dump_all && self.used[0].contains(var))) && !self.is_in_correct_memory(var) {
            if let Some(r) = self.maybe_register_target(var) {
                self.move_value(Target::Reg(r), Target::Memory(v_mem.clone()));
            } else if let Some(m) = self.maybe_memory_target(var) {
                self.move_value(Target::Memory(m), Target::Memory(v_mem.clone()));
            }
        }
    }

    fn make_copy_if_needed(&mut self, target: &Target) {
        for var in self.to_value.get(target).cloned().unwrap_or_default().iter() {
            if self.to_target.entry(var.clone()).or_default().len() == 1 {
                self.dump_to_memory(var, false);
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

    fn maybe_null_target(&self, reg: &Reg) -> bool {
        self.to_target[reg].iter().filter(|t| match t {
            TTarget::Null => true,
            _ => false
        }).next().is_some()
    }

    fn dump_all(&mut self, call_ret: Option<&Reg>) {
        let vars: Vec<Reg> = self.to_target.keys().cloned().collect();
        for var in vars {
            self.dump_to_memory(&var, true);
        }
        for v in self.outs.iter().filter(|r| call_ret.map(|c| c != *r).unwrap_or(true)) {
            assert!(self.is_in_correct_memory(v));
        }
        self.to_target.clear();
        self.to_value.clear();
        self.all_registers.iter_mut().for_each(|(_, v)| *v = Free);
        for v in self.outs.iter() {
            let m = self.memory[v].clone();
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
        self.mark_free_target(&to);
        match (&from, &to) {
            (Target::Memory(_), Target::Memory(_)) => {
                self.code.push(Opcode::Push(Target::Reg(EBX)));
                self.code.push(Opcode::Mov(Target::Reg(EBX), from.clone()));
                self.code.push(Opcode::Mov(to.clone(), Target::Reg(EBX)));
                self.code.push(Opcode::Pop(EBX));
            }
            (_, Target::Reg(_)) |
            (_, Target::Memory(_)) => self.code.push(Opcode::Mov(to.clone(), from.clone())),
            _ => panic!("Copying to invalid target"),
        }
        let vars: Vec<Reg> = self.to_value.entry(from).or_default().iter().cloned().collect();
        for var in vars.iter() {
            self.put_into_target_move(var, &to);
        }
    }

    // fn get_least_register(&self) -> Register {
    //     self.usage.iter().filter(|(r, _)| self.all_registers[r] == Used)
    //         .min_by_key(|x| *x.1).unwrap().0.clone()
    // }

    fn get_least_register(&self) -> Register {
        let mut all_regs: HashSet<Register> = self.all_registers.iter().filter_map(|(k, v)| if *v != RegState::Reserved {
            Some(k.clone())
        } else {
            None
        }).collect();
        for used in self.used.iter() {
            for used_reg in used.iter() {
                let regs: Vec<Register> = self.to_target.get(used_reg).cloned().unwrap_or(HashSet::new()).into_iter()
                    .filter_map(|t| match t {
                        TTarget::Reg(rr) => Some(rr),
                        TTarget::Mem(_) => None,
                        TTarget::Null => None
                    }).collect();
                if regs.len() >= 2 {
                    return regs[0].clone();
                } else {
                    for r in regs {
                        all_regs.remove(&r);
                        if all_regs.len() == 0 {
                            return r;
                        }
                    }
                }
            }
        }
        all_regs.into_iter().next().unwrap()
        // self.usage.iter().filter(|(r, _)| self.all_registers[r] == Used)
        //     .min_by_key(|x| *x.1).unwrap().0.clone()
    }

    fn put_into_target(&mut self, var: &Reg, target: &Target) {
        self.put_into_target_bool(var, target, false)
    }

    fn put_into_target_move(&mut self, var: &Reg, target: &Target) {
        self.put_into_target_bool(var, target, true)
    }


    fn put_into_target_bool(&mut self, var: &Reg, target: &Target, is_move: bool) {
        if self.outs.contains(var) || (is_move && self.used[0].contains(var)) {
            match target {
                Target::Reg(r) => {
                    self.to_target.entry(var.clone()).or_default().insert(TTarget::Reg(r.clone()));
                    self.all_registers.insert(r.clone(), Used);
                }
                Target::Memory(m) => {
                    self.to_target.entry(var.clone()).or_default().insert(TTarget::Mem(m.clone()));
                }
                Target::Null => {
                    self.to_target.entry(var.clone()).or_default().insert(TTarget::Null);
                }
                _ => panic!("Invalid target")
            }
            self.to_value.entry(target.clone()).or_default().insert(var.clone());
        }
    }

    fn mark_free_target(&mut self, target: &Target) -> Vec<Reg> {
        let vars = self.to_value.entry(target.clone()).or_default();
        let mut res = vec![];
        match target {
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
            _ => unreachable!("Invalid target")
        }
        res
    }

    fn mark_free_value(&mut self, reg: &Reg) {
        for t in self.to_target.entry(reg.clone()).or_default().drain() {
            match t {
                TTarget::Reg(r) => {
                    let vals = self.to_value.entry(Target::Reg(r.clone())).or_default();
                    vals.remove(reg);
                    if vals.is_empty() {
                        self.all_registers.insert(r, Free);
                    }
                },
                TTarget::Mem(m) => {
                    self.to_value.entry(Target::Memory(m)).or_default().remove(reg);
                },
                TTarget::Null => {}
            }
        }
    }

    /// get target with location of `v`
    fn get_target(&mut self, v: &Value) -> Target {
        match v {
            Value::Register(r) => {
                if let Some(reg) = self.maybe_register_target(r) {
                    Target::Reg(reg.clone())
                } else if let Some(mem) = self.maybe_memory_target(r) {
                    Target::Memory(mem.clone())
                } else if self.maybe_null_target(r) {
                   Target::Null
                } else {
                    panic!("Cannot find this value")
                }
            }
            Value::Int(i) => Target::Imm(*i),
            Value::String(s) => {
                let s_label = self.alloc_string(s);
                Target::Var(s_label)
            }
            Value::Bool(b) => {
                if *b { Target::Imm(1) } else { Target::Imm(0) }
            }
            Value::Null => Target::Null,
            Value::Const(label) => Target::Var(label.clone()),
        }
    }

    /// get register target with `v`, emit MOV if necessary
    fn get_reg_target(&mut self, v: &Value) -> Target {
        let t = self.get_target(v);
        match t {
            Target::Reg(_) => t,
            Target::Var(_) => {
                let f_reg = self.get_free_register();
                let f_target = Target::Reg(f_reg.clone());
                self.code.push(Opcode::Special(format!("lea {}, {}", f_target, t)));
                f_target
            },
            _ => {
                let f_reg = self.get_free_register();
                let f_target = Target::Reg(f_reg.clone());
                self.code.push(Opcode::Mov(f_target.clone(), t));
                self.all_registers.insert(f_reg, Reserved);
                f_target
            }
        }
    }

    /// Change status of Register to Used
    fn commit_register(&mut self, target: &Target) {
        let vars = self.to_value.get(target).cloned().unwrap_or(HashSet::new());
        for var in vars.iter() {
            if !self.outs.contains(var) {
                self.mark_free_value(var);
            }
        }

        if let Target::Reg(r) = target {
            if !self.to_value.get(target).map(|s| s.is_empty()).unwrap_or(true) {
                self.all_registers.insert(r.clone(), Used);
            } else {
                self.all_registers.insert(r.clone(), Free);
            }
        }
    }

    fn alloc_string(&mut self, string_id: &u32) -> Label {
        self.strings.entry(*string_id).or_insert(format!("__string_{}", string_id)).clone()
    }

    fn get_final_label(&self) -> Label {
        if let Some(label) = self.end_label.clone() {
            label
        } else {
            unreachable!("Not in function")
        }
    }
}


impl AssemblerTransformer {
    pub fn transform(&mut self, graph: &ControlFlowGraph) -> Vec<Opcode> {
        self.add_start(graph.builtin.keys().cloned().collect());

        for (function, (label, args)) in graph.functions.iter() {
            self.to_target.clear();
            self.to_value.clear();
            self.memory.clear();
            self.end_label = Some(format!("{}_endl", label));

            let locals = graph.locals(function);
            self.add_function_args_and_locals(&locals, args);

            self.transform_fun_start(function, label, locals.len());
            for (label, block) in graph.iter_fun(function) {
                self.to_target.clear();
                self.to_value.clear();
                for val in block.ins().iter() {
                    let mem = if let Some(m) = self.memory.get(val) {
                        m.clone()
                    } else {
                        panic!()
                    };
                    self.to_target.insert(val.clone(), HashSet::from_iter(vec![TTarget::Mem(mem.clone())]));
                    self.to_value.insert(Target::Memory(mem.clone()), HashSet::from_iter(vec![val.clone()]));
                }
                self.code.push(Opcode::Label(label.clone()));
                self.transform_block(block)
            }
            self.transform_fun_end(locals.len());
        }

        self.transform_add_static_strings(graph);
        for (class, (methods, _)) in graph.classes.iter() {
            self.add_vtable(format!("_vtable_{}", class), methods);
        }

        self.remove_one_line_jumps()
    }

    fn add_start(&mut self, built_ins: Vec<String>) {
        self.code.extend(vec![
            Opcode::Special(format!("global _start")),
            Opcode::Special(format!("section .text")),
            Opcode::Special(format!("extern {}, _concatString, _cmpString, _alloc_size", built_ins.join(", "))),
            Opcode::Label(format!("_start")),
            Opcode::Call(Target::Label(format!("main"))),
            Opcode::Mov(Target::Reg(EDI), Target::Reg(EAX)),
            Opcode::Mov(Target::Reg(EAX), Target::Imm(60)),
            Opcode::Special(format!("syscall")),
        ]);
    }

    fn add_vtable(&mut self, name: String,  methods: &Vec<String>) {
        let mut vtable = String::new();
        vtable.push_str(&name);
        vtable.push_str(" dq ");
        for m in methods {
            vtable.push_str(m);
            vtable.push_str(", ")
        }
        self.code.push(Opcode::Special(vtable));
    }

    fn add_function_args_and_locals(&mut self, locals: &Vec<Reg>, args: &Vec<Reg>) {
        for (i, reg) in locals.into_iter().enumerate() {
            self.memory.insert(reg.clone(), Memory::new(-(1 + i as i32)));
        }
        for (i, reg) in args.iter().enumerate() {
            self.memory.insert(reg.clone(), Memory::new((i + 2) as i32));
        }
    }

    fn transform_fun_start(&mut self, function: &Label, label: &Label, locals_num: usize) {
        self.code.push(Opcode::Label(function.clone()));
        self.code.push(Opcode::Push(Target::Reg(EBP)));
        self.code.push(Opcode::Mov(Target::Reg(EBP), Target::Reg(ESP)));
        self.code.push(Opcode::Sub(Target::Reg(ESP), Target::Imm(8 * locals_num as i32)));
        self.code.push(Opcode::Jmp(label.clone()));
    }

    fn transform_fun_end(&mut self, locals_num: usize) {
        let label = self.end_label.take().unwrap();
        self.code.push(Opcode::Label(label));
        if locals_num > 0 {
            self.code.push(Opcode::Add(Target::Reg(ESP), Target::Imm((locals_num * 8) as i32)));
        }
        self.code.push(Opcode::Pop(EBP));
        self.code.push(Opcode::Ret);
    }

    fn transform_add_static_strings(&mut self, graph: &ControlFlowGraph) {
        self.code.push(Opcode::Special(format!("section .data")));
        for (id, label) in self.strings.iter() {
            if let Some(s) = graph.strings.get(id) {
                self.code.push(Opcode::Special(format!("{} db '{}',0", label, s)));
            } else {
                unreachable!("Unknown string");
            }
        }
    }

    fn remove_one_line_jumps(&mut self) -> Vec<Opcode> {
        self.code.push(Opcode::Special("placeholder".to_string()));
        let returned = self.code.drain(..);
        returned.as_slice().windows(2).filter_map(|f| match (f[0].clone(), f[1].clone()) {
            (Opcode::Jmp(a), Opcode::Label(b)) if a == b => None,
            (a, _) => Some(a)
        }).collect()
    }


    fn transform_block(&mut self, block: &SimpleBlock) {
        self.used = block.code.iter().map(|i| i.2.1.clone()).collect();

        for Instruction(instr, (_, outs), _) in block.code.iter() {
            for (_, r) in self.all_registers.iter() {
                assert_ne!(*r, Reserved, );
            }
            self.outs = outs.clone();

            match instr {
                Instr::Cast(ret, v) => {
                    self.mark_free_value(ret);
                    let v_target = self.get_target(v);
                    self.put_into_target(ret, &v_target);
                }
                Instr::Insert(obj, offset, v) => {
                    let obj_addr = self.get_reg_target(obj);
                    let v_target = self.get_reg_target(v);
                    let lea_memory = Memory::from_target(&obj_addr, *offset as i32);
                    self.code.push(Opcode::Mov(Target::Memory(lea_memory), v_target.clone()));

                    self.commit_register(&obj_addr);
                    self.commit_register(&v_target);
                }
                Instr::Extract(ret, obj, offset) => {
                    let obj_addr = self.get_reg_target(obj);
                    let f_reg = self.get_free_register();
                    let f_target = Target::Reg(f_reg.clone());
                    let lea_memory = Memory::from_target(&obj_addr, *offset as i32);
                    self.code.push(Opcode::Mov(f_target.clone(), Target::Memory(lea_memory)));

                    self.mark_free_value(ret);
                    self.put_into_target(ret, &f_target);
                    self.commit_register(&obj_addr);
                    self.commit_register(&f_target);
                }
                Instr::Asg2(ret, a, BinOp::Concat, b) => {
                    let b_target = self.get_reg_target(b);
                    self.code.push(Opcode::Push(b_target));
                    let a_target = self.get_reg_target(a);
                    self.code.push(Opcode::Push(a_target));
                    self.dump_all(Some(ret));
                    self.code.push(Opcode::Call(Target::Label("_concatString".to_string())));
                    self.code.push(Opcode::Add(Target::Reg(ESP), Target::Imm(8 * 2 as i32)));

                    self.mark_free_value(ret);
                    self.put_into_target(ret, &Target::Reg(EAX));
                    self.commit_register(&Target::Reg(EAX));
                }
                Instr::Asg2(ret, a, op @ BinOp::Add, b) |
                Instr::Asg2(ret, a, op @ BinOp::Sub, b) |
                Instr::Asg2(ret, a, op @ BinOp::Mul, b) => {
                    let a_target = self.get_reg_target(a);
                    self.make_copy_if_needed(&a_target);
                    let b_target = self.get_target(b);
                    // USE_REGISTERS: let b_target = self.get_reg_target(b);
                    self.code.push(Opcode::from_op(op, a_target.clone(), b_target.clone()));

                    self.mark_free_target(&a_target);
                    self.mark_free_value(ret);
                    self.commit_register(&b_target);
                    self.put_into_target(ret, &a_target);
                    self.commit_register(&a_target);
                }
                Instr::Asg2(ret, a, op, b) => {
                    let a_target = self.get_target(a);
                    let eax_target = Target::Reg(EAX);
                    if a_target != eax_target {
                        self.make_free(&eax_target);
                        self.move_value(a_target.clone(), eax_target.clone());
                        self.all_registers.insert(EAX, Reserved);
                    }
                    self.make_copy_if_needed(&eax_target);

                    let edx_target = Target::Reg(EDX);
                    self.make_free(&edx_target);
                    self.all_registers.insert(EDX, Reserved);

                    self.code.push(Opcode::Special("cqo".to_string()));
                    let b_target = self.get_reg_target(b);
                    self.code.push(Opcode::Div(b_target.clone()));

                    self.mark_free_target(&eax_target);
                    self.mark_free_target(&edx_target);
                    self.mark_free_value(ret);
                    self.commit_register(&a_target);
                    self.commit_register(&b_target);
                    match op {
                        BinOp::Div => {
                            self.put_into_target(ret, &eax_target);
                            self.commit_register(&eax_target);
                        }
                        BinOp::Mod => {
                            self.put_into_target(ret, &edx_target);
                            self.commit_register(&edx_target);
                        }
                        _ => unreachable!(),
                    }
                }
                Instr::Asg1(r, o, v) => {
                    let v_target = self.get_reg_target(v);
                    self.make_copy_if_needed(&v_target);
                    match o {
                        UnOp::IntNeg => self.code.push(Opcode::Neg(v_target.clone())),
                        UnOp::Incr => self.code.push(Opcode::Add(v_target.clone(), Target::Imm(1))),
                        UnOp::Decr => self.code.push(Opcode::Sub(v_target.clone(), Target::Imm(1))),
                    }
                    self.mark_free_target(&v_target);
                    self.mark_free_value(r);
                    self.put_into_target(r, &v_target);
                    self.commit_register(&v_target);
                }
                Instr::Copy(t, v) => {
                    let v_target = self.get_reg_target(v);
                    self.mark_free_value(t);
                    self.put_into_target(t, &v_target);
                    self.commit_register(&v_target);
                }
                Instr::Jump(l) => {
                    self.dump_all(None);
                    self.code.push(Opcode::Jmp(l.clone()))
                }
                Instr::If(If(x, RelOp::CMP, y, t, f)) => {
                    let y_target = self.get_reg_target(y);
                    self.code.push(Opcode::Push(y_target.clone()));
                    let x_target = self.get_reg_target(x);
                    self.code.push(Opcode::Push(x_target.clone()));
                    self.commit_register(&x_target);
                    self.commit_register(&y_target);

                    self.dump_all(None);
                    self.code.push(Opcode::Call(Target::Label("_cmpString".to_string())));
                    self.code.push(Opcode::Add(Target::Reg(ESP), Target::Imm(8 * 2 as i32)));
                    self.code.push(Opcode::Cmp(Target::Reg(EAX).clone(), Target::Imm(1)));
                    self.code.push(Opcode::Je(t.clone()));
                    self.code.push(Opcode::Jmp(f.clone()));
                }
                Instr::If(If(x, o, y, t, f)) => {
                    let x_target = self.get_reg_target(x);
                    let y_target = self.get_target(y);
                    // USE_REGISTERS: let y_target = self.get_reg_target(y);
                    self.code.push(Opcode::Cmp(x_target.clone(), y_target.clone()));
                    self.commit_register(&x_target);
                    self.commit_register(&y_target);
                    self.dump_all(None);
                    match o {
                        RelOp::LT => self.code.push(Opcode::Jlt(t.clone())),
                        RelOp::LE => self.code.push(Opcode::Jle(t.clone())),
                        RelOp::GT => self.code.push(Opcode::Jgt(t.clone())),
                        RelOp::GE => self.code.push(Opcode::Jge(t.clone())),
                        RelOp::EQ => self.code.push(Opcode::Je(t.clone())),
                        RelOp::NE => self.code.push(Opcode::Jne(t.clone())),
                        _ => unreachable!(),
                    };
                    self.code.push(Opcode::Jmp(f.clone()));
                }
                Instr::Call(ret, label, args) => {
                    for arg in args.clone().iter().rev() { // add args
                        let a_target = self.get_reg_target(arg);
                        self.code.push(Opcode::Push(a_target.clone()));
                        self.commit_register(&a_target);
                    }
                    self.dump_all(Some(ret));
                    self.code.push(Opcode::Call(Target::Label(label.clone())));
                    if args.len() > 0 { // remove args
                        self.code.push(Opcode::Add(Target::Reg(ESP), Target::Imm(8 * args.len() as i32)))
                    }
                    if ret.itype != IType::Void {
                        self.mark_free_value(ret);
                        self.put_into_target(ret, &Target::Reg(EAX));
                        self.commit_register(&Target::Reg(EAX));
                    }
                }
                Instr::CallM(ret, obj, num, args) => {
                    for arg in args.clone().iter().rev() { // add args
                        let a_target = self.get_reg_target(arg);
                        self.code.push(Opcode::Push(a_target.clone()));
                        self.commit_register(&a_target);
                    }
                    let obj_addr = self.get_reg_target(obj);
                    let f_reg = self.get_free_register();
                    let f_target = Target::Reg(f_reg.clone());
                    let lea_memory = Memory::from_target(&obj_addr, 0);
                    self.code.push(Opcode::Mov(f_target.clone(), Target::Memory(lea_memory))); //vtable
                    let call_memory = Memory::from_target(&f_target, *num as  i32);
                    self.code.push(Opcode::Mov(f_target.clone(), Target::Memory(call_memory)));

                    self.dump_all(Some(ret));
                    self.code.push(Opcode::Call(f_target.clone()));
                    if args.len() > 0 { // remove args
                        self.code.push(Opcode::Add(Target::Reg(ESP), Target::Imm(8 * args.len() as i32)))
                    }
                    if ret.itype != IType::Void {
                        self.mark_free_value(ret);
                        self.put_into_target(ret, &Target::Reg(EAX));
                        self.commit_register(&Target::Reg(EAX));
                    }

                    self.mark_free_value(ret);
                    self.put_into_target(ret, &f_target);
                    self.commit_register(&obj_addr);
                    self.commit_register(&f_target);
                }
                Instr::Return(v) => {
                    let v_target = self.get_reg_target(v);
                    match &v_target {
                        Target::Reg(r) if *r == EAX => {}
                        _ => {
                            self.move_value(v_target.clone(), Target::Reg(EAX));
                        }
                    }
                    self.mark_free_target(&v_target);
                    self.code.push(Opcode::Jmp(self.get_final_label()));
                }
                Instr::VReturn => {
                    self.code.push(Opcode::Jmp(self.get_final_label()));
                }
            };
            self.used.pop_front();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::model::assembler::Register::*;
    use crate::model::ast::IType;

    use super::*;

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
        assembler_transformer.outs.extend(vars.iter().cloned());
        assembler_transformer.used.push_back(HashSet::from_iter(vec![vars[0].clone()]));

        i = 0;
        for v in vars[..3].iter() {
            assembler_transformer.put_into_target(v, &Target::Memory(Memory::new(i + 4)));
            i += 4;
        }
        assembler_transformer.put_into_target(&vars[0], &Target::Reg(regs[0].clone()));
        assembler_transformer.put_into_target(&vars[3], &Target::Reg(regs[1].clone()));

        assembler_transformer.mark_free_target(&Target::Reg(regs[0].clone()));
        assembler_transformer.put_into_target(&vars[0], &Target::Reg(regs[0].clone()));

        let reg = assembler_transformer.get_free_register();

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
        assembler_transformer.outs.extend(vars.iter().cloned());
        assembler_transformer.used.push_back(HashSet::from_iter(vars[..3].iter().cloned()));

        i = 0;
        for (v, r) in vars[..3].iter().zip(regs.iter()) {
            assembler_transformer.put_into_target(v, &Target::Reg(r.clone()));
            assembler_transformer.put_into_target(v, &Target::Memory(Memory::new(i + 4)));
            i += 4;
        }
        assembler_transformer.put_into_target(&vars[3], &Target::Reg(regs[3].clone()));

        for (v, r) in vars[..3].iter().zip(regs[..3].iter()) {
            assembler_transformer.mark_free_target(&Target::Reg(r.clone()));
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

        assembler_transformer.outs = HashSet::from_iter(vars.iter().cloned());

        let mut i = 0;
        for v in vars.iter() {
            assembler_transformer.memory.insert(v.clone(), Memory::new(i));
            i += 4;
        }

        for (v, r) in vars[..3].iter().zip(regs.iter()) {
            assembler_transformer.put_into_target(v, &Target::Reg(r.clone()));
        }
        for (v, r) in vars[1..3].iter().zip(regs.iter()) {
            assembler_transformer.mark_free_target(&Target::Reg(r.clone()));
            assembler_transformer.put_into_target(v, &Target::Reg(r.clone()));
        }

        let reg = assembler_transformer.get_free_register();
        assert_eq!(reg, regs[3])
    }
}