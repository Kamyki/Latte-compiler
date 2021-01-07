use core::fmt;

use serde::export::fmt::Display;
use serde::export::Formatter;

use crate::model::assembler::Register::*;
use crate::model::quadruple_code::Label;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Register {
    EAX,
    EBX,
    ECX,
    EDX,
    ESP,
    EBP,
    ESI,
    EDI,
    R8D,
    R9D,
    R10D,
    R11D,
    R12D,
    R13D,
    R14D,
    R15D,
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let r = match self {
            Register::EAX => "rax",
            Register::EBX => "rbx",
            Register::ECX => "rcx",
            Register::EDX => "rdx",
            Register::ESP => "rsp",
            Register::EBP => "rbp",
            Register::ESI => "rsi",
            Register::EDI => "rdi",
            Register::R8D => "r8",
            Register::R9D => "r9",
            Register::R10D => "r10",
            Register::R11D => "r11",
            Register::R12D => "r12",
            Register::R13D => "r13",
            Register::R14D => "r14",
            Register::R15D => "r15",
        };
        write!(f, "{}", r)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Memory {
    pub base: Option<Register>,
    pub offset: Option<(Register, u32)>,
    pub displacement: i32,
}

impl Memory {
    pub fn new(displacement: i32) -> Self {
        Self {
            base: Some(EBP),
            offset: None,
            displacement
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Target {
    Reg(Register),
    Imm(i32),
    Memory(Memory),
    Label(String),
}

impl Display for Target {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Target::Reg(r) => write!(f, "{}", r),
            Target::Imm(i) => write!(f, "{}", i),
            Target::Memory(Memory { base, offset, displacement }) => {
                let mut result = write!(f, "[");
                let mut v = vec![];
                if let Some(r) = base {
                    v.push(format!("{}", r))
                }
                if let Some((r, s)) = offset {
                    v.push(format!("+ {} * {}", r, s));
                }
                if *displacement > 0 {
                    v.push(format!("+ {}", displacement * 8));
                } else if *displacement < 0 {
                    v.push(format!("- {}", -displacement * 8));
                }
                result = result.and(write!(f, "{}", v.join(" ")));
                result.and(write!(f, "]"))
            }
            Target::Label(l) => write!(f, "{}", l)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Opcode {
    Cmp(Target, Target),
    Mov(Target, Target),

    Add(Target, Target),
    Mul(Target, Target),
    Sub(Target, Target),
    Div(Target, Target),

    Jmp(Label),
    Je(Label),
    Jne(Label),
    Jlt(Label),
    Jle(Label),
    Jgt(Label),
    Jge(Label),
    Label(Label),

    Push(Target),
    Pop(Register),
    Call(Label),
    Xchng(Target, Target),
    Ret,
    Special(String),
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Opcode::Cmp(a,b) => write!(f, "cmp {}, {}", a, b),
            Opcode::Mov(a, b) => write!(f, "mov {}, {}", a, b),
            Opcode::Add(a, b) => write!(f, "add {}, {} ", a, b),
            Opcode::Mul(a, b) => write!(f, "mul {}, {}", a, b),
            Opcode::Sub(a, b) => write!(f, "sub {}, {}", a, b),
            Opcode::Div(a, b) => write!{f, "div {}, {}", a, b},
            Opcode::Jmp(l) => write!(f, "jmp {}", l),
            Opcode::Je(l) => write!(f, "je {}", l),
            Opcode::Jne(l) => write!(f, "jne {}", l),
            Opcode::Jlt(l) => write!(f, "jlt {}", l),
            Opcode::Jle(l) => write!(f, "jle {}", l),
            Opcode::Jgt(l) => write!(f, "jgt {}", l),
            Opcode::Jge(l) => write!(f, "jge {}", l),
            Opcode::Label(l) => write!(f, "{}:", l),
            Opcode::Push(r) => write!(f, "push {}", r),
            Opcode::Pop(r) => write!(f, "pop {}", r),
            Opcode::Call(l) => write!(f, "call {}", l),
            Opcode::Xchng(a, b) => write!(f, "xchmg {} {}", a, b),
            Opcode::Ret => write!(f, "ret"),
            Opcode::Special(s) => write!(f, "{}", s),
        }
    }
}