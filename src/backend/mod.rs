use crate::model::ast::Program;
use crate::error_handling::CheckerResult;
use crate::backend::quadruple_code_transformer::QuadrupleCodeTransformer;
use crate::model::quadruple_code::ControlFlowGraph;
use crate::frontend::Maps;
use crate::backend::assembler_transformer::AssemblerTransformer;
use std::collections::HashSet;
use std::iter::FromIterator;
use crate::model::assembler::Register::*;
use crate::model::assembler::Opcode;

mod quadruple_code_transformer;
mod assembler_transformer;


pub fn transform(maps: Maps, ast: &mut Program) -> CheckerResult<(ControlFlowGraph, Vec<Opcode>)> {
    let mut transformer = QuadrupleCodeTransformer::new(maps);

    let result = transformer.transform(ast);
    let all_registers = HashSet::from_iter(vec![EAX, EBX, ECX, EDX, EBP, ESI, EDI, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D, ]);
    let mut assembler_transformer = AssemblerTransformer::new(all_registers);
    let code= assembler_transformer.transform(&result);
    Ok((result, code))
}