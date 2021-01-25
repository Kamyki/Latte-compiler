use crate::model::ast::Program;
use crate::error_handling::CheckerResult;
use crate::backend::quadruple_code_transformer::QuadrupleCodeTransformer;
use crate::model::quadruple_code::{ControlFlowGraph, Label};
use crate::frontend::Maps;
use crate::backend::assembler_transformer::AssemblerTransformer;
use std::collections::HashSet;
use std::iter::FromIterator;
use crate::model::assembler::Register::*;
use crate::model::assembler::Opcode;
use crate::backend::lcse_optimizer::LCSEOptimizer;

mod quadruple_code_transformer;
mod assembler_transformer;
mod lcse_optimizer;


pub fn transform(maps: Maps, ast: &mut Program) -> CheckerResult<(ControlFlowGraph, Vec<Opcode>)> {
    let lcse = true;
    let live = true;

    let mut transformer = QuadrupleCodeTransformer::new(maps);

    // println!("{:?}", ast);
    let mut graph = transformer.transform(ast);

    let mut lcse_optimizer = LCSEOptimizer::new();

    if lcse {
        lcse_optimizer.optimize(&mut graph);
    }

    if live {
        liveliness_analysis(&mut graph);
    }
    // println!("functions: {:?}", graph.functions);
    // println!("open_blocks: {:?}", graph.current_block);

    // for (l, b) in graph.iter() {
    //     println!("{:?}", l);
    //     println!("jumps: {:?}", b.jumps);
    //     println!("ins: {:?}", b.ins);
    //     println!("kill: {:?}", b.kill);
    //     println!("used: {:?}", b.used);
    //     println!("outs: {:?}", b.out);
    //
    //     for i in &b.code {
    //         println!("{:?}", i);
    //     }
    // }

    let all_registers = HashSet::from_iter(vec![EAX, EBX, ECX, EDX, ESI, EDI, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D, ]);
    let mut assembler_transformer = AssemblerTransformer::new(all_registers);
    let code= assembler_transformer.transform(&graph);


    // println!();
    // for c in code.iter() {
    //     println!("{}", c);
    // }

    Ok((graph, code))
}

fn liveliness_analysis(graph: &mut ControlFlowGraph) {
    let keys: Vec<Label> = graph.functions.keys().cloned().collect();
    for f in keys {
        graph.compute_liveliness(&f);
    }
}