use crate::model::ast::{Program, TopDef};
use crate::frontend::function_analyser::{FunctionAnalyser};
use crate::error_handling::{CheckerResult, AccErrors};
use crate::frontend::simplifier::Simplifier;
use crate::frontend::global_analyser::{GlobalAnalyser, FunctionSignature};
use crate::frontend::class_analyser::ClassAnalyser;
use crate::frontend::string_mapper::StringMapper;
use std::collections::HashMap;

mod function_analyser;
mod simplifier;
pub mod global_analyser;
mod class_analyser;
mod string_mapper;

pub type Maps = (HashMap<String, FunctionSignature>, HashMap<String, u32>);

pub fn check_semantics(ast: &mut Program) -> CheckerResult<Maps> {
    let simplifier = Simplifier::new();

    let mut result = simplifier.simplify_expressions_in_ast(ast);

    let mut global = GlobalAnalyser::new();

    let global_result = GlobalAnalyser::check(&mut global, ast);
    let function_analyser = FunctionAnalyser::new(&global);
    let class_analyser = ClassAnalyser::new(&global);

    let mut string_mapper = StringMapper::new();

    result = result.and(global_result);
    result = result.and_then(|()| check_functions(&function_analyser, &ast))
        .and_then(|_| check_classes(&class_analyser, &ast))
        .and_then(|_| string_mapper.add_strings(&ast));

    result.and(Ok((global.functions, string_mapper.strings)))
}

fn check_functions(analyser: &FunctionAnalyser, ast: &Program) -> CheckerResult<()> {
    ast.defs.iter().map(|d| match d {
        TopDef::Function(f) => analyser.check_function(f),
        TopDef::Class(_) => Ok(())
    }).acc()
}

fn check_classes(analyzer: &ClassAnalyser, ast: &Program) -> CheckerResult<()> {
    ast.defs.iter().map(|d| match d {
        TopDef::Function(_) => Ok(()),
        TopDef::Class(c) => analyzer.check_class(c),
    }).acc()
}