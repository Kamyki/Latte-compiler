use crate::model::ast::{Program, TopDef};
use crate::frontend::function_analyser::{FunctionAnalyser};
use crate::error_handling::{print_errors, Code, CheckerResult, AccErrors};
use crate::frontend::simplifier::Simplifier;
use crate::frontend::global_analyser::GlobalAnalyser;
use crate::frontend::class_analyser::ClassAnalyser;

mod function_analyser;
mod simplifier;
mod global_analyser;
mod class_analyser;

pub fn check_semantics(loc_map: &Code, ast: &mut Program) {
    let simplifier = Simplifier::new();

    let mut result = simplifier.simplify_expressions_in_ast(ast);

    let mut global = GlobalAnalyser::new();

    let global_result = GlobalAnalyser::check(&mut global, ast);
    let function_analyser = FunctionAnalyser::new(&global);
    let class_analyser = ClassAnalyser::new(&global);

    result = result.and(global_result);
    result = result.and_then(|()| check_functions(&function_analyser, &ast))
        .and_then(|_| check_classes(&class_analyser, &ast));
    match result {
        Ok(_) => (),
        Err(errors) => print_errors(loc_map, errors.as_slice())
    }
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