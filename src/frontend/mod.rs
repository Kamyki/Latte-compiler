use crate::model::ast::{Program, TopDef};
use crate::frontend::function_analyser::{FunctionAnalyser};
use crate::error_handling::{print_errors, Code, CheckerResult, AccErrors};
use crate::frontend::simplifier::Simplifier;
use crate::frontend::global_analyser::GlobalAnalyser;

mod function_analyser;
mod simplifier;
mod global_analyser;

pub fn check_semantics(loc_map: &Code, ast: &mut Program) {
    let simplifier = Simplifier::new();

    let mut result = simplifier.simplify_expressions_in_ast(ast);

    let mut global = GlobalAnalyser::new(&ast);

    result = result.and(global.check());

    result = result.and_then(|_| {
        let mut function_analyser = FunctionAnalyser::new(global);
        check_functions(&mut function_analyser, &ast)
    });
    match result {
        Ok(_) => (),
        Err(errors) => print_errors(loc_map, errors.as_slice())
    }
}

fn check_functions(analyser: &mut FunctionAnalyser, ast: &Program) -> CheckerResult<()> {
    ast.defs.iter().map(|d| match d {
        TopDef::Function(f) => analyser.check_function(f),
        TopDef::Class(_) => Ok(())
    }).acc()
}