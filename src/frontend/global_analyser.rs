use std::collections::HashMap;
use crate::model::ast::{Program, Id, Type, TopDef, Span, IType};
use crate::error_handling::FrontendError::{DoubleDeclaration, FunctionCall, MismatchedTypes};
use crate::error_handling::{CheckerResult, AccErrors};

pub struct GlobalAnalyser<'a> {
    pub functions: HashMap<String, FunctionSignature>,
    ast: &'a Program,
}

impl<'a> GlobalAnalyser<'a> {
    pub fn new(ast: &'a Program) -> Self {
        Self { functions: HashMap::new(), ast }
    }

    fn insert_function(&mut self, id: Id, fs: FunctionSignature) -> CheckerResult<()> {
        let span = id.span;
        match self.functions.insert(id.item, fs) {
            None => Ok(()),
            Some(_) => Err(DoubleDeclaration.add_done(span, "There is another function with that name")),
        }
    }
}

impl<'a> GlobalAnalyser<'a> {
    pub fn check(&mut self) -> CheckerResult<()> {
        self.functions.extend(builtin_function());
        self.add_function_names()
    }

    fn add_function_names(&mut self) -> CheckerResult<()> {
        self.ast.defs.iter()
            .map(|top_def| self.insert_function(top_def.id.clone(), top_def.into()))
            .acc()
    }
}

pub struct FunctionSignature {
    pub ret_type: Type,
    args: Vec<Type>,
}

impl From<&TopDef> for FunctionSignature {
    fn from(top_def: &TopDef) -> Self {
        Self {
            ret_type: top_def.ret_type.clone(),
            args: top_def.args.iter().map(|x| x.0.clone()).collect(),
        }
    }
}

impl FunctionSignature {
    pub fn check_call(&self, arg_types: Vec<Type>, span: Span) -> CheckerResult<()> {
        if self.args.len() != arg_types.len() {
            return Err(FunctionCall.add(span, "Wrong number of arguments")
                .add((self.ret_type.span.1, self.ret_type.span.1), "Function definition is here")
                .done());
        }
        self.args.iter().zip(arg_types).map(|(a, t)| {
            if a.item == t.item {
                Ok(())
            } else {
                Err(MismatchedTypes.add(a.span, "Expected type of argument")
                    .add(t.span, "Wrong argument type in call")
                    .done())
            }
        }).acc()
    }
}

fn builtin_function() -> HashMap<String, FunctionSignature> {
    let mut map = HashMap::new();

    map.insert("printInt".to_string(), FunctionSignature {
        ret_type: Type { item: IType::Void, span: (0, 0) },
        args: vec![Type { item: IType::Int, span: (0, 0) }],
    });
    map.insert("printString".to_string(), FunctionSignature {
        ret_type: Type { item: IType::Void, span: (0, 0) },
        args: vec![Type { item: IType::String, span: (0, 0) }],
    });
    map.insert("error".to_string(), FunctionSignature {
        ret_type: Type { item: IType::Void, span: (0, 0) },
        args: vec![],
    });
    map.insert("readInt".to_string(), FunctionSignature {
        ret_type: Type { item: IType::Int, span: (0, 0) },
        args: vec![],
    });
    map.insert("readString".to_string(), FunctionSignature {
        ret_type: Type { item: IType::String, span: (0, 0) },
        args: vec![],
    });
    map
}