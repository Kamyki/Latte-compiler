use crate::frontend::global_analyser::GlobalAnalyser;
use crate::model::ast::{Type, Id, Class, IType};
use crate::error_handling::{CheckerResult, AccErrors};
use crate::frontend::function_analyser::FunctionAnalyser;

pub struct ClassAnalyser<'a> {
    global: &'a GlobalAnalyser,
}

impl<'a> ClassAnalyser<'a> {
    pub fn new(context: &'a GlobalAnalyser) -> Self {
        ClassAnalyser { global: context }
    }
}

impl<'a> ClassAnalyser<'a> {
    pub fn check_class(&self, top_def: &Class) -> CheckerResult<()> {
        let mut fa = FunctionAnalyser::new(self.global);
        let obj_type = Type { span: top_def.span, item: IType::Class(top_def.id.item.clone()) };
        fa.insert_var(Id { span: top_def.id.span, item: "self".to_string() }, obj_type)
            .and(top_def.fields.iter().map(|field|
                fa.insert_var(field.1.clone(), field.0.clone())
            ).acc())
            .and_then(|()| top_def.methods.iter().map(|method|
                fa.check_function(method)).acc())
    }
}

