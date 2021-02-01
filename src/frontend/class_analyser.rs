use crate::frontend::global_analyser::GlobalAnalyser;
use crate::model::ast::{Type, Id, Class, IType};
use crate::error_handling::{CheckerResult, AccErrors};
use crate::frontend::function_analyser::FunctionAnalyser;
use crate::error_handling::FrontendError::VirtualMethod;

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
        self.check_normal_methods(top_def)
            .and(self.check_methods(top_def))
    }

    fn check_methods(&self, top_def: &Class) -> CheckerResult<()> {
        let mut fa = FunctionAnalyser::new(self.global);
        let obj_type = Type { span: top_def.span, item: IType::Class(top_def.id.item.clone()) };
        fa.insert_var(Id { span: top_def.id.span, item: "self".to_string() }, obj_type)
            .and(top_def.fields.iter().map(|field|
                fa.insert_var(field.1.clone(), field.0.clone())
            ).acc())
            .and_then(|()| top_def.methods.iter().map(|method|
                fa.check_function(method)).acc())
    }

    fn check_normal_methods(&self, class: &Class) -> CheckerResult<()> {
        let support_virtual_method = true;
        if !support_virtual_method {
            let mut cs = &self.global.classes[class.id.item.as_str()];

            while let Some(super_class) = &cs.super_id {
                let scs = &self.global.classes[super_class.item.as_str()];

                for method in class.methods.iter() {
                    if let Some(m) = scs.methods.iter().find(|(s, _, _)| s == &method.id.item) {
                        return Err(VirtualMethod.add(method.span, "Virtual method in class are not supported")
                            .add(scs.type_name.span, "Super method is in this class")
                            .add(m.1.span, "In this place")
                            .add_over_span(class.span).done());
                    }
                }
                cs = scs;
            }
        }
        Ok(())
    }
}

