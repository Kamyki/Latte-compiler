use std::collections::{HashMap, HashSet};
use crate::model::ast::{Program, Id, Type, TopDef, Span, IType, Function, Class};
use crate::error_handling::FrontendError::{DoubleDeclaration, FunctionCall, MismatchedTypes, WrongExtend, CircularClassHierarchy, MissingMain};
use crate::error_handling::{CheckerResult, AccErrors};

pub struct GlobalAnalyser {
    pub functions: HashMap<String, FunctionSignature>,
    pub classes: HashMap<String, ClassSignature>,
    pub strings: HashMap<String, u32>,
}

impl GlobalAnalyser {
    pub fn new() -> Self {
        Self { functions: HashMap::new(), classes: HashMap::new(), strings: HashMap::new() }
    }

    fn insert_function(&mut self, id: Id, fs: FunctionSignature) -> CheckerResult<()> {
        let span = id.span;
        match self.functions.insert(id.item, fs) {
            None => Ok(()),
            Some(_) => Err(DoubleDeclaration.add_done(span, "There is another function with that name")),
        }
    }

    fn insert_class(&mut self, id: Id, cs: ClassSignature) -> CheckerResult<()> {
        let span = id.span;
        match self.classes.insert(id.item, cs) {
            None => Ok(()),
            Some(_) => Err(DoubleDeclaration.add_done(span, "There is another class with that name")),
        }
    }
}

impl GlobalAnalyser {
    pub fn check(&mut self, ast: &Program) -> CheckerResult<()> {
        self.functions.extend(builtin_function());
        self.add_function_names(ast)
            .and(self.check_main())
            .and(self.add_class_names(ast))
            .and(self.check_super())
    }

    fn add_function_names(&mut self, ast: &Program) -> CheckerResult<()> {
        ast.defs.iter()
            .map(|top_def| match top_def {
                TopDef::Function(f) => self.insert_function(f.id.clone(), f.into()),
                TopDef::Class(_) => Ok(())
            })
            .acc()
    }

    fn add_class_names(&mut self, ast: &Program) -> CheckerResult<()> {
        ast.defs.iter()
            .map(|top_def| match top_def {
                TopDef::Function(_) => Ok(()),
                TopDef::Class(c) => self.insert_class(c.id.clone(), c.into())
            })
            .acc()
    }

    fn check_super(&mut self) -> CheckerResult<()> {
        let mut errors = vec![];
        let mut discovered = HashSet::new();
        let mut departure = HashMap::new();
        let mut time = 0;

        for class in self.classes.values() {
            if let Some(super_class) = &class.super_class {
                if !self.classes.contains_key(&super_class.item) {
                    errors.push(Err(WrongExtend.add_done(super_class.span, "Extending not existing class")))
                }
            }
        }
        if !errors.is_empty() {
            return errors.acc();
        }

        for name in self.classes.keys() {
            if !discovered.contains(name) {
                dfs(&self.classes, name, &mut discovered, &mut departure, &mut time);
            }
        }

        for (name, class) in &self.classes {
            if let Some(super_class) = &class.super_class {
                if departure[&super_class.item] >= departure[name] {
                    let mut err = CircularClassHierarchy.add(super_class.span, "Detected circular class hierarchy from here");
                    let mut super_super_class = &super_class.item;
                    let mut nr = 1;
                    while name != super_super_class {
                        err = err.add(self.classes[super_super_class].type_name.span, format!("Then this {}", nr));
                        super_super_class = &self.classes[super_super_class].super_class.as_ref().unwrap().item;
                        nr += 1;
                    }
                    err = err.add(class.type_name.span, "And back to beginning");
                    errors.push(Err(err.done()))
                }
            }
        }
        errors.acc()
    }

    fn check_main(&self) -> CheckerResult<()> {
        if let Some(x) = self.functions.get("main") {
            if x.ret_type.item == IType::Int && x.args.is_empty() {
                Ok(())
            } else {
                Err(MissingMain.add_done(x.span, "Wrong signature of function main"))
            }
        } else {
            Err(MissingMain.add_done(OUTSIDE_SPAN, "Couldn't find main"))
        }
    }

    pub fn extends(&self, cs: &ClassSignature, t: &IType) -> bool {
        if let Some(s) = &cs.super_class {
            if let Some(super_class) = self.classes.get(&s.item) {
                return if super_class.type_name.item == *t {
                    true
                } else {
                    self.extends(super_class, t)
                };
            }
        }
        false
    }
}

fn dfs(graph: &HashMap<String, ClassSignature>, v: &String, discovered: &mut HashSet<String>, departure: &mut HashMap<String, u32>, time: &mut u32) {
    discovered.insert(v.clone());

    if let Some(u) = &graph[v].super_class {
        if !discovered.contains(&u.item) {
            dfs(graph, &u.item, discovered, departure, time);
        }
    }
    departure.insert(v.clone(), *time);
    *time += 1;
}

#[derive(Debug)]
pub struct ClassSignature {
    pub type_name: Type,
    pub fields: HashMap<String, (usize, Type)>,
    pub methods: HashMap<String, (usize, FunctionSignature)>,
    pub super_class: Option<Id>,
}

impl From<&Class> for ClassSignature {
    fn from(class: &Class) -> Self {
        let mut cs = Self {
            fields: HashMap::new(),
            methods: HashMap::new(),
            super_class: class.super_class.clone(),
            type_name: Type { item: IType::Class(class.id.item.clone()), span: class.id.span },
        };
        let mut errs = vec![];
        for (idx, field) in class.fields.iter().enumerate() {
            let span = field.1.span;
            let e = match cs.fields.insert(field.1.item.clone(), (idx +1, field.0.clone())) {
                None => Ok(()),
                Some(_) => Err(DoubleDeclaration.add_done(span, "There is another field with that name")),
            };
            errs.push(e)
        }
        for (idx, method) in class.methods.iter().enumerate() {
            let span = method.id.span;
            let fs = FunctionSignature::from(method);
            let e = match cs.methods.insert(method.id.item.clone(), (idx, fs)) {
                None => Ok(()),
                Some(_) => Err(DoubleDeclaration.add_done(span, "There is another method with that name")),
            };
            errs.push(e)
        }
        cs
    }
}

impl ClassSignature {
    pub fn find_var(&self, id: &Id) -> Option<&Type> {
        self.fields.get(id.item.as_str()).map(|v| &v.1)
    }

    pub fn find_method(&self, id: &Id) -> Option<&FunctionSignature> {
        self.methods.get(id.item.as_str()).map(|v| &v.1)
    }

    pub fn method_num(&self, id: &Id) -> usize {
        *self.methods.get(id.item.as_str()).map(|v| &v.0).unwrap()
    }

    pub fn var_num(&self, id: &Id) -> usize {
        *self.fields.get(id.item.as_str()).map(|v| &v.0).unwrap()
    }
}

#[derive(Debug)]
pub struct FunctionSignature {
    pub span: Span,
    pub ret_type: Type,
    pub args: Vec<Type>,
}

impl From<&Function> for FunctionSignature {
    fn from(fun: &Function) -> Self {
        Self {
            span: fun.id.span,
            ret_type: fun.ret_type.clone(),
            args: fun.args.iter().map(|x| x.0.clone()).collect(),
        }
    }
}

impl FunctionSignature {
    pub fn check_call(&self, arg_types: Vec<Type>, span: Span) -> CheckerResult<()> {
        if self.args.len() != arg_types.len() {
            return Err(FunctionCall.add(span, "Wrong number of arguments")
                .add(self.span, "Function definition is here")
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

const OUTSIDE_SPAN: Span = (1, 0);

fn builtin_function() -> HashMap<String, FunctionSignature> {
    let mut map = HashMap::new();

    map.insert("printInt".to_string(), FunctionSignature {
        span: OUTSIDE_SPAN,
        ret_type: Type { item: IType::Void, span: OUTSIDE_SPAN },
        args: vec![Type { item: IType::Int, span: OUTSIDE_SPAN }],
    });
    map.insert("printString".to_string(), FunctionSignature {
        span: OUTSIDE_SPAN,
        ret_type: Type { item: IType::Void, span: OUTSIDE_SPAN },
        args: vec![Type { item: IType::String, span: OUTSIDE_SPAN }],
    });
    map.insert("error".to_string(), FunctionSignature {
        span: OUTSIDE_SPAN,
        ret_type: Type { item: IType::Void, span: OUTSIDE_SPAN },
        args: vec![],
    });
    map.insert("readInt".to_string(), FunctionSignature {
        span: OUTSIDE_SPAN,
        ret_type: Type { item: IType::Int, span: OUTSIDE_SPAN },
        args: vec![],
    });
    map.insert("readString".to_string(), FunctionSignature {
        span: OUTSIDE_SPAN,
        ret_type: Type { item: IType::String, span: OUTSIDE_SPAN },
        args: vec![],
    });
    map
}