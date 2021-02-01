use std::collections::{HashMap, HashSet, VecDeque};
use crate::model::ast::{Program, Id, Type, TopDef, Span, IType, Function, Class};
use crate::error_handling::FrontendError::{DoubleDeclaration, WrongExtend, CircularClassHierarchy, MissingMain, OverloadedMethod};
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

    pub fn find_class_method(&self, class: &str, method: &Id) -> Option<&FunctionSignature> {
        self.classes.get(class).and_then(|c| {
            if let Some(m) = c.find_method(method) {
                Some(m)
            } else {
                c.super_id.as_ref().and_then(|s| self.find_class_method(&s.item, method))
            }
        })
    }
}

impl GlobalAnalyser {
    pub fn check(&mut self, ast: &Program) -> CheckerResult<()> {
        self.functions.extend(builtin_function());
        let mut res = self.add_function_names(ast)
            .and(self.check_main())
            .and(self.add_class_names(ast))
            .and(self.check_super());

        let keys: Vec<String> = self.classes.keys().cloned().collect();
        for m in keys {
            res = res.and(self.check_super_methods_and_fields(&m));
        }
        res
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
            if let Some(super_class) = &class.super_id {
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
            if let Some(super_class) = &class.super_id {
                if departure[&super_class.item] >= departure[name] {
                    let mut err = CircularClassHierarchy.add(super_class.span, "Detected circular class hierarchy from here");
                    let mut super_super_class = &super_class.item;
                    let mut nr = 1;
                    while name != super_super_class {
                        err = err.add(self.classes[super_super_class].type_name.span, format!("Then this {}", nr));
                        super_super_class = &self.classes[super_super_class].super_id.as_ref().unwrap().item;
                        nr += 1;
                    }
                    err = err.add(class.type_name.span, "And back to beginning");
                    errors.push(Err(err.done()))
                }
            }
        }
        errors.acc()
    }

    fn check_super_methods_and_fields(&mut self, class_id: &str) -> CheckerResult<()> {
        let mut res = Ok(());
        if self.classes.get(class_id).unwrap().has_super_fields {
            return res
        }
        self.classes.entry(class_id.to_string()).and_modify(|v| v.has_super_fields = true);
        if let Some(super_id) =  self.classes.get(class_id).unwrap().super_id.clone() {
            let mut super_methods = vec![];
            let mut to_remove = vec![];
            let mut super_fields = vec![];
            res = res.and(self.check_super_methods_and_fields(&super_id.item));
            let super_class = self.classes.get(&super_id.item).unwrap();
            let class = self.classes.get(class_id).unwrap();
            for (m, method, sc) in super_class.methods.iter() {
                if let Some((_, class_s, c)) = class.methods.iter().find(|(s, _, _)| s == m) {
                    if !class_s.match_signature(method) {
                        res = res.and(
                            Err(OverloadedMethod.add(class_s.span, "Signature not match super method")
                                .add(method.span, "Super method")
                                .done()));
                    } else {
                        super_methods.push((m.clone(), class_s.clone(), c.clone()));
                        to_remove.push(m.clone());
                    }
                } else {
                    super_methods.push((m.clone(), method.clone(), sc.clone()));
                }
            }
            for (f, field) in super_class.fields.iter() {
                if let Some((_, field_t)) = class.fields.iter().find(|(s,_)| s == f) {
                    res = res.and(Err(DoubleDeclaration.add(field_t.span, "Double declaration of field in class")
                        .add(field.span, "super field is here")
                        .done()))
                } else {
                    super_fields.push((f.clone(), field.clone()));
                }
            }
            let methods: Vec<(String, FunctionSignature, String)> = self.classes.get(class_id).unwrap().methods.iter().cloned().collect();
            self.classes.entry(class_id.to_string()).and_modify(|v| v.methods = methods.into_iter().filter(|c| !to_remove.contains(&c.0)).collect());

            for m in super_methods.into_iter().rev() {
                self.classes.get_mut(class_id).unwrap().methods.push_front(m);
            }
            for f in super_fields.into_iter().rev() {
                self.classes.get_mut(class_id).unwrap().fields.push_front(f);
            }
        }
        res
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
        if let Some(s) = &cs.super_id {
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

    if let Some(u) = &graph[v].super_id {
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
    pub fields: VecDeque<(Id, Type)>,
    pub methods: VecDeque<(String, FunctionSignature, String)>,
    pub super_id: Option<Id>,
    pub has_super_fields: bool,
}

impl From<&Class> for ClassSignature {
    fn from(class: &Class) -> Self {
        let mut cs = Self {
            fields: VecDeque::new(),
            methods: VecDeque::new(),
            super_id: class.super_class.clone(),
            type_name: Type { item: IType::Class(class.id.item.clone()), span: class.id.span },
            has_super_fields: false
        };
        let mut errs = vec![];
        for field in class.fields.iter() {
            let span = field.1.span;
            let e = match cs.fields.iter().filter(|(f, _)| f.item == field.1.item).next() {
                None => {
                    cs.fields.push_back((field.1.clone(), field.0.clone()));
                    Ok(())
                },
                Some(_) => Err(DoubleDeclaration.add_done(span, "There is another field with that name")),
            };
            errs.push(e)
        }
        for method in class.methods.iter() {
            let span = method.id.span;
            let fs = FunctionSignature::from(method);

            let e = match cs.methods.iter().filter(|(c, _, _)| *c == method.id.item).next() {
                None => {
                    cs.methods.push_back((method.id.item.clone(), fs, class.id.item.clone()));
                    Ok(())
                }
                Some(_) => Err(DoubleDeclaration.add_done(span, "There is another method with that name")),
            };
            errs.push(e)
        }
        cs
    }
}

impl ClassSignature {
    pub fn find_var(&self, id: &Id) -> Option<&Type> {
        self.fields.iter().find(|(s, _)| s.item == id.item.as_str()).map(|v| &v.1)
    }

    pub fn find_method(&self, id: &Id) -> Option<&FunctionSignature> {
        self.methods.iter().find(|(s, _, _)| s == id.item.as_str()).map(|v| &v.1)
    }

    pub fn method_num(&self, id: &Id) -> usize {
        self.methods.iter().enumerate().find(|(_, (s, _, _))| s == id.item.as_str()).map(|v| v.0).unwrap()
    }

    pub fn var_num(&self, id: &Id) -> usize {
        self.fields.iter().enumerate().find(|(_,(s, _))| s.item == id.item.as_str()).map(|v|v.0 + 1).unwrap()
    }
}

#[derive(Debug, Clone)]
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
    fn match_signature(&self, other: &FunctionSignature) -> bool {
        self.ret_type.item == other.ret_type.item &&
            self.args.len() == other.args.len() &&
            self.args.iter().zip(&other.args).fold(true, |v, (t1, t2)| { v && t1.item == t2.item })
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