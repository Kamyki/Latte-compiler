use std::collections::HashMap;

use crate::backend::quadruple_code_transformer::QuadrupleCodeTransformer::Root;
use crate::frontend::global_analyser::{ClassSignature, FunctionSignature};
use crate::frontend::Maps;
use crate::model::ast::{Block, Class, Expr, Field, Function, IBinOp, Id, IExpr, IStmt, Item, IType, Program, Target, TopDef, UnOp as Unary};
use crate::model::quadruple_code::{BinOp, ControlFlowGraph, If, Instr, Label, Reg, RelOp, UnOp, Value};
use crate::model::quadruple_code::Instr::Jump;
use crate::model::quadruple_code::RelOp::EQ;


pub enum QuadrupleCodeTransformer<'a> {
    Root {
        label_num: u32,
        env: HashMap<String, (Reg, Option<(IType, usize)>)>,
        strings: HashMap<String, u32>,
        functions: HashMap<Label, FunctionSignature>,
        classes: HashMap<Label, ClassSignature>,
        num: u32,
    },
    Frame {
        parent: &'a QuadrupleCodeTransformer<'a>,
        root: &'a QuadrupleCodeTransformer<'a>,
        env: HashMap<String, (Reg, Option<(IType, usize)>)>,
        num: u32,
        label_num: u32,
        current_name: String,
    },
}

impl<'a> QuadrupleCodeTransformer<'a> {
    fn new_label(&mut self) -> Label {
        let mut label = match self {
            QuadrupleCodeTransformer::Root { label_num, .. } => {
                *label_num += 1;
                format!("._{}", label_num)
            }
            QuadrupleCodeTransformer::Frame { label_num, num, current_name, .. } => {
                *label_num += 1;
                format!("._{}_{}_{}", current_name, label_num, num)
            }
        };
        let mut a = &*self;
        while let QuadrupleCodeTransformer::Frame { num, parent, .. } = a {
            label = format!("{}_{}", label, num);
            a = parent;
        }
        if let QuadrupleCodeTransformer::Root { num, .. } = a {
            format!("{}_{}", label, num)
        } else {
            label
        }
    }

    fn obj_self() -> Id {
        Id { item: format!("self"), span: (0, 0) }
    }

    fn find_function(&self, id: &str) -> &FunctionSignature {
        match self {
            QuadrupleCodeTransformer::Root { functions, .. } => functions.get(id).unwrap(),
            QuadrupleCodeTransformer::Frame { root, .. } => root.find_function(id),
        }
    }

    fn find_class(&self, id: &str) -> &ClassSignature {
        match self {
            QuadrupleCodeTransformer::Root { classes, .. } => classes.get(id).unwrap(),
            QuadrupleCodeTransformer::Frame { root, .. } => root.find_class(id),
        }
    }

    pub fn find_string(&self, str: &str) -> u32 {
        match self {
            QuadrupleCodeTransformer::Root { strings, .. } => strings.get(str).unwrap().clone(),
            QuadrupleCodeTransformer::Frame { root, .. } => root.find_string(str)
        }
    }

    pub fn insert_var(&mut self, id: Id, reg: Reg, field_num: Option<(IType, usize)>) {
        match self {
            QuadrupleCodeTransformer::Root { env, .. } => env.insert(id.item, (reg, field_num)),
            QuadrupleCodeTransformer::Frame { env, .. } => env.insert(id.item, (reg, field_num)),
        };
    }

    fn find_var(&self, id: &Id) -> (Reg, Option<(IType, usize)>) {
        match self {
            QuadrupleCodeTransformer::Root { env, .. } => env.get(id.item.as_str()).unwrap().clone(),
            QuadrupleCodeTransformer::Frame { parent, env, .. } => {
                match env.get(id.item.as_str()) {
                    None => parent.find_var(id),
                    Some(a) => a.clone(),
                }
            }
        }
    }
}

impl<'a> QuadrupleCodeTransformer<'a> {
    pub fn new(maps: Maps) -> Self {
        Root {
            label_num: 0,
            env: HashMap::new(),
            strings: maps.1,
            functions: maps.0,
            classes: maps.2,
            num: 0,
        }
    }

    pub fn new_frame(parent: &'a QuadrupleCodeTransformer<'a>, new_name: Option<String>) -> Self {
        let (root, current_name) = match parent {
            QuadrupleCodeTransformer::Root { .. } => {
                //*num+=1;
                (parent, new_name.unwrap())
            }
            QuadrupleCodeTransformer::Frame { root, current_name, .. } => {
                //*num+=1;
                (*root, new_name.unwrap_or(current_name.clone()))
            }
        };
        QuadrupleCodeTransformer::Frame { parent, root, env: HashMap::new(), num: 0, label_num: 0, current_name }
    }

    fn inc_num(&mut self) {
        match self {
            QuadrupleCodeTransformer::Root { num, .. } => *num += 1,
            QuadrupleCodeTransformer::Frame { num, .. } => *num += 1,
        }
    }

    fn add_strings(&self, graph: &mut ControlFlowGraph) {
        match self {
            QuadrupleCodeTransformer::Root { strings, .. } => {
                graph.strings.extend(strings.iter().map(|(s, i)| (*i, s.clone())))
            }
            QuadrupleCodeTransformer::Frame { root, .. } => root.add_strings(graph),
        }
    }

    pub fn transform(&mut self, ast: &Program) -> ControlFlowGraph {
        let mut graph = ControlFlowGraph::new();

        for def in &ast.defs {
            match def {
                TopDef::Function(f) => self.transform_function(&mut graph, f),
                TopDef::Class(c) => self.transform_class(&mut graph, c),
            }
        }
        for builtin in ["printInt", "printString", "error", "readInt", "readString"].iter() {
            let args_num = self.find_function(builtin).args.len();
            graph.builtin.insert(builtin.to_string(), (builtin.to_string(), args_num as u32));
        }
        assert_eq!(graph.current_block.len(), 0);
        self.add_strings(&mut graph);
        graph
    }

    fn transform_class(&mut self, graph: &mut ControlFlowGraph, c: &Class) {
        self.inc_num();
        let mut class_transformer = QuadrupleCodeTransformer::new_frame(self, Some(c.id.item.clone()));

        // let mut supers = vec![self.find_class(&c.id.item)];
        // while let Some(c2) = &c.super_class {
        //     supers.push(self.find_class(&c2.item));
        //     c =
        // }
        let cs = self.find_class(&c.id.item);

        let mut fields = vec![];
        let self_obj = Reg::new(IType::Class(c.id.item.clone()), class_transformer.new_label());
        for (field_num, field) in cs.fields.iter().enumerate() {
            class_transformer.insert_var(field.0.clone(), self_obj.clone(), Some((field.1.item.clone(), field_num + 1)));
            fields.push(     Reg::new(field.1.item.clone(), class_transformer.new_label()));
        }
        let mut ctable = vec![];

        for (method, _, class_name) in cs.methods.iter() {
            let method_mangled_name = format!("_{}_{}", class_name, method);
            ctable.push(method_mangled_name);
        }

        for method in c.methods.iter() {
            class_transformer.transform_method(graph, method, &self_obj, &c.id.item);
        }

        let constructor_label = class_transformer.add_constructor(graph, &c.id.item, &fields);
        graph.functions.insert(format!("_new_{}", &c.id.item), (constructor_label.clone(), vec![]));
        graph.classes.insert(c.id.item.clone(), (ctable, fields));
    }

    fn add_constructor(&mut self, graph: &mut ControlFlowGraph, class: &String, fields: &Vec<Reg>) -> Label {
        self.inc_num();
        let mut function_transformer = QuadrupleCodeTransformer::new_frame(self, Some(class.clone()));
        let block_label = function_transformer.new_label();
        graph.begin_block(block_label.clone());
        let reg = Reg::new(IType::Class(class.clone()), function_transformer.new_label());
        graph.push(Instr::Call(reg.clone(), format!("_alloc_size"), vec![Value::Int(1 + fields.len() as i32)]));
        let object = Value::Register(reg);

        graph.push(Instr::Insert(object.clone(), 0, Value::Const(format!("_vtable_{}", class))));
        for (i, reg) in fields.iter().enumerate() {
            let init = match &reg.itype {
                IType::Int => Value::Int(0),
                IType::String => Value::String(0),
                IType::Boolean => Value::Bool(false),
                IType::Class(_) => Value::Null,
                _ => unreachable!()
            };
            graph.push(Instr::Insert(object.clone(), i + 1, init));
        }
        graph.push(Instr::Return(object));
        graph.close_block();
        block_label
    }

    fn transform_method(&mut self, graph: &mut ControlFlowGraph, f: &Function, self_obj: &Reg, class_name: &str) {
        let method_mangled_name = format!("_{}_{}", class_name, f.id.item);
        self.inc_num();
        let mut method_transformer = QuadrupleCodeTransformer::new_frame(self, Some(method_mangled_name.clone()));
        let mut arg_reg = vec![];
        arg_reg.push(self_obj.clone());
        method_transformer.insert_var(Self::obj_self(), self_obj.clone(), None);

        for arg in &f.args {
            let var = method_transformer.new_label();
            let reg = Reg::new(arg.0.item.clone(), var);
            method_transformer.insert_var(arg.1.clone(), reg.clone(), None);
            arg_reg.push(reg);
        }
        let label = method_transformer.transform_block(graph, &f.block, None);
        graph.functions.insert(method_mangled_name.clone(), (label, arg_reg));
    }

    fn transform_function(&mut self, graph: &mut ControlFlowGraph, f: &Function) {
        self.inc_num();
        let mut function_transformer = QuadrupleCodeTransformer::new_frame(self, Some(f.id.item.clone()));
        let mut arg_reg = vec![];
        for arg in &f.args {
            let var = function_transformer.new_label();
            let reg = Reg::new(arg.0.item.clone(), var);
            function_transformer.insert_var(arg.1.clone(), reg.clone(), None);
            arg_reg.push(reg);
        }

        let label = function_transformer.transform_block(graph, &f.block, None);
        graph.functions.insert(f.id.item.clone(), (label, arg_reg));
    }

    fn transform_block(&mut self, graph: &mut ControlFlowGraph, b: &Block, out: Option<Label>) -> Label {
        self.inc_num();
        let mut block_transformer = QuadrupleCodeTransformer::new_frame(self, None);
        let block_label = block_transformer.new_label();
        graph.begin_block(block_label.clone());

        for i in &b.stmts {
            match &i.item {
                IStmt::Empty => (),
                IStmt::Block(bb) => {
                    let new_label = block_transformer.new_label();
                    let inner_label = block_transformer.transform_block(graph, bb, Some(new_label.clone()));

                    graph.push(Instr::Jump(inner_label));
                    graph.close_block();
                    graph.begin_block(new_label);
                }

                IStmt::Decl { t, items } => {
                    for item in items {
                        match item {
                            Item::NoInit(x) => {
                                let var = block_transformer.new_label();
                                let reg = Reg::new(t.item.clone(), var);
                                block_transformer.insert_var(x.clone(), reg.clone(), None);
                                match t.item {
                                    IType::Int => graph.push(Instr::Copy(reg, Value::Int(0))),
                                    IType::String => graph.push(Instr::Copy(reg, Value::String(0))),
                                    IType::Boolean => graph.push(Instr::Copy(reg, Value::Bool(false))),
                                    IType::Void => unreachable!(),
                                    IType::Class(_) => graph.push(Instr::Copy(reg, Value::Null)),
                                    IType::Null => unreachable!(), //todo check?
                                }
                            }
                            Item::Init { i, e } => {
                                let v = block_transformer.transform_expr(graph, e, None);
                                match v {
                                    Value::Register(reg) => block_transformer.insert_var(i.clone(), reg.clone(), None),
                                    _ => {
                                        let var = block_transformer.new_label();
                                        let reg = Reg::new(t.item.clone(), var);
                                        block_transformer.insert_var(i.clone(), reg.clone(), None);
                                        graph.push(Instr::Copy(reg, v))
                                    }
                                }
                            }
                        }
                    }
                }
                IStmt::Asg { i, e } => match i {
                    Target::Id(id) => {
                        let v = block_transformer.transform_expr(graph, e, None);
                        match block_transformer.find_var(id) {
                            (reg, None) => graph.push(Instr::Copy(reg, v)),
                            (obj, Some((_, field_num))) => {
                                let object = Value::Register(obj);
                                graph.push(Instr::Insert(object, field_num, v));
                            }
                        }
                    }
                    Target::Field(Field { e: obj, id }) => {
                        let v = block_transformer.transform_expr(graph, e, None);
                        let object = block_transformer.transform_expr(graph, obj, None);
                        let class_name = object.to_class();
                        let num = block_transformer.find_class(&class_name).var_num(id);
                        graph.push(Instr::Insert(object, num, v));
                    }
                }
                IStmt::Incr(t) => match t {
                    Target::Id(id) => {
                        match block_transformer.find_var(id) {
                            (reg, None) => graph.push(Instr::Asg1(reg.clone(), UnOp::Incr, Value::Register(reg))),
                            (obj, Some((itype, field_num))) => {
                                let object = Value::Register(obj);
                                let reg = Reg::new(itype, block_transformer.new_label());
                                graph.push(Instr::Extract(reg.clone(), object.clone(), field_num));
                                graph.push(Instr::Asg1(reg.clone(), UnOp::Incr, Value::Register(reg.clone())));
                                graph.push(Instr::Insert(object, field_num, Value::Register(reg.clone())));
                            }
                        }
                    }
                    Target::Field(Field { e: obj, id }) => {
                        let object = block_transformer.transform_expr(graph, obj, None);
                        let class_name = object.to_class();
                        let num = block_transformer.find_class(&class_name).var_num(id);
                        let reg = Reg::new(IType::Int, block_transformer.new_label());

                        graph.push(Instr::Extract(reg.clone(), object.clone(), num));
                        graph.push(Instr::Asg1(reg.clone(), UnOp::Incr, Value::Register(reg.clone())));
                        graph.push(Instr::Insert(object, num, Value::Register(reg.clone())));
                    }
                }

                IStmt::Decr(t) => match t {
                    Target::Id(id) => {
                        match block_transformer.find_var(id) {
                            (reg, None) => graph.push(Instr::Asg1(reg.clone(), UnOp::Decr, Value::Register(reg))),
                            (obj, Some((itype, field_num))) => {
                                let object = Value::Register(obj);
                                let reg = Reg::new(itype, block_transformer.new_label());
                                graph.push(Instr::Extract(reg.clone(), object.clone(), field_num));
                                graph.push(Instr::Asg1(reg.clone(), UnOp::Decr, Value::Register(reg.clone())));
                                graph.push(Instr::Insert(object, field_num, Value::Register(reg.clone())));
                            }
                        }
                    }
                    Target::Field(Field { e: obj, id }) => {
                        let object = block_transformer.transform_expr(graph, obj, None);
                        let class_name = object.to_class();
                        let num = block_transformer.find_class(&class_name).var_num(id);
                        let reg = Reg::new(IType::Int, block_transformer.new_label());

                        graph.push(Instr::Extract(reg.clone(), object.clone(), num));
                        graph.push(Instr::Asg1(reg.clone(), UnOp::Decr, Value::Register(reg.clone())));
                        graph.push(Instr::Insert(object, num, Value::Register(reg.clone())));
                    }
                }
                IStmt::Ret(t) => {
                    let v = block_transformer.transform_expr(graph, t, None);
                    graph.push(Instr::Return(v))
                }
                IStmt::VRet => {
                    graph.push(Instr::VReturn)
                }
                IStmt::Cond { c, if_true } => {
                    let end_label = block_transformer.new_label();
                    let true_label = block_transformer.transform_block(graph, if_true, Some(end_label.clone()));

                    block_transformer.transform_expr(graph, c, Some((&true_label, &end_label)));
                    graph.close_block();

                    graph.begin_block(end_label);
                }

                IStmt::CondElse { c, if_true, if_false } => {
                    let end_label = block_transformer.new_label();

                    let false_label = block_transformer.transform_block(graph, if_false, Some(end_label.clone()));
                    let true_label = block_transformer.transform_block(graph, if_true, Some(end_label.clone()));

                    block_transformer.transform_expr(graph, c, Some((&true_label, &false_label)));
                    graph.close_block();
                    graph.begin_block(end_label);
                }
                IStmt::While { c, while_true } => {
                    let end_label = block_transformer.new_label();
                    let cond_label = block_transformer.new_label();
                    let true_label = block_transformer.transform_block(graph, while_true, Some(cond_label.clone()));

                    graph.push(Instr::Jump(cond_label.clone()));
                    graph.close_block();

                    graph.begin_block(cond_label);
                    block_transformer.transform_expr(graph, c, Some((&true_label, &end_label)));
                    graph.close_block();

                    graph.begin_block(end_label);
                }
                IStmt::Expr(e) => {
                    let _ = block_transformer.transform_expr(graph, e, None);
                }
            }
        }

        if let Some(out) = out {
            graph.push(Jump(out.clone()));
        }
        graph.close_block();
        block_label
    }

    fn transform_expr(&mut self, graph: &mut ControlFlowGraph, expr: &Expr, cond: Option<(&Label, &Label)>) -> Value {
        let reg = match &expr.item {
            IExpr::Unary { o, e } => {
                match o {
                    Unary::IntNegation => {
                        let v = self.transform_expr(graph, e, cond);
                        let reg = Reg::new(IType::Int, self.new_label());
                        graph.push(Instr::Asg1(reg.clone(), UnOp::IntNeg, v));
                        Value::Register(reg)
                    }
                    Unary::BoolNegation => {
                        match cond {
                            None => {
                                let label_true = self.new_label();
                                let label_false = self.new_label();
                                let label_end = self.new_label();
                                let reg = Reg::new(IType::Boolean, self.new_label());
                                self.transform_expr(graph, e, Some((&label_false, &label_true)));

                                graph.begin_block(label_true);
                                graph.push(Instr::Copy(reg.clone(), Value::Bool(true)));
                                graph.push(Instr::Jump(label_end.clone()));
                                graph.close_block();
                                graph.begin_block(label_false);
                                graph.push(Instr::Copy(reg.clone(), Value::Bool(false)));
                                graph.push(Instr::Jump(label_end.clone()));
                                graph.close_block();

                                graph.close_block();
                                graph.begin_block(label_end);

                                Value::Register(reg)
                            }
                            Some((t, f)) => {
                                self.transform_expr(graph, e, Some((f, t)))
                            }
                        }
                    }
                }
            }
            IExpr::Binary { o, l, r } => {
                match o.item {
                    IBinOp::Mul |
                    IBinOp::Div |
                    IBinOp::Mod |
                    IBinOp::Add |
                    IBinOp::Sub => self.transform_op(graph, l, &o.item, r, cond),
                    IBinOp::LT |
                    IBinOp::LE |
                    IBinOp::GT |
                    IBinOp::GE |
                    IBinOp::EQ |
                    IBinOp::NE => self.transform_rel(graph, l, &o.item, r, cond),
                    IBinOp::And => self.transform_and(graph, l, r, cond),
                    IBinOp::Or => self.transform_or(graph, l, r, cond),
                }
            }
            IExpr::Var(v) => {
                let reg = match self.find_var(&v) {
                    (reg, None) => reg,

                    (reg, Some((itype, field_num))) => {
                        let obj = Value::Register(reg);
                        let new_reg = Reg::new(itype, self.new_label());
                        graph.push(Instr::Extract(new_reg.clone(), obj, field_num));
                        new_reg
                    }
                };
                match cond {
                    None => Value::Register(reg),
                    Some((t, f)) => {
                        graph.push(Instr::If(If(Value::Register(reg.clone()), EQ, Value::Bool(true), t.clone(), f.clone())));
                        Value::Register(reg)
                    }
                }
            }
            IExpr::Int(i) => Value::Int(i.clone()),
            IExpr::Bool(b) => match cond {
                None => Value::Bool(b.clone()),
                Some((t, f)) => {
                    graph.push(Instr::Jump(if *b { t.clone() } else { f.clone() }));
                    Value::Bool(b.clone())
                }
            }
            IExpr::FunCall { name: Target::Field(Field { e, id }), args } => {
                let mut call_args = vec![];
                let obj = self.transform_expr(graph, e, None);
                call_args.push(obj.clone());
                for arg in args {
                    let v = self.transform_expr(graph, arg, None);
                    call_args.push(v);
                }
                let class_name = obj.to_class();
                let class = self.find_class(&class_name);
                let method_num = class.method_num(id);
                let method = class.find_method(id).unwrap();
                let reg = Reg::new(method.ret_type.item.clone(), self.new_label());
                graph.push(Instr::CallM(reg.clone(), obj, method_num, call_args));
                match cond {
                    None => {}
                    Some((t, f)) => graph.push(Instr::If(If(Value::Register(reg.clone()), EQ, Value::Bool(true), t.clone(), f.clone())))
                }
                Value::Register(reg)
            }

            IExpr::FunCall { name: Target::Id(id), args } => {
                let mut call_args = vec![];
                for arg in args {
                    let v = self.transform_expr(graph, arg, None);
                    call_args.push(v);
                }
                let sig = self.find_function(id.item.as_str());
                let reg = Reg::new(sig.ret_type.item.clone(), self.new_label());
                let fun = id.item.clone();
                graph.push(Instr::Call(reg.clone(), fun, call_args));
                match cond {
                    None => {}
                    Some((t, f)) => graph.push(Instr::If(If(Value::Register(reg.clone()), EQ, Value::Bool(true), t.clone(), f.clone())))
                }
                Value::Register(reg)
            }
            IExpr::String(str) => {
                let l = self.find_string(str);
                Value::String(l)
            }
            IExpr::Paren(e) => self.transform_expr(graph, e, cond),
            IExpr::Null => Value::Null,
            IExpr::Field(Field { e, id }) => {
                let object = self.transform_expr(graph, e, cond);
                let class_name = object.to_class();
                let class = self.find_class(&class_name).clone();
                let num = class.var_num(id);
                let new_reg = Reg::new(class.find_var(id).unwrap().item.clone(), self.new_label());
                graph.push(Instr::Extract(new_reg.clone(), object, num));
                Value::Register(new_reg)
            }
            IExpr::Object(o) => match &o.item {
                IType::Class(c) => {
                    let reg = Reg::new(o.item.clone(), self.new_label());
                    graph.push(Instr::Call(reg.clone(), format!("_new_{}", c), vec![]));
                    Value::Register(reg)
                }
                _ => unreachable!(), //can be null type? No!
            }
            IExpr::Cast { t, e } => {
                let v = self.transform_expr(graph, e, None);
                let class_type = t.item.clone();
                let new_reg = Reg::new(class_type, self.new_label());
                graph.push(Instr::Cast(new_reg.clone(), v));
                Value::Register(new_reg)
            }
        };
        reg
    }

    fn transform_and(&mut self, graph: &mut ControlFlowGraph, l: &Box<Expr>, r: &Box<Expr>, cond: Option<(&Label, &Label)>) -> Value {
        match cond {
            None => {
                let label_true1 = self.new_label();
                let label_true2 = self.new_label();
                let label_false = self.new_label();
                let label_end = self.new_label();

                let v1 = self.transform_expr(graph, l, Some((&label_true1, &label_false)));
                let res = match v1 {
                    Value::Bool(true) => {
                        graph.begin_block(label_true1);
                        let v2 = self.transform_expr(graph, r, None);
                        graph.close_block();
                        graph.begin_block(label_false);
                        graph.close_block();
                        v2
                    }
                    Value::Bool(false) => {
                        graph.begin_block(label_false);
                        graph.push(Instr::Jump(label_end.clone()));
                        graph.close_block();
                        graph.begin_block(label_true1);
                        graph.close_block();
                        v1
                    }
                    _ => {
                        graph.begin_block(label_true1);
                        let reg = Reg::new(IType::Boolean, self.new_label());
                        let v2 = self.transform_expr(graph, r, Some((&label_true2, &label_false)));
                        graph.close_block();
                        match v2 {
                            Value::Bool(false) => {
                                graph.begin_block(label_false);
                                graph.push(Instr::Copy(reg.clone(), Value::Bool(false)));
                                graph.push(Instr::Jump(label_end.clone()));
                                graph.close_block();
                            }
                            _ => {
                                graph.begin_block(label_true2);
                                graph.push(Instr::Copy(reg.clone(), Value::Bool(true)));
                                graph.push(Instr::Jump(label_end.clone()));
                                graph.close_block();
                                graph.begin_block(label_false);
                                graph.push(Instr::Copy(reg.clone(), Value::Bool(false)));
                                graph.push(Instr::Jump(label_end.clone()));
                                graph.close_block();
                            }
                        }
                        Value::Register(reg)
                    }
                };
                graph.close_block();
                graph.begin_block(label_end);
                res
            }
            Some((t, f)) => {
                let m = self.new_label();
                let v1 = self.transform_expr(graph, l, Some((&m, &f)));
                match v1 {
                    Value::Bool(false) => v1,
                    Value::Bool(true) => {
                        graph.begin_block(m);
                        graph.close_block();
                        self.transform_expr(graph, r, Some((&t, &f)))
                    }
                    _ => {
                        graph.begin_block(m);
                        self.transform_expr(graph, r, Some((&t, &f)));
                        graph.close_block();
                        v1
                    }
                }
            }
        }
    }

    fn transform_or(&mut self, graph: &mut ControlFlowGraph, l: &Box<Expr>, r: &Box<Expr>, cond: Option<(&Label, &Label)>) -> Value {
        match cond {
            None => {
                let label_false1 = self.new_label();
                let label_false2 = self.new_label();
                let label_true = self.new_label();
                let label_end = self.new_label();

                let v1 = self.transform_expr(graph, l, Some((&label_true, &label_false1)));
                let res = match v1 {
                    Value::Bool(true) => {
                        graph.begin_block(label_true);
                        graph.push(Instr::Jump(label_end.clone()));
                        graph.close_block();
                        graph.begin_block(label_false1);
                        graph.close_block();
                        v1
                    }
                    Value::Bool(false) => {
                        graph.begin_block(label_false1);
                        let v2 = self.transform_expr(graph, r, None);
                        graph.push(Instr::Jump(label_end.clone()));
                        graph.close_block();
                        graph.begin_block(label_true);
                        graph.close_block();
                        v2
                    }
                    _ => {
                        graph.begin_block(label_false1);
                        let reg = Reg::new(IType::Boolean, self.new_label());
                        let v2 = self.transform_expr(graph, r, Some((&label_true, &label_false2)));
                        graph.close_block();
                        match v2 {
                            Value::Bool(true) => {
                                graph.begin_block(label_true);
                                graph.push(Instr::Copy(reg.clone(), Value::Bool(true)));
                                graph.push(Instr::Jump(label_end.clone()));
                                graph.close_block();
                            }
                            _ => {
                                graph.begin_block(label_false2);
                                graph.push(Instr::Copy(reg.clone(), Value::Bool(false)));
                                graph.push(Instr::Jump(label_end.clone()));
                                graph.close_block();
                                graph.begin_block(label_true);
                                graph.push(Instr::Copy(reg.clone(), Value::Bool(true)));
                                graph.push(Instr::Jump(label_end.clone()));
                                graph.close_block();
                            }
                        }
                        Value::Register(reg)
                    }
                };
                graph.close_block();
                graph.begin_block(label_end);
                res
            }
            Some((t, f)) => {
                let m = self.new_label();
                let v1 = self.transform_expr(graph, l, Some((&t, &m)));
                match v1 {
                    Value::Bool(true) => v1,
                    Value::Bool(false) => {
                        graph.begin_block(m);
                        graph.close_block();
                        self.transform_expr(graph, r, Some((&t, &f)))
                    }
                    _ => {
                        graph.begin_block(m);
                        self.transform_expr(graph, r, Some((&t, &f)));
                        graph.close_block();
                        v1
                    }
                }
            }
        }
    }

    fn transform_rel(&mut self, graph: &mut ControlFlowGraph, l: &Box<Expr>, o: &IBinOp, r: &Box<Expr>, cond: Option<(&Label, &Label)>) -> Value {
        match cond {
            None => {
                let label_false = self.new_label();
                let label_true = self.new_label();
                let label_end = self.new_label();
                let v1 = self.transform_expr(graph, l, None);
                let v2 = self.transform_expr(graph, r, None);
                let reg = Reg::new(IType::Boolean, self.new_label());

                graph.push(Instr::If(If(v1.clone(), o.clone().into(), v2.clone(), label_true.clone(), label_false.clone())));

                graph.begin_block(label_false);
                graph.push(Instr::Copy(reg.clone(), Value::Bool(false)));
                graph.push(Instr::Jump(label_end.clone()));
                graph.close_block();

                graph.begin_block(label_true);
                graph.push(Instr::Copy(reg.clone(), Value::Bool(true)));
                graph.push(Instr::Jump(label_end.clone()));
                graph.close_block();

                graph.close_block();
                graph.begin_block(label_end);
                Value::Register(reg)
            }
            Some((t, f)) => {
                let v1 = self.transform_expr(graph, l, None);
                let v2 = self.transform_expr(graph, r, None);
                match (&v1, &o) {
                    (Value::String(_), IBinOp::EQ) |
                    (Value::Register(Reg { itype: IType::String, .. }), IBinOp::EQ) => {
                        graph.push(Instr::If(If(v1.clone(), RelOp::CMP, v2.clone(), t.clone(), f.clone())));
                    }
                    _ => graph.push(Instr::If(If(v1.clone(), o.clone().into(), v2.clone(), t.clone(), f.clone()))),
                }

                v1
            }
        }
    }

    fn transform_op(&mut self, graph: &mut ControlFlowGraph, l: &Box<Expr>, o: &IBinOp, r: &Box<Expr>, cond: Option<(&Label, &Label)>) -> Value {
        let v1 = self.transform_expr(graph, l, cond);
        let v2 = self.transform_expr(graph, r, cond);

        let reg = match &v1 {
            Value::Register(reg) => Reg::new(reg.itype.clone(), self.new_label()),
            Value::Int(_) => Reg::new(IType::Int, self.new_label()),
            Value::String(_) => Reg::new(IType::String, self.new_label()),
            Value::Bool(_) => Reg::new(IType::Boolean, self.new_label()),
            _ => unreachable!(),
        };
        if reg.itype == IType::String && *o == IBinOp::Add {
            graph.push(Instr::Asg2(reg.clone(), v1, BinOp::Concat, v2));
        } else {
            graph.push(Instr::Asg2(reg.clone(), v1, o.clone().into(), v2));
        }
        Value::Register(reg)
    }
}