use std::collections::HashMap;

use crate::backend::quadruple_code_transformer::QuadrupleCodeTransformer::Root;
use crate::frontend::global_analyser::FunctionSignature;
use crate::frontend::Maps;
use crate::model::ast::{Block, Expr, Function, IBinOp, Id, IExpr, IStmt, Item, IType, Program, Target, TopDef, UnOp as Unary};
use crate::model::quadruple_code::{ControlFlowGraph, Instr, Label, Reg, UnOp, Value};
use crate::model::quadruple_code::Instr::Jump;
use crate::model::quadruple_code::RelOp::EQ;

pub enum QuadrupleCodeTransformer<'a> {
    Root {
        label_num: u32,
        env: HashMap<String, Reg>,
        strings: HashMap<String, u32>,
        functions: HashMap<Label, FunctionSignature>,
        num: u32,
    },
    Frame {
        parent: &'a QuadrupleCodeTransformer<'a>,
        root: &'a QuadrupleCodeTransformer<'a>,
        env: HashMap<String, Reg>,
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
                format!("_{}", label_num)
            }
            QuadrupleCodeTransformer::Frame { label_num, num, current_name, .. } => {
                *label_num += 1;
                format!("_{}_{}_{}", current_name, label_num, num)
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

    fn find_function(&self, id: &str) -> &FunctionSignature {
        match self {
            QuadrupleCodeTransformer::Root { functions, .. } => functions.get(id).unwrap(),
            QuadrupleCodeTransformer::Frame { root, .. } => root.find_function(id),
        }
    }

    pub fn find_string(&self, str: &str) -> u32 {
        match self {
            QuadrupleCodeTransformer::Root { strings, .. } => strings.get(str).unwrap().clone(),
            QuadrupleCodeTransformer::Frame { root, .. } => root.find_string(str)
        }
    }

    pub fn insert_var(&mut self, id: Id, reg: Reg) {
        match self {
            QuadrupleCodeTransformer::Root { env, .. } => env.insert(id.item, reg),
            QuadrupleCodeTransformer::Frame { env, .. } => env.insert(id.item, reg),
        };
    }

    fn find_var(&self, id: &Id) -> Reg {
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
                TopDef::Class(_) => todo!(),
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

    fn transform_function(&mut self, graph: &mut ControlFlowGraph, f: &Function) {
        self.inc_num();
        let mut function_transformer = QuadrupleCodeTransformer::new_frame(self, Some(f.id.item.clone()));
        let mut arg_reg = vec![];
        for arg in &f.args {
            let var = function_transformer.new_label();
            let reg = Reg::new(arg.0.item.clone(), var);
            function_transformer.insert_var(arg.1.clone(), reg.clone());
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
                                block_transformer.insert_var(x.clone(), reg.clone());
                                match t.item {
                                    IType::Int => graph.push(Instr::Copy(reg, Value::Int(0))),
                                    IType::String => graph.push(Instr::Copy(reg, Value::String(0))),
                                    IType::Boolean => graph.push(Instr::Copy(reg, Value::Bool(false))),
                                    IType::Void => unreachable!(),
                                    IType::Class(_) => todo!(),
                                    IType::Null => todo!(),
                                }
                            }
                            Item::Init { i, e } => {
                                let v = block_transformer.transform_expr(graph, e, None);
                                match v {
                                    Value::Register(reg) => block_transformer.insert_var(i.clone(), reg.clone()),
                                    _ => {
                                        let var = block_transformer.new_label();
                                        let reg = Reg::new(t.item.clone(), var);
                                        block_transformer.insert_var(i.clone(), reg.clone());
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
                        let reg = block_transformer.find_var(id);
                        graph.push(Instr::Copy(reg, v))
                    }
                    Target::Field(_) => todo!(),
                }
                IStmt::Incr(t) => match t {
                    Target::Id(id) => {
                        let reg = block_transformer.find_var(id);
                        graph.push(Instr::Asg1(reg.clone(), UnOp::Incr, Value::Register(reg)));
                    }
                    Target::Field(_) => todo!(),
                }

                IStmt::Decr(t) => match t {
                    Target::Id(id) => {
                        let reg = block_transformer.find_var(id);
                        graph.push(Instr::Asg1(reg.clone(), UnOp::Decr, Value::Register(reg)));
                    }
                    Target::Field(_) => todo!(),
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
            IExpr::Var(v) => match cond {
                None => Value::Register(self.find_var(&v)),
                Some((t, f)) => {
                    let val = self.find_var(&v);
                    graph.push(Instr::If(Value::Register(val.clone()), EQ, Value::Bool(true), t.clone(), f.clone()));
                    Value::Register(val)
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
            IExpr::FunCall { name, args } => {
                let mut call_args = vec![];
                for arg in args {
                    let v = self.transform_expr(graph, arg, cond);
                    call_args.push(v);
                }
                let (id, reg) = match name {
                    Target::Id(id) => {
                        let sig = self.find_function(id.item.as_str());
                        let reg = Reg::new(sig.ret_type.item.clone(), self.new_label());
                        (id, reg)
                    }
                    Target::Field(_) => todo!()
                };
                graph.push(Instr::Call(reg.clone(), id.item.clone(), call_args));
                match cond {
                    None => {}
                    Some((t, f)) => graph.push(Instr::If(Value::Register(reg.clone()), EQ, Value::Bool(true), t.clone(), f.clone()))
                }
                Value::Register(reg)
            }
            IExpr::String(str) => {
                let l = self.find_string(str);
                Value::String(l)
            }
            IExpr::Paren(e) => self.transform_expr(graph, e, cond),
            IExpr::Null => todo!(),
            IExpr::Field(_) => todo!(),
            IExpr::Object(_) => todo!(),
            IExpr::Cast { .. } => todo!()
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
                    },
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
                    },
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

                graph.push(Instr::If(v1.clone(), o.clone().into(), v2.clone(), label_true.clone(), label_false.clone()));

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
                graph.push(Instr::If(v1.clone(), o.clone().into(), v2.clone(), t.clone(), f.clone()));

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
        };
        graph.push(Instr::Asg2(reg.clone(), v1, o.clone().into(), v2));
        Value::Register(reg)
    }
}