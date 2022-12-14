use crate::parser::{CompilationUnit, Declaration, DeclarationKind, Expr, ExprData};
use crate::lexer::{BinOp, UnOp};
use crate::visitors::validator::Type;

use std::collections::HashMap;

use super::validator::IntrinsicType;

#[derive(Debug, Copy, Clone)]
pub enum OpRef {
    Unresolved(usize),
    Resolved(usize)
}

#[derive(Debug, Clone)]
pub enum IrOp {
    Lit(i64),

    BinOp(BinOp),
    UnOp(UnOp),

    Set(usize),
    Get(usize),

    Call(OpRef),
    Return,

    Print,
    PrintString,
    PrintType(Type),

    BpUpdate,
    BpRem(isize),
    BpGet(isize),

    GotoIf(OpRef),

    Exit
}

pub struct Compiler {
    func_ids: HashMap<String, usize>,
    funcs: Vec<usize>,

    strings: Vec<String>,

    scope: Scope,

    globals: Vec<(usize, Box<Expr>)>,
    ops: Vec<IrOp>,
}

#[derive(Debug, Clone)]
struct Scope {
    var_total: usize,
    var_ptrs: Vec<HashMap<String, usize>>,

    arg_stores: HashMap<String, usize>
}

impl Scope {
    fn new() -> Scope {
        Scope {
            var_total: 0,
            var_ptrs: vec![HashMap::new()],

            arg_stores: HashMap::new()
        }
    }

    fn var_count(&self) -> usize {
        self.var_ptrs
            .iter()
            .map(|s| s.len())
            .sum()
    }

    fn current_scope(&mut self) -> &mut HashMap<String, usize> {
        self.var_ptrs.iter_mut().last().unwrap()
    }

    fn push_scope(&mut self) {
        self.var_ptrs.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.var_ptrs.pop().unwrap();
    }

    fn define_var(&mut self, name: String) -> usize {
        let r = self.var_count();
        self.current_scope()
            .insert(name.clone(), r);

        if self.var_total <= r {
            self.var_total = r + 1;
        }

        eprintln!("scope state: {:#?}", self);

        r
    }

    fn get_var_ptr(&mut self, name: &String) -> usize {
        for scope in self.var_ptrs.iter().rev() {
            if let Some(var) = scope.get(name) {
                return *var;
            }
        }

        panic!("var not found: `{}`", name)
    }
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            func_ids: HashMap::new(),
            funcs: Vec::new(),

            strings: Vec::new(),

            scope: Scope::new(),

            globals: Vec::new(),
            ops: Vec::new()
        }
    }

    fn func_count(&self) -> usize { self.func_ids.len() }

    fn process_declaration(&mut self, Declaration { kind, .. }: &Declaration) {
        match kind {
            DeclarationKind::Func { name, .. } => {
                self.func_ids.insert(name.clone(), self.func_count());
            },

            DeclarationKind::Var { name, value } => {
                let ptr = self.scope.define_var(name.clone());
                self.globals.push((ptr, value.clone()));
            },

            _ => ()
        }

        self.funcs = vec![0; self.func_count()];
    }

    fn compile_expr(&mut self, Expr { data, .. }: &Expr) {
        match data {
            ExprData::Integer(n) => {
                self.ops.push(IrOp::Lit(*n));
            }

            ExprData::StringLit(s) => {
                let id = self.strings.len();
                self.strings.push(s.clone());
                self.ops.push(IrOp::Lit(id as i64))
            }

            ExprData::FnCall(name, args) => {
                let id = *self.func_ids.get(name).unwrap();

                self.ops.push(IrOp::BpUpdate);

                for arg in args {
                    self.compile_expr(arg);
                }

                self.ops.push(IrOp::Call(OpRef::Unresolved(id)))
            }

            ExprData::BinOp(op, n1, n2) => {
                self.compile_expr(n1);
                self.compile_expr(n2);
                self.ops.push(IrOp::BinOp(*op));
            }

            ExprData::UnOp(op, n) => {
                self.compile_expr(n);
                self.ops.push(IrOp::UnOp(*op));
            }

            ExprData::Print(val) => {
                self.compile_expr(val);

                if val.typ == Some(Type::Concrete(IntrinsicType::String)) {
                    self.ops.push(IrOp::PrintString);
                } else {
                    self.ops.push(IrOp::Print);
                }
            }

            ExprData::PrintType(expr) => {
                self.ops.push(IrOp::PrintType(expr.typ.clone().unwrap()))
            }

            ExprData::Get(name) => {
                if let Some(pos) = self.scope.arg_stores.get(name) {
                    self.ops.push(IrOp::BpGet(*pos as isize));
                } else {
                    let ptr = self.scope.get_var_ptr(name);
                    self.ops.push(IrOp::Get(ptr));
                }
            }

            ExprData::Set(name, val) => {
                let ptr = self.scope.get_var_ptr(name);
                self.compile_expr(val);
                self.ops.push(IrOp::Set(ptr));
            },

            ExprData::DoLoopIf(cond, body) => {
                let start = self.ops.len();
                
                self.scope.push_scope();

                for expr in body.iter() {
                    self.compile_expr(expr);
                }

                self.compile_expr(cond);

                self.scope.pop_scope();

                self.ops.push(IrOp::GotoIf(OpRef::Resolved(start)));
            }

            ExprData::DoWhile(cond, body) => {
                let start = self.ops.len();

                self.compile_expr(cond);
                self.ops.push(IrOp::UnOp(UnOp::Not));

                let jmp_pos = self.ops.len();

                self.ops.push(IrOp::GotoIf(OpRef::Unresolved(0)));

                self.scope.push_scope();

                for expr in body.iter() {
                    self.compile_expr(expr);
                }

                self.scope.pop_scope();

                self.ops.push(IrOp::Lit(1));
                self.ops.push(IrOp::GotoIf(OpRef::Resolved(start)));

                let end = self.ops.len();
                self.ops[jmp_pos] = IrOp::GotoIf(OpRef::Resolved(end));
            }

            ExprData::DoBlock(body) => {
                self.scope.push_scope();

                for expr in body.iter() {
                    self.compile_expr(expr);
                }

                self.scope.pop_scope();
            }

            ExprData::Var(name, val) => {
                let ptr = self.scope.define_var(name.clone());
                self.compile_expr(val);
                self.ops.push(IrOp::Set(ptr))
            }

            ExprData::TypeCast(e, _) => {
                self.compile_expr(e);
            }
        }
    }

    fn compile_declaration(&mut self, Declaration { kind, .. }: &Declaration) {
        match kind {
            DeclarationKind::Func { name, args, body } => {
                let id = *self.func_ids.get(name).unwrap();
                self.funcs[id] = self.ops.len();

                self.scope.push_scope();

                for (i, arg) in args.iter().enumerate() {
                    self.scope.arg_stores.insert(arg.clone(), i);
                }

                for expr in body.iter() {
                    self.compile_expr(expr);
                }
                
                self.scope.pop_scope();

                self.ops.push(IrOp::BpRem(args.len() as isize));

                self.ops.push(IrOp::Return);
            },

            DeclarationKind::Var { name, value } => {
                let ptr = self.scope.get_var_ptr(name);
                
                self.globals.push((ptr, value.clone()));
            },

            _ => ()
        }
    }

    fn resolve_references(&mut self) {
        for op in self.ops.iter_mut() {
            match op {
                IrOp::Call(r) => {
                    if let OpRef::Unresolved(i) = r {
                        *r = OpRef::Resolved(self.funcs[*i]);
                    }
                },
                IrOp::GotoIf(r) => {
                    if let OpRef::Unresolved(i) = r {
                        *r = OpRef::Resolved(self.funcs[*i]);
                    }
                },

                _ => ()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub var_count: usize,
    pub body: Vec<IrOp>,
    pub strings: Vec<String>,
}

pub fn compile(unit: CompilationUnit) -> Program{
    let mut compiler = Compiler::new();

    for decl in unit.decls.iter() {
        compiler.process_declaration(decl);
    }

    for (var, val) in compiler.globals.clone() {
        compiler.compile_expr(&val);
        compiler.ops.push(IrOp::Set(var));
    }

    compiler.ops.push(IrOp::Call(OpRef::Unresolved(
        *compiler.func_ids.get(&String::from("main")).expect("failed to resolve main function")
    )));
    compiler.ops.push(IrOp::Exit);

    for decl in unit.decls.iter() {
        compiler.compile_declaration(decl);
    }

    compiler.resolve_references();

    Program {
        var_count: compiler.scope.var_total,
        body: compiler.ops,
        strings: compiler.strings
    }
}