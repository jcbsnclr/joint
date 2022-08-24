use crate::parser::Expression;
use crate::lexer::{BinaryOperator, UnaryOperator};

use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
pub enum IrOp {
    Lit(i64),

    BinOp(BinaryOperator),
    UnOp(UnaryOperator),

    Set(usize),
    Get(usize),

    Call(usize),
    Return,

    Print,
    Ip,

    GotoIf(usize),
}

pub struct Compiler {
    variables: HashMap<String, usize>,
    variables_to_fill: HashMap<usize, String>,
    labels: HashMap<String, usize>,
    labels_to_fill: HashMap<usize, String>,
    ops: Vec<IrOp>,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            labels: HashMap::new(),
            labels_to_fill: HashMap::new(),
            variables: HashMap::new(),
            variables_to_fill: HashMap::new(),
            ops: Vec::new()
        }
    }

    fn scan_declarations(&mut self, prog: &[Expression]) -> usize {
        let mut varc = 0;

        for expr in prog {
            match expr {
                Expression::Decl(name) => {
                    self.variables
                        .insert(name.to_string(), varc);

                    varc += 1;
                },

                _ => continue
            }
        }

        varc
    }

    fn fill_values(&mut self) {
        for (index, label) in self.labels_to_fill.iter() {
            let ip = self.labels.get(label).unwrap();

            match &mut self.ops[*index] {
                IrOp::Call(i) => *i = *ip,
                IrOp::GotoIf(i) => *i = *ip,

                o => unimplemented!("{:?}", o)
            }
        }

        for (index, name) in self.variables_to_fill.iter() {
            let var = self.variables.get(name).unwrap();

            match &mut self.ops[*index] {
                IrOp::Set(v) => *v = *var,
                IrOp::Get(v) => *v = *var,

                _ => unimplemented!()
            }
        }
    }

    fn compile_expr(&mut self, expr: Expression) {
        match expr {
            Expression::Label(name) => {
                self.labels.insert(name, self.ops.len());
            }

            Expression::Func(name, args, body) => {
                let trampoline_hook = format!("__{}_trampoline_hook", name);

                self.ops.push(IrOp::Lit(1));
                self.label_op(trampoline_hook.clone());
                self.ops.push(IrOp::GotoIf(0));

                self.labels.insert(name, self.ops.len());

                for expr in body {
                    self.compile_expr(expr);
                }

                self.ops.push(IrOp::Return);
                self.labels.insert(trampoline_hook, self.ops.len());
            }

            Expression::Identifier(name) => {
                self.var_op(name);
                self.ops.push(IrOp::Get(0));
            },

            Expression::Set(name, val) => {
                self.compile_expr(*val);
                self.var_op(name.clone());
                self.ops.push(IrOp::Set(0));
            }

            Expression::GotoIf(label, expr) => {
                self.compile_expr(*expr);
                self.label_op(label);
                self.ops.push(IrOp::GotoIf(0));
            }

            Expression::Call(label) => {
                self.label_op(label);
                self.ops.push(IrOp::Call(0));
            }

            Expression::Print(expr) => {
                self.compile_expr(*expr);
                self.ops.push(IrOp::Print);
            }

            Expression::Return => self.ops.push(IrOp::Return),
            Expression::Ip => self.ops.push(IrOp::Ip),

            Expression::BinaryOperator(op, n1, n2) => {
                self.compile_expr(*n1);
                self.compile_expr(*n2);
                self.ops.push(IrOp::BinOp(op));
            }

            Expression::UnaryOperator(op, n) => {
                self.compile_expr(*n);
                self.ops.push(IrOp::UnOp(op));
            }

            Expression::DoLoopIf(cond, body) => {
                let body_ptr = self.ops.len();
                
                for expr in body {
                    self.compile_expr(expr);
                }

                self.compile_expr(*cond);

                self.ops.push(IrOp::GotoIf(body_ptr));
            }

            Expression::Integer(i) => self.ops.push(IrOp::Lit(i)),
        
            _ => {}
        }
    }

    fn var_op(&mut self, name: String) {
        self.variables_to_fill.insert(self.ops.len(), name);
    }

    fn label_op(&mut self, name: String) {
        self.labels_to_fill.insert(self.ops.len(), name);
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub var_count: usize,
    pub body: Vec<IrOp>
}

pub fn compile(program: Vec<Expression>) -> Program{
    let mut compiler = Compiler::new();

    let var_count = compiler.scan_declarations(&program);

    for expr in program {
        compiler.compile_expr(expr);
    }

    compiler.fill_values();

    Program {
        var_count,
        body: compiler.ops
    }
}