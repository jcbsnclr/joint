use crate::parser::Expression;
use crate::lexer::{BinaryOperator, UnaryOperator};

use std::collections::HashMap;

struct State {
    labels: HashMap<String, usize>,
    vars: HashMap<String, i64>,
    ip: (usize, bool),
    cstack: Vec<usize>
}

impl State {
    pub fn new(labels: HashMap<String, usize>) -> State {
        State {
            labels,
            vars: HashMap::new(),
            ip: (0, false),
            cstack: Vec::new()
        }
    }

    pub fn set_ip(&mut self, new_ip: usize) {
        self.ip = (new_ip, true);
    }

    pub fn is_ip_dirty(&self) -> bool { self.ip.1 }

    pub fn ip_clear_dirty(&mut self) {
        self.ip.1 = false;
    }

    pub fn ip(&self) -> usize { self.ip.0 }

    fn eval_expr(&mut self, expr: Expression) -> i64 {
        let val = match expr {
            Expression::Print(e) => {println!("{:?}", self.eval_expr(*e)); 0},
            Expression::BinaryOperator(op, v1, v2) => match op {
                BinaryOperator::Plus => self.eval_expr(*v1) + self.eval_expr(*v2),
                BinaryOperator::Hyphen => self.eval_expr(*v1) - self.eval_expr(*v2),
                BinaryOperator::Asterisk => self.eval_expr(*v1) * self.eval_expr(*v2),
                BinaryOperator::Slash => self.eval_expr(*v1) / self.eval_expr(*v2),

                BinaryOperator::Equals => if self.eval_expr(*v1) == self.eval_expr(*v2) {
                    1
                } else {
                    0
                },

                BinaryOperator::And => if self.eval_expr(*v1) != 0 && self.eval_expr(*v2) != 0 { 1 } else { 0 },
                BinaryOperator::Or => if self.eval_expr(*v1) != 0 || self.eval_expr(*v2) != 0 { 1 } else { 0 },
            },

            Expression::UnaryOperator(op, val) => match op {
                UnaryOperator::Not => if self.eval_expr(*val) == 0 { 1 } else { 0 }
            }

            Expression::Ip => self.ip() as i64,

            Expression::GotoIf(name, cond) => {
                if self.eval_expr(*cond) != 0 {
                    let index = *self.labels.get(&name).unwrap();
                    self.set_ip(index);
                }
                0
            }

            Expression::Call(label) => {
                self.cstack.push(self.ip());
                let new_ip = *self.labels.get(&label).unwrap();
                self.set_ip(new_ip);
                0
            }

            Expression::Return => {
                self.ip.0 = self.cstack.pop().unwrap();
                0
            }

            Expression::Set(name, val) => {
                let result = self.eval_expr(*val);
                self.vars.insert(name, result);
                0
            }

            Expression::Integer(i) => i,
            Expression::String(_) => 0,
            Expression::Identifier(name) => *self.vars.get(&name).unwrap()
        };

        val
    }

    fn eval_expr_tl(&mut self, expr: Expression) {
        self.eval_expr(expr);

        if !self.is_ip_dirty() {
            self.ip.0 += 1;
        } else {
            self.ip_clear_dirty();
        }
    }
}

pub fn run((program, labels): (Vec<Expression>, HashMap<String, usize>)) {
    let mut state = State::new(labels);

    while state.ip() < program.len() {
        state.eval_expr_tl(program[state.ip()].clone());
    }
}