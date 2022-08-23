use crate::visitors::compiler::{IrOp, Program};
use crate::lexer::{BinaryOperator, UnaryOperator};

pub fn run(prog: Program) {
    let mut vars = vec![0; prog.var_count];
    let mut ip = 0;
    let mut cstack = Vec::new();
    let mut vstack = Vec::new();

    while ip < prog.body.len() {
        let cur = prog.body[ip];
        let mut new_ip = None;

        match cur {
            IrOp::Lit(i) => vstack.push(i),
            
            IrOp::BinOp(op) => {
                let (v2, v1) = (vstack.pop().unwrap(), vstack.pop().unwrap());
                vstack.push(match op {
                    BinaryOperator::Plus => v1 + v2,
                    BinaryOperator::Hyphen => v1 - v2,
                    BinaryOperator::Asterisk => v1 * v2,
                    BinaryOperator::Slash => v1 / v2,
                    BinaryOperator::Equals => if v1 == v2 { 1 } else { 0 },
                    BinaryOperator::And => if v1 != 0 && v2 != 0 { 1 } else { 0 },
                    BinaryOperator::Or => if v1 != 0 || v2 != 0 { 1 } else { 0 },
                });
            }

            IrOp::UnOp(op) => {
                let v = vstack.pop().unwrap();

                vstack.push(match op {
                    UnaryOperator::Not => if v == 0 { 1 } else { 0 }
                });
            }

            IrOp::Set(var) => {
                let v = vstack.pop().unwrap();
                vars[var] = v;
            }

            IrOp::Get(var) => {
                vstack.push(vars[var]);
            }

            IrOp::Call(i) => {
                cstack.push(ip);
                new_ip = Some(i);
            }

            IrOp::Return => {
                let i = cstack.pop().unwrap();
                new_ip = None;
                ip = i;
            }

            IrOp::Print => {
                let val = vstack.pop().unwrap();
                println!("{}", val);
            }

            IrOp::Ip => {
                vstack.push(ip as i64);
            }

            IrOp::GotoIf(i) => {
                let cond = vstack.pop().unwrap();
                if cond != 0 {
                    new_ip = Some(i);
                }
            }
        }

        if let Some(new_ip) = new_ip {
            ip = new_ip;
        } else {
            ip += 1;
        }
    }
}