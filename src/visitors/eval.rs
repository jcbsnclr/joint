use crate::visitors::compiler::{IrOp, Program};
use crate::lexer::{BinOp, UnOp};

use super::compiler::OpRef;

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

            IrOp::PrintType(t) => println!("{:?}", t),
            
            IrOp::BinOp(op) => {
                let (v2, v1) = (vstack.pop().unwrap(), vstack.pop().unwrap());
                vstack.push(match op {
                    BinOp::Plus => v1 + v2,
                    BinOp::Hyphen => v1 - v2,
                    BinOp::Asterisk => v1 * v2,
                    BinOp::Slash => v1 / v2,
                    BinOp::Equals => if v1 == v2 { 1 } else { 0 },
                    BinOp::And => if v1 != 0 && v2 != 0 { 1 } else { 0 },
                    BinOp::Or => if v1 != 0 || v2 != 0 { 1 } else { 0 },
                });
            }

            IrOp::UnOp(op) => {
                let v = vstack.pop().unwrap();

                vstack.push(match op {
                    UnOp::Not => if v == 0 { 1 } else { 0 }
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
                if let OpRef::Resolved(i) = i {
                    cstack.push(ip);
                    new_ip = Some(i);
                } else {
                    panic!("Unresolved IP {:?}", i);
                }
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
                if let OpRef::Resolved(i) = i {
                    let cond = vstack.pop().unwrap();
                    if cond != 0 {
                        new_ip = Some(i);
                    }
                } else {
                    panic!("Unresolved IP {:?}", i);
                }
            }

            IrOp::Exit => break
        }

        if let Some(new_ip) = new_ip {
            ip = new_ip;
        } else {
            ip += 1;
        }
    }
}