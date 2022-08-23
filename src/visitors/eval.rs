use crate::parser::Expression;
use crate::lexer::Operator;

use std::collections::HashMap;

fn eval_expr(expr: Expression, labels: &HashMap<String, usize>, vars: &mut HashMap<String, i64>, ip: Option<&mut usize>) -> i64 {
    let mut new_ip = None;

    let val = match expr {
        Expression::Print(e) => {println!("{:?}", eval_expr(*e, labels, vars, None)); 0},
        Expression::Operator(op, v1, v2) => match op {
            Operator::Plus => eval_expr(*v1, labels, vars, None) + eval_expr(*v2, labels, vars, None),
            Operator::Hyphen => eval_expr(*v1, labels, vars, None) - eval_expr(*v2, labels, vars, None),
            Operator::Asterisk => eval_expr(*v1, labels, vars, None) * eval_expr(*v2, labels, vars, None),
            Operator::Slash => eval_expr(*v1, labels, vars, None) / eval_expr(*v2, labels, vars, None),
            Operator::Equals => if eval_expr(*v1, labels, vars, None) == eval_expr(*v2, labels, vars, None) {
                1
            } else {
                0
            }
        },

        Expression::GotoIf(name, cond) => {
            if eval_expr(*cond, labels, vars, None) != 0 {
                let index = *labels.get(&name).unwrap();
                new_ip = Some(index);
            }
            0
        }

        Expression::Set(name, val) => {
            let result = eval_expr(*val, labels, vars, None);
            vars.insert(name, result);
            0
        }

        Expression::Integer(i) => i,
        Expression::String(_) => 0,
        Expression::Identifier(name) => *vars.get(&name).unwrap()
    };

    if let Some(ip) = ip {
        if let Some(new_ip) = new_ip {
            *ip = new_ip;
        } else {
            *ip += 1;
        }
    }

    val
}

pub fn run((program, labels): (Vec<Expression>, HashMap<String, usize>)) {
    let mut ip = 0;

    let mut vars = HashMap::new();

    while ip < program.len() {
        eval_expr(program[ip].clone(), &labels, &mut vars, Some(&mut ip));
    }
}