use crate::parser::{CompilationUnit, Declaration, DeclarationKind, Expr, ExprData, TypeExprData};
use crate::lexer::{BinOp, UnOp};

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Validator {
    var_types: Vec<HashMap<String, TypeExprData>>,
}            

impl Validator {
    fn new() -> Validator {
        Validator {
            var_types: vec![HashMap::new()]
        }
    }

    fn current_scope(&self) -> &HashMap<String, TypeExprData> {
        self.var_types.iter().last().unwrap()
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, TypeExprData> {
        self.var_types.iter_mut().last().unwrap()
    }

    fn push_scope(&mut self) {
        self.var_types.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.var_types.pop().unwrap();
    }

    fn declare_var(&mut self, name: String, typ: TypeExprData) {
        self.current_scope_mut()
            .insert(name, typ);
    }

    fn get_var_type(&mut self, name: &String) -> TypeExprData {
        self.current_scope()
            .get(name)
            .cloned()
            .unwrap()
    }

    fn fill_types(&mut self, expr: &mut Expr) {
        expr.typ = match &mut expr.data {
            ExprData::BinOp(op, n1, n2) => {
                self.fill_types(n1);
                self.fill_types(n2);

                None
            }
            ExprData::UnOp(op, n) => {
                self.fill_types(n);
                None
            }
            ExprData::DoBlock(body) => {
                for expr in body {
                    self.fill_types(expr);
                }
                None
            }
            ExprData::DoLoopIf(cond, body) => {
                self.fill_types(cond);
                
                for expr in body {
                    self.fill_types(expr);
                }
                None
            }
            ExprData::DoWhile(cond, body) => {
                self.fill_types(cond);

                for expr in body {
                    self.fill_types(expr);
                }
                None
            }
            ExprData::Print(val) => {
                self.fill_types(val);
                None
            }
            ExprData::PrintType(expr) => {
                self.fill_types(expr);
                None
            }
            ExprData::Set(var, val) => {
                self.fill_types(val);
                None
            },

            ExprData::Ident(n) => {
                Some(self.get_var_type(n))
            }
            
            _ => None
        };
    }
}

pub fn validate(unit: &mut CompilationUnit) {
    let mut validator = Validator::new();

    for Declaration { kind, .. } in unit.decls.iter() {
        match kind {
            DeclarationKind::Type { var, typ } => validator.declare_var(var.clone(), typ.data),
            _ => ()
        }
    }

    eprintln!("var types: {:#?}", validator.var_types);

    for Declaration { kind, .. } in unit.decls.iter_mut() {
        match kind {
            DeclarationKind::Func { body, .. } => {
                for expr in body {
                    validator.fill_types(expr);
                }
            },
            _ => ()
        }
    }
}