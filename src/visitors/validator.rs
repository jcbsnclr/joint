use crate::parser::{CompilationUnit, Declaration, DeclarationKind, Expr, ExprData, TypeExpr, TypeExprData};
use crate::lexer::{BinOp, UnOp};

use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
pub enum AbstractType {
    Integer
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IntrinsicType {
    U8, I8,
    U16, I16,
    U32, I32,
    U64, I64,
    String
}

#[derive(Debug, Copy, Clone)]
pub enum Type {
    Concrete(IntrinsicType),
    Abstract(AbstractType)
}

impl Type {
    fn from_type_expr(expr: &TypeExpr) -> Type {
        Type::Concrete(match expr.data {
            TypeExprData::U8 => IntrinsicType::U8,
            TypeExprData::I8 => IntrinsicType::I8,

            TypeExprData::U16 => IntrinsicType::U16,
            TypeExprData::I16 => IntrinsicType::I16,

            TypeExprData::U32 => IntrinsicType::U32,
            TypeExprData::I32 => IntrinsicType::I32,

            TypeExprData::U64 => IntrinsicType::U64,
            TypeExprData::I64 => IntrinsicType::I64,

            TypeExprData::String => IntrinsicType::String
        })
    }
}

impl PartialEq<Type> for Type {
    fn eq(&self, rhs: &Type) -> bool {
        use IntrinsicType as I;
        use AbstractType as A;

        match self {
            Type::Abstract(AbstractType::Integer) => matches!(
                rhs, 
                Type::Concrete(
                    I::U8 | I::I8 | 
                    I::U16 | I::I16 | 
                    I::U32 | I::I32 | 
                    I::U64 | I::I64
                )
            ),

            Type::Concrete(
                I::U8 | I::I8 | 
                I::U16 | I::I16 | 
                I::U32 | I::I32 | 
                I::U64 | I::I64
            ) => matches!(rhs, Type::Abstract(AbstractType::Integer)),

            Type::Concrete(I::String) => matches!(rhs, Type::Concrete(I::String))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Validator {
    var_types: Vec<HashMap<String, Type>>,
}            

impl Validator {
    fn new() -> Validator {
        Validator {
            var_types: vec![HashMap::new()]
        }
    }

    fn current_scope(&self) -> &HashMap<String, Type> {
        self.var_types.iter().last().unwrap()
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, Type> {
        self.var_types.iter_mut().last().unwrap()
    }

    fn push_scope(&mut self) {
        self.var_types.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.var_types.pop().unwrap();
    }

    fn declare_var(&mut self, name: String, typ: Type) {
        self.current_scope_mut()
            .insert(name, typ);
    }

    fn get_var_type(&mut self, name: &String) -> Type {
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

                if n1.typ != Some(Type::Abstract(AbstractType::Integer)) || n1.typ != n2.typ {
                    panic!("BinOp: type mismatch: {:?}, {:?}", n1, n2);
                }

                n1.typ
            }
            ExprData::UnOp(op, n) => {
                self.fill_types(n);

                if n.typ != Some(Type::Abstract(AbstractType::Integer)) {
                    panic!("UnOp: type mismatch: {:?}", n.typ);
                }

                n.typ
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

                let var_type = self.get_var_type(var);

                if Some(var_type) != val.typ {
                    panic!("Set: attempted to assign value of type `{:?}` to variable of type `{:?}`", val.typ, var_type);
                }

                None
            },

            ExprData::Ident(n) => {
                Some(self.get_var_type(n))
            }

            ExprData::Integer(_) => {
                Some(Type::Abstract(AbstractType::Integer))
            }

            ExprData::TypeCast(e, t) => {
                self.fill_types(e);

                let typ = Type::from_type_expr(t);

                if e.typ != Some(typ) {
                    panic!("TypeCast: invalid cast ({:?} -> {:?})", e.typ, t);
                }

                Some(typ)
            },

            ExprData::StringLit(_) => Some(Type::Concrete(IntrinsicType::String)),

            ExprData::Var(_, _) => None,
        };
    }
}

pub fn validate(unit: &mut CompilationUnit) {
    let mut validator = Validator::new();

    for Declaration { kind, .. } in unit.decls.iter() {
        match kind {
            DeclarationKind::Type { var, typ } => validator.declare_var(var.clone(), Type::from_type_expr(typ)),
            _ => ()
        }
    }

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

    eprintln!("var types: {:#?}", validator.var_types);
}