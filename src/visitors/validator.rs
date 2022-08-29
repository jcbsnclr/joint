use crate::parser::{CompilationUnit, Declaration, DeclarationKind, Expr, ExprData};

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

#[derive(Debug, Clone)]
pub enum Type {
    Concrete(IntrinsicType),
    Abstract(AbstractType),
    Function {
        args: Vec<Type>
    }
}

impl Type {

}

impl PartialEq<Type> for Type {
    fn eq(&self, rhs: &Type) -> bool {
        use IntrinsicType as I;
        use AbstractType as A;

        match self {
            Type::Abstract(A::Integer) => matches!(
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
            ) => matches!(rhs, Type::Abstract(A::Integer)),

            Type::Concrete(I::String) => matches!(rhs, Type::Concrete(I::String)),

            Type::Function { args } => match rhs {
                Type::Function { args: rhs_args} => {
                    args.iter()
                        .zip(rhs_args.iter())
                        .map(|(l,r)| l == r)
                        .fold(true, |res, test| res && test)
                }
                _ => false
            }
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

    fn current_scope_mut(&mut self) -> &mut HashMap<String, Type> {
        self.var_types.iter_mut().last().unwrap()
    }

    // TODO: var and type decls outside of the top level
    // 
    // fn push_scope(&mut self) {
    //     self.var_types.push(HashMap::new());
    // }

    // fn pop_scope(&mut self) {
    //     self.var_types.pop().unwrap();
    // }

    fn declare_var(&mut self, name: String, typ: Type) {
        self.current_scope_mut()
            .insert(dbg!(name), dbg!(typ));
    }

    fn get_var_type(&mut self, name: &String) -> Type {
        dbg!(name);
        self.var_types.iter()
            .flatten()
            .map(|(lhs, rhs)| (lhs.clone(), rhs.clone()))
            .collect::<HashMap<String, Type>>()
            .get(name)
            .cloned()
            .unwrap()
    }

    fn fill_types(&mut self, expr: &mut Expr) {
        expr.typ = match &mut expr.data {
            ExprData::BinOp(_,n1, n2) => {
                self.fill_types(n1);
                self.fill_types(n2);

                if n1.typ != Some(Type::Abstract(AbstractType::Integer)) || n1.typ != n2.typ {
                    panic!("BinOp: type mismatch: {:?}, {:?}", n1, n2);
                }

                n1.typ.clone()
            }
            ExprData::UnOp(_,n) => {
                self.fill_types(n);

                if n.typ != Some(Type::Abstract(AbstractType::Integer)) {
                    panic!("UnOp: type mismatch: {:?}", n.typ);
                }

                n.typ.clone()
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

                if Some(var_type.clone()) != val.typ {
                    panic!("Set: attempted to assign value of type `{:?}` to variable of type `{:?}`", val.typ, var_type);
                }

                None
            },

            ExprData::Get(n) => {
                Some(self.get_var_type(n))
            }

            ExprData::Integer(_) => {
                Some(Type::Abstract(AbstractType::Integer))
            }

            ExprData::TypeCast(e, t) => {
                self.fill_types(e);

                if e.typ != Some(t.clone()) {
                    panic!("TypeCast: invalid cast ({:?} -> {:?})", e.typ, t);
                }

                Some(t.clone())
            },

            ExprData::StringLit(_) => Some(Type::Concrete(IntrinsicType::String)),

            ExprData::FnCall(name, args) => {
                for arg in args.iter_mut() {
                    self.fill_types(arg);
                }

                let found_type = Type::Function { 
                    args: args.iter()
                        .map(|a| a.typ.clone())
                        .filter_map(|t| t)
                        .collect()
                };

                let fn_type = self.get_var_type(&name.clone());

                if found_type == fn_type {
                    dbg!(name);
                    dbg!(args);
                    None
                } else {
                    panic!("FnCall ({}): type mismatch\nexpected `{:?}`, found `{:?}`)", name, fn_type, found_type)
                }
            }

            ExprData::Var(_, _) => None,
        };
    }
}

pub fn validate(unit: &mut CompilationUnit) {
    let mut validator = Validator::new();

    for Declaration { kind, .. } in unit.decls.iter() {
        match kind {
            DeclarationKind::Type { var, typ } => validator.declare_var(var.clone(), typ.clone()),
            _ => ()
        }
    }

    for Declaration { kind, .. } in unit.decls.iter_mut() {
        match kind {
            DeclarationKind::Func { name, body, args, .. } => {
                let type_info = validator.get_var_type(name);
                
                if let Type::Function { args: arg_types } = type_info {
                    let vs = args.iter()
                        .zip(arg_types.iter());

                    for (k,v) in vs {
                        validator.declare_var(k.clone(), v.clone())
                    }
                }

                for expr in body {
                    validator.fill_types(expr);
                }
            },
            _ => ()
        }
    }

    eprintln!("var types: {:#?}", validator.var_types);
}