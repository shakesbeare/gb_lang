use crate::ast::{Expr, Op};
use crate::gb_type::{variant_eq, GbType};
use crate::scope::Scope;

pub fn init() -> Scope {
    let mut global_scope = Scope::init();
    global_scope.identifiers.insert(
        "print".to_string(),
        GbType::Function(Expr::FunctionDefinition {
            arg_types: vec![],
            arg_names: vec![],
            body: Box::new(Expr::Print),
        }),
    );

    return global_scope;
}
pub fn evaluate(input: Expr, current_scope: &mut Scope) -> GbType {
    match input {
        Expr::Integer(x) => GbType::Integer(x),
        Expr::Float(x) => GbType::Float(x),
        Expr::String(x) => GbType::String(x),
        Expr::Boolean(x) => GbType::Boolean(x),
        Expr::Identifier(x) => {
            let Some(value) = current_scope.lookup(&x) else {
                println!("The identifier {} has not yet been defined", x);
                return GbType::None;
            };
            Clone::clone(value)
        }
        Expr::UnaryMinus(x) => {
            evaluate(*x, current_scope) * GbType::Integer(-1)
        }
        Expr::BinOp { lhs, op, rhs } => {
            let mut left = GbType::None;
            let right = evaluate((*rhs).clone(), current_scope);
            if op != Op::Assign {
                left = evaluate((*lhs).clone(), current_scope);
            }

            match op {
                Op::Add => left + right,
                Op::Subtract => left - right,
                Op::Multiply => left * right,
                Op::Divide => left / right,
                Op::LessThan => GbType::Boolean(left < right),
                Op::GreaterThan => GbType::Boolean(left > right),
                Op::Modulo => {
                    todo!()
                }
                Op::Assign => {
                    let Expr::Identifier(ident_name) = *lhs else {
                        unreachable!()
                    };

                    if std::mem::discriminant(&right)
                        == std::mem::discriminant(&GbType::None)
                    {
                        return GbType::None;
                    }

                    if current_scope.identifiers.contains_key(&ident_name) {
                        let test =
                            current_scope.identifiers.get(&ident_name).unwrap();
                        if !variant_eq(test, &right) {
                            println!("ERROR: Wrong Type");
                            return GbType::None;
                        }
                    }
                    current_scope
                        .identifiers
                        .insert(ident_name.to_string(), right);
                    (&current_scope.identifiers.get(&ident_name).unwrap())
                        .clone()
                        .to_owned()
                }
            }
        }
        Expr::CompoundExpr(exprs) => {
            let mut last_result = None;
            for expr in exprs {
                if let Expr::Return(final_expr) = expr {
                    return evaluate(*final_expr, current_scope);
                }
                last_result = Some(evaluate(expr, current_scope));
            }

            if let Some(x) = last_result {
                return x;
            } else {
                return GbType::None;
            }
        }
        Expr::Return(expr) => evaluate(*expr, current_scope),
        Expr::If {
            condition,
            expr,
            els,
        } => {
            if evaluate(*condition, current_scope) == GbType::Boolean(true) {
                evaluate(*expr, current_scope)
            } else {
                if let Some(els_inner) = els {
                    return evaluate(*els_inner, current_scope);
                }
                GbType::None
            }
        }
        Expr::While { condition, expr } => {
            #[allow(unused_mut)]
            let mut last_result = None;
            while evaluate(*condition.clone(), current_scope)
                == GbType::Boolean(true)
            {
                #[allow(unused_variables)]
                let last_result = evaluate(*expr.clone(), current_scope);
            }

            if let Some(x) = last_result {
                return x;
            } else {
                return GbType::None;
            }
        }
        Expr::FunctionCall { identifier, args } => {
            let expr_gb = evaluate(*identifier, current_scope);
            let mut args_gb = vec![];

            for arg in args {
                args_gb.push(evaluate(arg, current_scope));
            }

            if let GbType::Function(expr) = expr_gb {
                let Some(x) = function_call(expr, args_gb, current_scope) else {
                    return GbType::None;
                };
                return x;
            }

            GbType::None
        }
        Expr::Print => {
            println!("hello");
            GbType::None
        }
        _ => {
            println!("{:?}", input);
            todo!()
        }
    }
}

#[allow(unused_variables)]
fn function_call(
    expr: Expr,
    args: Vec<GbType>,
    outer_scope: &mut Scope,
) -> Option<GbType> {
    let Expr::FunctionDefinition { arg_types, arg_names, body } = expr else {
        return None;
    };

    if !(args.len() == arg_types.len()) {
        return None;
    }

    let mut scope = Scope::init();

    for (i, arg) in args.iter().enumerate() {
        if variant_eq(arg, &arg_types[i]) {
            return None;
        } else {
            scope.identifiers.insert(arg_names[i].clone(), arg.clone());
        }
    }

    return Some(evaluate(*body, &mut scope));
}
