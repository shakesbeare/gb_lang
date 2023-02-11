use crate::ast::{Expr, Op};
use crate::gb_type::{variant_eq, GbType};
use crate::scope::Scope;

pub fn evaluate(input: Expr, current_scope: &mut Scope) -> GbType {
    match input {
        Expr::Integer(x) => GbType::Integer(x),
        Expr::Float(x) => GbType::Float(x),
        Expr::String(x) => GbType::String(x),
        Expr::Boolean(x) => GbType::Boolean(x),
        Expr::Identifier(x) => {
            let Some(value) = current_scope.lookup(&x) else {
                println!("The identifier {} has not yet been defined", x);
                return GbType::Typeless(x);
            };
            Clone::clone(value)
        }
        Expr::UnaryMinus(x) => evaluate(*x, current_scope) * GbType::Integer(-1),
        Expr::BinOp { lhs, op, rhs } => {
            let mut left = GbType::Typeless("".to_string());
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
                        == std::mem::discriminant(&GbType::Typeless("".to_string()))
                    {
                        return GbType::Typeless(ident_name);
                    }

                    if current_scope.identifiers.contains_key(&ident_name) {
                        let test = current_scope.identifiers.get(&ident_name).unwrap();
                        if !variant_eq(test, &right) {
                            println!("ERROR: Wrong Type");
                            return GbType::Typeless(ident_name);
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
                return GbType::Typeless("".to_string());
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
                GbType::Typeless("".to_string())
            }
        }
        Expr::While { condition, expr } => {
            let mut last_result = None;
            while evaluate(*condition.clone(), current_scope) == GbType::Boolean(true) {
                let last_result = evaluate(*expr.clone(), current_scope);
            }

            if let Some(x) = last_result {
                return x;
            } else {
                return GbType::Typeless("".to_string());
            }
        }
        Expr::FunctionCall(ident) => {
            let expr_gb  = evaluate(*ident, current_scope);

            if let GbType::Function(expr) = expr_gb { 
                return evaluate(expr, current_scope);
            }
            
            GbType::Typeless("".to_string())
        }
        Expr::FnPrint => {
            println!("hello");
            GbType::Typeless("".to_string())
        }
        _ => {
            println!("{:?}", input);
            todo!()
        }
    }
}
