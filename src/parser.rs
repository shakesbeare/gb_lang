#![allow(deprecated)]

use crate::ast::{Expr, Op};
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::Assoc::*;
use pest::prec_climber::{Operator, PrecClimber};

#[derive(Parser)]
#[grammar = "gb.pest"]
pub struct GbParser;

lazy_static::lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;

        PrecClimber::new(vec![
            Operator::new(assign, Right),
            Operator::new(less_than, Left) | Operator::new(greater_than, Left),
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left) | Operator::new(modulo, Left),
        ])
    };
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PREC_CLIMBER.climb(
        pairs,
        |pair: Pair<Rule>| match pair.as_rule() {
            _ => parse_one(pair),
        },
        |lhs: Expr, op: Pair<Rule>, rhs: Expr| {
            let op = match op.as_rule() {
                Rule::add => Op::Add,
                Rule::subtract => Op::Subtract,
                Rule::multiply => Op::Multiply,
                Rule::divide => Op::Divide,
                Rule::modulo => Op::Modulo,
                Rule::assign => Op::Assign,
                Rule::less_than => Op::LessThan,
                Rule::greater_than => Op::GreaterThan,
                rule => unreachable!(
                    "Expr::parse expected infix operation, found {:?}",
                    rule
                ),
            };
            Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        },
    )
}

fn parse_one(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::integer => Expr::Integer(pair.as_str().parse::<i64>().unwrap()),
        Rule::float => {
            let pair = pair.as_str();
            if pair.chars().last().unwrap() == 'f' {
                let mut chars = pair.chars();
                chars.next_back();
                Expr::Float(chars.as_str().parse::<f64>().unwrap())
            } else {
                Expr::Float(pair.parse::<f64>().unwrap())
            }
        }
        Rule::identifier => Expr::Identifier(pair.as_str().to_string()),
        Rule::boolean => Expr::Boolean(pair.as_str().parse::<bool>().unwrap()),
        Rule::expr => parse_expr(pair.into_inner()),
        Rule::compound_expr => {
            let mut exprs: Vec<Expr> = vec![];

            let pairs_inner = pair.into_inner();

            for x in pairs_inner {
                exprs.push(parse_expr(x.into_inner()));
            }

            Expr::CompoundExpr(exprs)
        }
        Rule::unary_minus => {
            Expr::UnaryMinus(Box::new(parse_expr(pair.into_inner())))
        }
        Rule::return_expr => {
            Expr::Return(Box::new(parse_expr(pair.into_inner())))
        }
        Rule::if_expr => {
            let inner: Vec<Pair<Rule>> = pair.into_inner().collect();
            let condition = inner[0].clone();
            let expr = inner[1].clone();
            let els = if inner.len() == 3 {
                Some(inner[2].clone())
            } else {
                None
            };

            Expr::If {
                condition: Box::new(parse_expr(condition.into_inner())),
                expr: Box::new(parse_expr(expr.into_inner())),
                els: {
                    match els {
                        None => None,
                        Some(x) => Some(Box::new(parse_expr(x.into_inner()))),
                    }
                },
            }
        }
        Rule::while_expr => {
            let inner: Vec<Pair<Rule>> = pair.into_inner().collect();
            let condition = inner[0].clone();
            let expr = inner[1].clone();

            Expr::While {
                condition: Box::new(parse_expr(condition.into_inner())),
                expr: Box::new(parse_expr(expr.into_inner())),
            }
        }
        Rule::call => {
            let inner: Vec<Pair<Rule>> = pair.into_inner().collect();
            let identifier = inner[0].clone();
            let args_pair = inner[1].clone();
            let mut args: Vec<Expr> = vec![];

            for arg_pair in args_pair.into_inner() {
                args.push(parse_one(arg_pair));
            }

            Expr::FunctionCall {
                identifier: Box::new(parse_one(identifier)),
                args,
            }
        }
        rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
    }
}
