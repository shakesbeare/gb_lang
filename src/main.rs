extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::error::Error;
use pest::Parser;
use std::fs;

#[derive(Parser)]
#[grammar = "gb.pest"]
pub struct GbParser;

#[derive(Debug)]
pub enum MonadicVerb {
    Negate,
}

#[derive(Debug)]
pub enum DyadicVerb {
    Add,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Equal,
    Minus,
}

#[derive(Debug)]
pub enum AstNode {
    Identifier(String),
    Number(f64),
    Str(String),
    MonadicOp {
        verb: MonadicVerb,
        expr: Box<AstNode>,
    },
    DyadicOp {
        verb: DyadicVerb,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    Terms(Vec<AstNode>),
    NewAssignment {
        ident: String,
        expr: Box<AstNode>,
    },
    Assignment {
        ident: String,
        expr: Box<AstNode>,
    },
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        println!("Invalid number of arguments");
        std::process::exit(1);
    }

    let filename = &args[1];
    let unparsed_file = fs::read_to_string(filename).unwrap();

    let astnode = parse(&unparsed_file).expect("unsuccessful parse");

    println!("{:?}", &astnode);
}

pub fn parse(source: &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let mut ast = vec![];

    let pairs = GbParser::parse(Rule::program, source)?;
    for pair in pairs {
        match pair.as_rule() {
            Rule::expr => {
                ast.push(build_ast_from_expr(pair));
            }
            _ => {}
        }
    }

    Ok(ast)
}

pub fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::term => build_ast_from_term(pair.into_inner().next().unwrap()),
        Rule::monadicExpression => {
            let mut pair = pair.into_inner();
            let verb = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            parse_monadic_verb(verb, expr)
        }
        Rule::dyadicExpression => {
            let mut pair = pair.into_inner();
            let verb = pair.next().unwrap();

            let left = pair.next().unwrap();
            let left = build_ast_from_expr(left);

            let right = pair.next().unwrap();
            let right = build_ast_from_expr(right);
            parse_dyadic_verb(verb, left, right)
        }
        Rule::initializationExpression => {
            let mut pair = pair.into_inner();
            let _ = pair.next().unwrap(); // ignore "let" keyword
            let ident = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);

            AstNode::NewAssignment {
                ident: ident.to_string(),
                expr: Box::new(expr),
            }
        }
        Rule::assignmentExpression => {
            let mut pair = pair.into_inner();

            let ident = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);

            AstNode::Assignment {
                ident: ident.to_string(),
                expr: Box::new(expr),
            }
        }
        unknown_expression => panic!("Unexpected expression: {:?}", unknown_expression),
    }
}

pub fn parse_monadic_verb(pair: pest::iterators::Pair<Rule>, expr: AstNode) -> AstNode {
    AstNode::MonadicOp {
        verb: match pair.as_str() {
            "!" => MonadicVerb::Negate,
            _ => panic!("Unsupported monadic verb: {:?}", pair.as_str()),
        },
        expr: Box::new(expr),
    }
}

pub fn parse_dyadic_verb(
    pair: pest::iterators::Pair<Rule>,
    left: AstNode,
    right: AstNode,
) -> AstNode {
    AstNode::DyadicOp {
        left: Box::new(left),
        right: Box::new(right),
        verb: match pair.as_str() {
            "+" => DyadicVerb::Add,
            "*" => DyadicVerb::Multiply,
            "-" => DyadicVerb::Minus,
            "/" => DyadicVerb::Divide,
            "<" => DyadicVerb::LessThan,
            ">" => DyadicVerb::GreaterThan,
            "=" => DyadicVerb::Equal,
            _ => panic!("Unexpected dyadic verb: {}", pair.as_str()),
        },
    }
}

pub fn build_ast_from_term(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::number => {
            let nstr = pair.as_str();

            let number: f64 = nstr.parse().unwrap();
            AstNode::Number(number)
        }
        Rule::expr => build_ast_from_expr(pair),
        Rule::identifier => AstNode::Identifier(String::from(pair.as_str())),
        unknown_term => panic!("Unexpected term: {:?}", unknown_term),
    }
}
