use pest::error::Error;
use pest::iterators::Pair;
use pest::Parser;
use std::fmt;
use std::fs;

#[derive(Parser)]
#[grammar = "gb.pest"]
pub struct GbParser;

#[derive(Debug)]
#[allow(dead_code)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,

    Assign, // =

    IsLessThan,
    IsLessThanEqualTo,
    IsGreaterThan,
    IsGreaterThanEqualTo,
    IsEqualTo, // ==
    IsNotEqualTo,

    And,
    Or,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum AstNode {
    Identifier(String),
    String(String),
    Integer(i64),
    Float(f64),

    BinaryExpression {
        op: BinaryOp,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    UnaryExpression {
        op: UnaryOp,
        val: Box<AstNode>,
    },
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn parse(filename: &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let unparsed_file = fs::read_to_string(filename).unwrap();
    let mut ast: Vec<AstNode> = vec![];
    let pairs = GbParser::parse(Rule::program, &unparsed_file)?;

    for pair in pairs {
        match pair.as_rule() {
            Rule::varDecl => ast.push(build_ast_from_var_decl(pair)),
            Rule::funDecl => {}
            _ => {}
        }
    }

    Ok(ast)
}

fn build_ast_from_var_decl(pair: Pair<Rule>) -> AstNode {
    let mut pair = pair.into_inner();
    let _decl_kw = pair.next().unwrap().as_str();

    let var_decl_idents = pair.next().unwrap();
    let var_decl_assign = pair.next().unwrap();

    let var_decl_idents_inner = var_decl_idents.into_inner().next().unwrap();

    let mut identifiers: Vec<&str> = vec![];
    let mut type_hint: Option<&str> = None;
    let mut expr: Option<AstNode> = None;

    match var_decl_idents_inner.as_rule() {
        Rule::identifier => {
            // only one identifier in this assignment expression
            identifiers.push(var_decl_idents_inner.as_str());
        }
        Rule::varDeclParen => {}
        _ => unreachable!("var_decl_indents_inner"),
    }

    let mut var_decl_assign_inner = var_decl_assign.into_inner();
    let first = var_decl_assign_inner.next().unwrap();
    match first.as_rule() {
        Rule::typeHint => {
            let mut first_inner = first.into_inner();
            first_inner.next();
            type_hint = Some(first_inner.next().unwrap().as_str());
        }
        Rule::equal => {
            type_hint = None;
            let expr_stmt = var_decl_assign_inner.next().unwrap();
            expr = parse_expression_statement(expr_stmt);
        }
        _ => unreachable!("var_decl_assign_inner"),
    }

    AstNode::BinaryExpression {
        op: BinaryOp::Assign,
        left: Box::new(AstNode::Identifier(identifiers[0].to_string())),
        right: Box::new(expr.unwrap()),
    }
}

fn parse_expression_statement(pair: Pair<Rule>) -> Option<AstNode> {
    println!("{:#?}", pair);
    let mut pairs = pair.into_inner();

    let mut pair = pairs.next().unwrap();

    match pair.as_rule() {
        Rule::exp => return parse_expression_statement(pair),
        Rule::simpleExp => {
            pair = pair.into_inner();

            match pair.count() {
                1 => return parse_expression_statement(pair),
                _ => todo!(),
            }
        }
        // Rule::andExp => {}
        // Rule::unaryRelExp => {}
        // Rule::relExp => {}
        // Rule::sumExp => {}
        // Rule::mulExp => {}
        // Rule::unaryExp => {}
        // Rule::factor => {}
        // Rule::mutable => {}
        // Rule::immutable => {}
        // Rule::call => {}
        // Rule::args => {}
        // Rule::argList => {}
        // Rule::constant => {}
        // Rule::integer => {}
        // Rule::decimal => {}
        // Rule::string => {}
        // Rule::identifier => {}
        _ => unreachable!("Unable to parse expression"),
    }
    todo!()
}
