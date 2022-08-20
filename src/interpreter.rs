use crate::parser::AstNode;
use crate::parser::BinaryOp;
use crate::parser::UnaryOp;
use std::collections::HashMap;

pub fn evaluate(ast: Vec<AstNode>) {
    let mut global_scope: HashMap<String, String> = HashMap::new();

    for ast_node in ast {
        match ast_node {
            AstNode::BinaryExpression { op, left, right } => match op {
                BinaryOp::Assign => {
                    let new_identifier = left;
                    let value = right;

                    let mut ident = "".to_string();
                    if let AstNode::Identifier(x) = *new_identifier {
                        ident = x;
                    }

                    match *value {
                        AstNode::Float(x) => {
                            global_scope.insert(ident, x.to_string());
                        }
                        AstNode::Integer(x) => {
                            global_scope.insert(ident, x.to_string());
                        }
                        AstNode::String(x) => {
                            global_scope.insert(ident, x);
                        }
                        x => panic!("Cannot assign variable to type {}", x),
                    }
                }
                BinaryOp::Add => {
                    todo!()
                }
                _ => todo!(),
            },
            AstNode::UnaryExpression { op, val } => {
                todo!()
            }
            _ => todo!(),
        }
    }

    println!("{:?}", global_scope);
}
