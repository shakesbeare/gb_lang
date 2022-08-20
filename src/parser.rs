use pest::Parser;
use std::fs;

#[derive(Parser)]
#[grammar = "gb.pest"]
pub struct GbParser;

pub fn parse(filename: &str) {
    let unparsed_file = fs::read_to_string(filename).unwrap();

    let pairs = GbParser::parse(Rule::program, &unparsed_file).unwrap();

    println!("{:?}", pairs);
}
