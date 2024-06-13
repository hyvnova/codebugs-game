

mod parser;
mod operators;
mod compiler;
//mod separated_by_save;

use parser::parser;
use compiler::compile;
use chumsky::Parser;

fn main() {
    let src = std::fs::read_to_string("src/test.txt").unwrap();
    let parsed = parser().parse(src);
    println!("PARSED:\n{:#?}",parsed);
    let instr = compile(parsed.unwrap());
    println!("INSTR:\n{:#?}",instr);
}
