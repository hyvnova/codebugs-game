

mod parser;
mod separated_by_save;

use parser::exprparser;
use chumsky::Parser;

fn main() {
    let src = std::fs::read_to_string("src/test.txt").unwrap();
    println!("{:?}",exprparser().parse(src));
}
