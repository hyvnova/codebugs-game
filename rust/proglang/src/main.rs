

mod parser;
//mod separated_by_save;

use parser::parser;
use chumsky::Parser;

fn main() {
    let src = std::fs::read_to_string("src/test.txt").unwrap();
    println!("{:#?}",parser().parse(src));
}
