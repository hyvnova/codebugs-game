

mod parser;
mod operators;
//mod separated_by_save;

use parser::parser;
use chumsky::Parser;

fn main() {
    let src = std::fs::read_to_string("src/test.txt").unwrap();
<<<<<<< HEAD
    println!("{:?}", exprparser().parse(src));
=======
    println!("{:#?}",parser().parse(src));
>>>>>>> 56fbececa6bc65de5a73274eaa007ce3a8c88c54
}
