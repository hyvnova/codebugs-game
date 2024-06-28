

mod parser;
mod operators;
mod compiler;
mod cpu;
//mod separated_by_save;

use parser::parser;
use chumsky::Parser;

use compiler::compile;
use cpu::{CPU,SysCall};

struct SC {}
impl SysCall for SC {}

fn main() {
    let src = std::fs::read_to_string("src/test.txt").unwrap();

    let parsed = parser().parse(src);
    println!("PARSED:\n{:#?}",parsed);

    let instr = compile(parsed.unwrap());
    println!("INSTR:\n{:#?}",instr);
    let instr=instr.unwrap();

    let mut cpu = CPU::<SC>::new(20);

    loop {
        println!("PC {}:\tMEM {:?}\n{:?}",cpu.memory[cpu.sp],cpu.memory,instr[cpu.memory[cpu.sp] as usize]);
        match cpu.execute(&instr) {
            Ok(Some(sc)) => println!("SYSCALL: {:?}",sc),
            Ok(None) => {}
            Err(e) => println!("ERROR: {:?}",e),
        }
    }

}
