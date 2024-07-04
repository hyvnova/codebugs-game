use log::LevelFilter;

mod parser;
mod operators;
mod compiler;
mod simple_cpu;
mod advanced_cpu;
//mod separated_by_save;

use parser::parser;
use chumsky::Parser;

use compiler::SysCallParamCheck;
use advanced_cpu::{CPU,BuiltinOrSysCall};

#[derive(Debug,Clone)]
enum SC {
    Print
}

fn main() {
    env_logger::builder().filter_level(LevelFilter::Debug).init();

    let src = std::fs::read_to_string("src/test.txt").unwrap();

    let parsed = parser().parse(src);
    println!("PARSED:\n{:#?}",parsed);

    let instr = CPU::compiler()
        .create_basescope("@sys".to_string())
        .add_syscall("print",BuiltinOrSysCall::SysCall(SC::Print),SysCallParamCheck::Runtime)
        .compile(parsed.unwrap());
    println!("INSTR:\n{:#?}",instr);
    let instr=instr.unwrap();

    let mut cpu = CPU::<SC>::new(20);

    loop {
        println!("PC {}\tSP {}\tMEM {:?}\n{:?}\n",cpu.cpu.memory[cpu.cpu.sp],cpu.cpu.sp,cpu.cpu.memory,instr[cpu.cpu.memory[cpu.cpu.sp] as usize]);
        match cpu.execute(&instr) {
            Ok(Some(sc)) => println!("SYSCALL: {:?}",sc),
            Ok(None) => {}
            Err(e) => println!("ERROR: {:?}",e),
        }
    }

}
