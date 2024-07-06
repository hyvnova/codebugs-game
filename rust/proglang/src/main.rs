use log::LevelFilter;

mod parser;
mod operators;
mod compiler;
mod simple_cpu;
mod advanced_cpu;
use parser::parser;
use chumsky::Parser;

use compiler::{SysCallParamCheck,ProgramMeta};
use advanced_cpu::{CPU,BuiltinOrSysCall};
use simple_cpu::MemMetaVariant;

#[derive(Debug,Clone)]
enum SC {
    Print
}

fn main() {
    env_logger::builder().filter_level(LevelFilter::Debug).init();

    let src = std::fs::read_to_string("src/test.txt").unwrap();

    let parsed = parser().parse(src);
    println!("PARSED:\n{:#?}",parsed);

    let mut compiler = CPU::compiler();
    compiler
        .create_basescope("@sys".to_string())
        .add_syscall("print",BuiltinOrSysCall::SysCall(SC::Print),SysCallParamCheck::Runtime);
    let (instr,meta) = compiler.compile(parsed.unwrap()).unwrap();
    println!("INSTR:\n{:#?}",instr);
    println!("META:\n{:#?}",meta);

    let mut cpu = CPU::<SC>::new(20);

    loop {
        // println!("PC {}\tSP {}\tMEM {:?}\n{:?}\n",cpu.cpu.memory[cpu.cpu.sp],cpu.cpu.sp,cpu.cpu.memory,instr[cpu.cpu.memory[cpu.cpu.sp] as usize]);
        display_stack(&cpu,&meta);
        
//        println!("\n");
        println!("\n{:?}\n{:?}\n",instr[cpu.cpu.memory[cpu.cpu.sp] as usize],meta.instr[cpu.cpu.memory[cpu.cpu.sp] as usize]);


        match cpu.execute(&instr) {
            Ok(Some(sc)) => println!("SYSCALL: {:?}",sc),
            Ok(None) => {}
            Err(e) => println!("ERROR: {:?}",e),
        }
        std::thread::sleep(std::time::Duration::from_millis(3000));
    }

}

fn display_stack<SC:std::fmt::Debug + Clone>(cpu:&CPU<SC>,meta:&ProgramMeta) {
    let data = cpu.cpu.annotate_stack(meta);
    println!("==================== STACK ====================");
    println!("ADDRESS\tNAME\tTYPE\tVALUE");
    for i in 0..data.len() {
        println!("\x1B[{}m{i:#06X}\t{}\t{}\t{}\x1B[m",
            match data[i].variant {
                MemMetaVariant::PC          => "93", // yellow
                MemMetaVariant::Result      => "38;5;130", // orange
                MemMetaVariant::Var         => "97", // white
                MemMetaVariant::VarRef      => "95", // magenta
                MemMetaVariant::Array       => "92", // green
                MemMetaVariant::ArrayRef    => "96", // cyan
                MemMetaVariant::TMP         => "38;5;19", // dark blue
                MemMetaVariant::Unused      => "38;5;236", // dark gray
            },
            data[i].name,
            data[i].r#type.as_ref().map(|s|s.as_str()).unwrap_or_else(|| "-"),
            data[i].value
        );
    }
}
