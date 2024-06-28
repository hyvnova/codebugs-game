


use crate::compiler::{Instr,/*FnArg,*/FnArgMode,StackRef,Reg};
// use crate::operators::{BinaryOperator,UnaryOperator};
use std::marker::PhantomData;

type Error = String;

pub struct CPU<SC: SysCall> {
    pub memory: Vec<i32>,
    pub sp: usize, /* stack pointer == stack size-1; index of top of stack */
    phantom: PhantomData<SC>,
}


impl<SC:SysCall> CPU<SC> {
    // init
    pub fn new(stacksize:usize) -> Self {
        Self {
            memory: vec![0;stacksize],
            sp: 0, //TODO: 1 or 0? idk
            phantom: PhantomData,
        }
    }

    // main important thing: instruction execution
    pub fn execute(&mut self, program: &Vec<Instr<usize>>) -> Result<Option<()>,Error> {
        // get the PC
        let pc = self.memory[self.sp] as usize;
        
        // get instruction from program memory
        let instr = &program[pc];
        self.memory[self.sp]+=1;

        // execute instruction
        match instr {
            Instr::BinaryOperator { op, lhs, rhs, res } => 
                self.set_val(*res,op.eval(self.get_val(*lhs), self.get_val(*rhs))?),
            Instr::UnaryOperator { op, rhs, res } => 
                self.set_val(*res,op.eval(self.get_val(*rhs))),
            Instr::ArrayIndex { array, index, res } => {
                let (pos,len) = self.get_array(*array);
                let index = self.get_val(*index);
                if index as usize>=len {self.memory[self.sp]+=1;return Err(format!("Index {index} out of range for array with length {len}"))}
                self.set_val(*res, self.memory[(pos+index) as usize]);
            }
            Instr::ArrayAssign { array, index, value } => {
                let (pos,len) = self.get_array(*array);
                let index = self.get_val(*index);
                if index as usize>=len {self.memory[self.sp]+=1;return Err(format!("Index {index} out of range for array with length {len}"))}
                self.memory[(pos+index) as usize] = self.get_val(*value);
            }
            Instr::Jump { index } =>
                self.memory[self.sp]=*index as i32,
            Instr::JumpUnless { index, condition } =>
                if self.get_val(*condition)!=0 {self.memory[self.sp]=*index as i32},
            Instr::FnCall { index, params, res, stack } => {
                let values: Vec<i32> = params.iter().map(|fnarg| match fnarg.mode {
                    FnArgMode::Reference => self.get_ref(fnarg.reg),
                    FnArgMode::Value => self.get_val(fnarg.reg),
                }).collect();
                self.sp+=stack;
                self.memory[self.sp]=*index as i32;
                self.memory[self.sp-1]=self.get_ref(*res);
                for i in 0..values.len() {
                    self.memory[self.sp-2-i] = values[i];
                }
            }
            Instr::SystemCall {  } => {
                todo!();
                return Ok(Some(()))
            }
            Instr::Return { stack, value } => {
                let value = self.get_val(*value);
                self.set_val(Reg::VarRef(StackRef::Rel(-1)),value);
                self.sp-=stack;
            }
            Instr::Init { stack, index } => {
                self.sp+=stack;
                self.memory[self.sp]=*index as i32-1;
            }
        }

        Ok(None)

    }

    // memory operations
    pub fn get_ref(&self, reg:Reg) -> i32 {
        // get reference to variable
        match reg {
            Reg::Const(_) => 0, /* ERROR */
            Reg::Var(sr) => self.stack_index(sr) as i32,
            Reg::VarRef(sr) => self.memory[self.stack_index(sr)], /* might cause ERROR */
            Reg::Array(sr, len) => (self.stack_index(sr) | (len<<16)) as i32,
            Reg::ArrayRef(sr) => self.memory[self.stack_index(sr)],
        }
    }
    pub fn get_val(&self, reg:Reg) -> i32 {
        // get value
        match reg {
            Reg::Const(x) => x,
            Reg::Var(sr) => self.memory[self.stack_index(sr)],
            Reg::VarRef(sr) => self.memory[self.memory[self.stack_index(sr)] as usize],
            Reg::Array(_, _) => panic!(),
            Reg::ArrayRef(_) => panic!(),
        }
    }
    pub fn set_val(&mut self, reg:Reg, value:i32) {
        // set value----
        match reg {
            Reg::Const(x) => {}, // storing in CONST is a safe thing to do
            Reg::Var(sr) => {
                let index=self.stack_index(sr);
                self.memory[index] = value
            },
            Reg::VarRef(sr) => {
                let index=self.stack_index(sr);
                let index=self.memory[index] as usize;
                self.memory[index] = value
            },
            Reg::Array(_, _) => panic!(),
            Reg::ArrayRef(_) => panic!(),
        }
    }

    pub fn get_array(&self, reg:Reg) -> (i32,usize) {
        // get reference to array, split into memory location and length
        match reg {
            Reg::Const(x) => panic!(),
            Reg::Var(sr) => panic!(),
            Reg::VarRef(sr) => panic!(),
            Reg::Array(sr, len) => (self.stack_index(sr) as i32,len),
            Reg::ArrayRef(sr) => {
                let p = self.memory[self.stack_index(sr)];
                (p&((1<<16)-1), (p as usize)>>16)
            },
        }
    }

    //stack indexing
    fn stack_index(&self, refr:StackRef) -> usize {
        match refr {
            StackRef::Abs(x) => x as usize,
            StackRef::Rel(rel) => (self.sp as i32+rel) as usize,
        }
    }
}






pub trait SysCall {


}