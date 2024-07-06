


use crate::compiler::{Instr,FnArg,StackRef,Reg,ArrayReg};
use crate::compiler::{ProgramMeta,ScopeMetaVariant,VarMetaVariant};
// use crate::operators::{BinaryOperator,UnaryOperator};
use std::marker::PhantomData;

type RunError = String;






/// A simple CPU that can execute instructions, but does not handle any system calls
pub struct CPU<SC:Clone> {
    pub memory: Vec<i32>,
    pub sp: usize, /* stack pointer == stack size-1; index of top of stack */
    phantom: PhantomData<SC>,
}


impl<SC:Clone> CPU<SC> {
    // init
    pub fn new(stacksize:usize) -> Self {
        Self {
            memory: vec![0;stacksize],
            sp: 0,
            phantom: PhantomData,
        }
    }

    // main important thing: instruction execution
    pub fn execute(&mut self, program: &Vec<Instr<usize,usize,SC>>) -> Result<Option<(SC,Vec<FnArg>,Reg)>,RunError> {
        // get the PC
        let pc = self.memory[self.sp] as usize;
        
        // get instruction from program memory
        let instr = &program[pc];
        self.memory[self.sp]+=1;

        // execute instruction
        match instr {
            Instr::BinaryOperator { op, lhs, rhs, res } => 
                self.set_val(res,op.eval(self.get_val(lhs), self.get_val(rhs))?),
            Instr::UnaryOperator { op, rhs, res } => 
                self.set_val(res,op.eval(self.get_val(rhs))),
            Instr::ArrayIndex { array, index, res } => {
                let (pos,len) = self.get_array(array);
                let index = self.get_val(index);
                if index as usize>=len {self.memory[self.sp]+=1;return Err(format!("Index {index} out of range for array with length {len}"))}
                self.set_val(res, self.memory[(pos+index) as usize]);
            }
            Instr::ArrayAssign { array, index, value } => {
                let (pos,len) = self.get_array(array);
                let index = self.get_val(index);
                if index as usize>=len {self.memory[self.sp]+=1;return Err(format!("Index {index} out of range for array with length {len}"))}
                self.memory[(pos+index) as usize] = self.get_val(value);
            }
            Instr::Jump { index } =>
                self.memory[self.sp]=*index as i32,
            Instr::JumpUnless { index, condition } =>
                if self.get_val(condition)!=0 {self.memory[self.sp]=*index as i32},
            Instr::FnCall { index, params, res, stack } => {
                let values: Vec<i32> = params.iter().map(|fnarg| match fnarg {
                    FnArg::ArrayRef(ar) => self.get_array_ref(ar),
                    FnArg::VarRef(r) => self.get_ref(r),
                    FnArg::Val(r) => self.get_val(r),
                }).collect();
                self.sp+=stack;
                self.memory[self.sp]=*index as i32;
                self.memory[self.sp-1]=self.get_ref(res);
                for i in 0..values.len() {
                    self.memory[self.sp-2-i] = values[i];
                }
            }
            Instr::SystemCall { params, res, call } => {
                //todo!();
                return Ok(Some((call.clone(),params.clone(),*res)))
            }
            Instr::Return { stack, value } => {
                let value = self.get_val(value);
                self.set_val(&Reg::VarRef(StackRef::Rel(-1)),value);
                self.sp-=stack;
            }
            Instr::Init { stack, index } => {
                self.memory[self.sp]-=1; //undo initial +1
                self.sp=*stack-1;
                self.memory[self.sp]=*index as i32;
            }
        }

        Ok(None)

    }

    // memory operations
    pub fn get_ref(&self, reg:&Reg) -> i32 {
        // get reference to variable
        match reg {
            Reg::Const(_) => -1, /* Constants are not stored in the memory; make sure indexing by -1 is fine */
            Reg::Var(sr) => self.stack_index(sr) as i32,
            Reg::VarRef(sr) => self.memory[self.stack_index(sr)], /* might cause ERROR */
            // Reg::Array(sr, len) => (self.stack_index(sr) | (len<<16)) as i32,
            // Reg::ArrayRef(sr) => self.memory[self.stack_index(sr)],
        }
    }
    pub fn get_val(&self, reg:&Reg) -> i32 {
        // get value
        match reg {
            Reg::Const(x) => *x,
            Reg::Var(sr) => self.memory[self.stack_index(sr)],
            Reg::VarRef(sr) => self.memory[self.memory[self.stack_index(sr)] as usize],
        }
    }
    pub fn set_val(&mut self, reg:&Reg, value:i32) {
        // set value----
        match reg {
            Reg::Const(_) => {}, // storing in CONST is a safe thing to do
            Reg::Var(sr) => {
                let index=self.stack_index(sr);
                self.memory[index] = value
            },
            Reg::VarRef(sr) => {
                let index=self.stack_index(sr);
                let index=self.memory[index];
                if index<0 {return;} // if Reg was a constant, the reference will be -1, which shouldn't do anything
                self.memory[index as usize] = value
            }
        }
    }

    pub fn get_array(&self, reg:&ArrayReg) -> (i32,usize) {
        // get reference to array, split into memory location and length
        match reg {
            ArrayReg::Array(sr, len) => (self.stack_index(sr) as i32,*len),
            ArrayReg::ArrayRef(sr) => {
                let p = self.memory[self.stack_index(sr)];
                (p&((1<<16)-1), ((p as usize)>>16)&((1<<16)-1))
            }
        }
    }

    pub fn get_array_index(&self, reg:&ArrayReg, index:i32) -> Result<i32,RunError> {
        // get reference to array, split into memory location and length
        let (start,len) = self.get_array(reg);
        if index<0 || index>=len as i32 {return Err("Array read out of range".to_string())}
        Ok(self.memory[(start+index) as usize])
    }

    pub fn set_array_index(&mut self, reg:&ArrayReg, index:i32, value:i32) -> Result<(),RunError> {
        // get reference to array, split into memory location and length
        let (start,len) = self.get_array(reg);
        if index<0 || index>=len as i32 {return Err("Array write out of range".to_string())}
        self.memory[(start+index) as usize]=value;
        Ok(())
    }

    pub fn get_array_ref(&self, reg:&ArrayReg) -> i32 {
        // get reference to array, split into memory location and length
        match reg {
            ArrayReg::Array(sr, len) => self.stack_index(sr) as i32 | (len<<16) as i32,
            ArrayReg::ArrayRef(sr) => self.memory[self.stack_index(sr)],
        }
    }

    //stack indexing
    fn stack_index(&self, refr:&StackRef) -> usize {
        match refr {
            StackRef::Abs(x) => *x as usize,
            StackRef::Rel(rel) => (self.sp as i32+*rel) as usize,
        }
    }






// debug

    pub fn annotate_stack(&self, meta:&ProgramMeta) -> Vec<MemMeta> {
        let mut sp = self.sp;
        
        let mut stack = Vec::with_capacity(self.memory.len());
        for i in 0..self.memory.len() {stack.push(MemMeta{
            name:"-".to_string(),
            r#type:None,
            value:format!("({})",
            self.memory[i]),
            variant: MemMetaVariant::Unused,
        })}

        'outer: loop {
            let pc = self.memory[sp];

            stack[sp] = MemMeta{
                name: "<PC>".to_string(),
                r#type: None,
                value: format!("{pc:#04X}"),
                variant: MemMetaVariant::PC,
            };

            if sp>0 {
                stack[sp-1] = MemMeta{
                    name: "<&res>".to_string(),
                    r#type: None,
                    value: format!("{:#06X}",self.memory[sp-1]),
                    variant: MemMetaVariant::Result,
                };
            }

            for tmp in &meta.instr[pc as usize].tmp_vars {
                let index = (sp as i32+tmp) as usize;
                stack[index] = MemMeta{
                    name: format!("<TMP{}>",-tmp),
                    r#type: None,
                    value: format!("{}",self.memory[index]),
                    variant: MemMetaVariant::TMP,
                }
            }

            let mut scope = &meta.instr[pc as usize].scope;
            loop {
                let scope_meta = meta.scopes.get(scope).unwrap();
                for var in &scope_meta.variables {

                    for i in 0..(if let VarMetaVariant::Array(len) = var.variant {len} else {1}) {
                        let index = (sp as i32+var.pos) as usize + i;
                        let stack_meta=&mut stack[index];

                        stack_meta.name=match var.variant {
                            VarMetaVariant::Val => var.name.clone(),
                            VarMetaVariant::Ref => format!("&{}",var.name),
                            VarMetaVariant::Array(_) => format!("{}[{i}]",var.name),
                            VarMetaVariant::ArrayRef => format!("{}[]",var.name),
                        };

                        stack_meta.variant=match var.variant {
                            VarMetaVariant::Val => MemMetaVariant::Var,
                            VarMetaVariant::Ref => MemMetaVariant::VarRef,
                            VarMetaVariant::Array(_) => MemMetaVariant::Array,
                            VarMetaVariant::ArrayRef => MemMetaVariant::ArrayRef,
                        };

                        stack_meta.r#type=var.r#type.clone();

                        stack_meta.value=match var.variant {
                            VarMetaVariant::Val => format!("{}",self.memory[index]),
                            VarMetaVariant::Ref => format!("{:#06X}",self.memory[index]),
                            VarMetaVariant::Array(_) => format!("{}",self.memory[index]),
                            VarMetaVariant::ArrayRef => {
                                let (a,l) = self.get_array(&ArrayReg::ArrayRef(StackRef::Rel(var.pos)));
                                format!("{a:#06X}[{l}]")
                            }
                        };
                    }
                }
                
                match &scope_meta.variant {
                    ScopeMetaVariant::Function(stacksize) => {
                        if *stacksize <= sp { //not the top scope, move on to function below
                            // println!("SP {sp} STACKSIZE {stacksize}");
                            sp -= *stacksize;
                            break;
                        } else { //top scope; stop
                            break 'outer;
                        }
                    },
                    ScopeMetaVariant::Sub(parent) => scope=&parent,
                }
            }


        }

        stack
    }



}


/// Annotated stack data
pub struct MemMeta { // can be a value, array value, ref, or array ref
    //index
    pub name: String, //var
    pub r#type: Option<String>, //
    pub value: String,
    pub variant: MemMetaVariant,
}

pub enum MemMetaVariant {
    PC,
    Result,
    Var,
    VarRef,
    Array,
    ArrayRef,
    TMP,
    Unused,
}
