
use crate::parser::FnParamVariant as PV;
use crate::compiler::{Instr as Instr_,FnArg,Reg,ArrayReg,SysCallParamCheck as Check};
use crate::simple_cpu::CPU as SimpleCPU;
use crate::compiler::Environment;

type RunError=String;
type Instr<SC> = Instr_<usize,usize,BuiltinOrSysCall<SC>>;

/// System calls are split into actual system calls, and builtin functions
#[derive(Debug,Clone)]
pub enum BuiltinOrSysCall<SC:Clone+std::fmt::Debug> {
    Builtin(Builtin),
    SysCall(SC),
}
type BoSC<SC> = BuiltinOrSysCall<SC>;

/// Standard builting functions
#[derive(Debug,Clone)]
pub enum Builtin {
    // normal
    Swap,
    Pow,
    Min, // also for array O(n)
    Max, //also for array O(n)

    // array
    Reverse, //O(n)
    Len, //O(1)
    Fill, // O(n)
    Clear, // fill, but always 0, O(n)
    Copy, // copy part of array into other array O(n)
    Seq, // fill array witch sequence O(n)
    Sort, // sort, possibly in reverse, O(nlogn)
    Find, // get index of first occurence, else -1 O(n)
}



// Argument passed to system calls consists of numbers and arrays.
#[derive(Debug,Clone)]
pub enum Arg {
    Number(Number),
    Array(Array),
}

/// A number that can be read, and possibly written to.
#[derive(Debug,Clone)]
pub struct Number(Reg,bool,bool); //memory access, writable, readable

/// An array with a length
#[derive(Debug,Clone)]
pub struct Array(ArrayReg,usize);


impl Arg {
    /// Get number inside Arg, provided it is a Number.
    pub fn num(&self) -> &Number {
        match self {
            Arg::Number(n) => n,
            _ => panic!("Argument is not a number")
        }
    }
    /// Get array inside Arg, provided it is an Array.
    pub fn ar(&self) -> &Array {
        match self {
            Arg::Array(a) => a,
            _ => panic!("Argument is not an array")
        }
    }
}

impl Number {
    /// Return whether the Number can be written to using CPU::set.
    pub fn is_writable(&self) -> bool {self.1}
    /// Return whether the Number can be read using CPU::get
    pub fn is_readable(&self) -> bool {self.2}
}

impl Array {
    pub fn len(&self) -> usize {self.1}
}









/// CPU that can handle some builtin functions
pub struct CPU<SC:std::fmt::Debug+Clone>{
    pub cpu:SimpleCPU<BuiltinOrSysCall<SC>>,
    stall_cycles:usize,
}

impl<SC:std::fmt::Debug+Clone> CPU<SC> {

    /// Create new instance with a memory size of `stacksize`
    pub fn new(stacksize:usize) -> Self {
        Self {
            cpu: SimpleCPU::new(stacksize),
            stall_cycles: 1,
        }
    }

    /// Run one cycle.
    /// This generally executes one instruction, unless the CPU is stalling.
    /// If a (true) system call is made, return the system call. The system call then needs to be handled outside the CPU.
    pub fn execute(&mut self,program:&Vec<Instr<SC>>) -> Result<Option<(SC,Vec<Arg>,Number)>,RunError> {
        self.stall_cycles-=1;
        if self.stall_cycles>0 {
            return Ok(None)
        }

        self.stall(1);


        Ok(match self.cpu.execute(program)? {
            Some((BuiltinOrSysCall::Builtin(builtin),args,res)) => {
                self.execute_builtin(builtin,self.transform_args(args),Number(res,true,false))?;
                None
            }
            Some((BuiltinOrSysCall::SysCall(sc),args,res)) =>
                Some((sc,self.transform_args(args),Number(res,true,false))),
            None => None,
        })
    }

    /// Stall the CPU for this many cycles
    pub fn stall(&mut self,cycles:usize) {
        self.stall_cycles=cycles.max(1); //every instruction takes at least 1 cycle to execute
    }


    /// Create compilation environment for this CPU.
    pub fn compiler() -> Environment<BuiltinOrSysCall<SC>> {//Environment<BuiltinOrSysCall<SC>> {
        let mut env = Environment::new();
        env
            .create_basescope("@builtin".to_string())
            .add_syscall("swap"   ,BoSC::Builtin(Builtin::Swap   ),Check::Pattern(vec![PV::Reference,PV::Reference]))
            .add_syscall("pow"    ,BoSC::Builtin(Builtin::Pow    ),Check::Pattern(vec![PV::Value,PV::Value]))
            .add_syscall("min"    ,BoSC::Builtin(Builtin::Min    ),Check::Match(
                |args|
                    Environment::<BoSC<SC>>::check_fn_args(&vec![PV::Reference,PV::Reference],args) // min(a,b)
                    .or(Environment::<BoSC<SC>>::check_fn_args(&vec![PV::Array],args)) // min([a,b,c])
                ))
            .add_syscall("max"    ,BoSC::Builtin(Builtin::Max    ),Check::Match(
                |args|
                    Environment::<BoSC<SC>>::check_fn_args(&vec![PV::Reference,PV::Reference],args) // max(a,b)
                    .or(Environment::<BoSC<SC>>::check_fn_args(&vec![PV::Array],args)) // max([a,b,c])
                ))
            .add_syscall("reverse",BoSC::Builtin(Builtin::Reverse),Check::Pattern(vec![PV::Array]))
            .add_syscall("len"    ,BoSC::Builtin(Builtin::Len    ),Check::Pattern(vec![PV::Array]))
            .add_syscall("fill"   ,BoSC::Builtin(Builtin::Fill   ),Check::Pattern(vec![PV::Array,PV::Value]))
            .add_syscall("clear"  ,BoSC::Builtin(Builtin::Clear  ),Check::Pattern(vec![PV::Array]))
            .add_syscall("copy"   ,BoSC::Builtin(Builtin::Copy   ),Check::Match(
                |args|
                    Environment::<BoSC<SC>>::check_fn_args(&vec![PV::Array,PV::Array,PV::Value,PV::Value,PV::Value],args) // copy(FROM,TO,from_start,to_start,len)
                    .or(Environment::<BoSC<SC>>::check_fn_args(&vec![PV::Array,PV::Array],args)) // copy(FROM,TO)
                ))
            .add_syscall("seq"    ,BoSC::Builtin(Builtin::Seq    ),Check::Pattern(vec![PV::Array,PV::Value,PV::Value]))
            .add_syscall("sort"   ,BoSC::Builtin(Builtin::Sort   ),Check::Match(
                |args|
                    Environment::<BoSC<SC>>::check_fn_args(&vec![PV::Array],args) // sort(array)
                    .or(Environment::<BoSC<SC>>::check_fn_args(&vec![PV::Array,PV::Value],args)) // sort(array,reverse)
                ))
            .add_syscall("find"   ,BoSC::Builtin(Builtin::Find   ),Check::Pattern(vec![PV::Array,PV::Value]));
        env

    }


// memory access for calls
    /// Set a value
    pub fn set(&mut self, number:&Number,value:i32) {
        if !number.is_writable() {panic!("Attempt to write to read-only number argument")}
        self.cpu.set_val(&number.0, value);
    }
    pub fn get(&self, number:&Number) -> i32 {
        if !number.is_readable() {panic!("Attempt to read write-only number argument")}
        self.cpu.get_val(&number.0)
    }

    pub fn array_set(&mut self, array:&Array,index:usize,value:i32) -> Result<(),RunError> {
        self.cpu.set_array_index(&array.0,index as i32,value)?;
        Ok(())
    }
    pub fn array_get(&self, array:&Array,index:usize) -> Result<i32,RunError> {
        self.cpu.get_array_index(&array.0,index as i32)
    }

    // pub fn array_set_full(&mut self, array:&Array,values:Vec<i32>) -> Result<(),RunError> {
    //     todo!()
    // }
    pub fn array_get_full(&self, array:&Array) -> &[i32] {
        let (start,len) = self.cpu.get_array(&array.0);
        &self.cpu.memory[(start as usize)..(start as usize+len)]
    }
    pub fn array_get_full_mut(&mut self, array:&Array) -> &mut [i32] {
        let (start,len) = self.cpu.get_array(&array.0);
        &mut self.cpu.memory[(start as usize)..(start as usize+len)]
    }


// execution
    fn execute_builtin(&mut self, func:Builtin,args:Vec<Arg>,res:Number) -> Result<(),RunError> {
        match func {
            Builtin::Swap => {
                let a = self.get(args[0].num());
                let b = self.get(args[1].num());
                self.set(args[0].num(),b);
                self.set(args[1].num(),a);
                self.set(&res,0);
            }
            Builtin::Pow => {
                let base = self.get(args[0].num());
                let exp = self.get(args[1].num());
                self.set(&res,base.wrapping_pow(exp as u32));
                self.stall(5);
            }
            Builtin::Min => {
                match args[0] {
                    Arg::Array(_) => {
                        let min = self.array_get_full(args[0].ar()).iter().min().unwrap_or(&0);
                        self.set(&res,*min);
                        self.stall(args[0].ar().len());
                    }
                    Arg::Number(_) => {
                        let a = self.get(args[0].num());
                        let b = self.get(args[1].num());
                        self.set(&res,a.min(b));
                    }
                }
            }
            Builtin::Max => {
                match args[0] {
                    Arg::Array(_) => {
                        let max = self.array_get_full(args[0].ar()).iter().max().unwrap_or(&0);
                        self.set(&res,*max);
                        self.stall(args[0].ar().len());
                    }
                    Arg::Number(_) => {
                        let a = self.get(args[0].num());
                        let b = self.get(args[1].num());
                        self.set(&res,a.max(b));
                    }
                }
            }
            Builtin::Reverse => {
                self.array_get_full_mut(args[0].ar()).reverse();
                self.stall(args[0].ar().len());
            }
            Builtin::Len => {
                self.set(&res,args[0].ar().len() as i32)
            }
            Builtin::Fill => {
                let value = self.get(args[1].num());
                self.array_get_full_mut(args[0].ar()).fill(value);
                self.stall(args[0].ar().len());
            }
            Builtin::Clear => {
                self.array_get_full_mut(args[0].ar()).fill(0);
                self.stall(args[0].ar().len());
            }
            Builtin::Copy => {
                let from = args[0].ar();
                let to = args[1].ar();
                let mut start_from = self.get(args[2].num());
                let start_to = self.get(args[3].num());
                let mut len = self.get(args[4].num());

                if len < 0 {len=from.len() as i32}
                let offset = start_to-start_from;
                let mut end_from = start_from+len;
                start_from = start_from.max(0).min(from.len() as i32);
                end_from = end_from.max(0).min(from.len() as i32);
                for i in start_from..end_from {
                    if 0<=i+offset && i+offset<(to.len() as i32) {
                        let val = self.array_get(from,i as usize).unwrap();
                        self.array_set(to,(i+offset) as usize, val).unwrap();
                    }
                }

                self.stall((end_from-start_from) as usize);
            },
            Builtin::Seq => {
                let ar = args[0].ar();
                let mut seq = self.get(args[1].num());
                let step = self.get(args[2].num());

                for i in 0..ar.len() {
                    self.array_set(ar,i,seq).unwrap();
                    seq+=step;
                }
                self.set(&res,0);

                self.stall(args[0].ar().len());
            },
            Builtin::Sort => {
                let ar = args[0].ar();
                let rev = if args.len()>1 {
                    self.get(args[1].num())!=0
                } else {false};
                
                let data = self.array_get_full_mut(ar);
                data.sort();
                if rev {data.reverse();}
                self.set(&res,0);

                if ar.len()!=0 {self.stall(ar.len()*(ar.len()*2-1).ilog2() as usize);}
            }
            Builtin::Find => {
                let value = self.get(args[1].num());
                let index = self.array_get_full(args[0].ar()).iter().position(|&x|x==value).map(|i|i as i32).unwrap_or(-1);
                self.set(&res,index);

                self.stall(args[0].ar().len());
            }
        }
        Ok(())
    }


// argument conversion

    /// Transform `FnArg` arguments into `Arg` arguments, which are easier to use from the outside.
    fn transform_args(&self, args:Vec<FnArg>) -> Vec<Arg> {
        args.into_iter().map(|fnarg|
            match fnarg {
                FnArg::Val(reg) => Arg::Number(Number(reg,false,true)),
                FnArg::VarRef(reg) => Arg::Number(Number(reg,true,true)),
                FnArg::ArrayRef(areg) => Arg::Array(Array(areg,self.cpu.get_array(&areg).1)),
            }
        ).collect()
    }
}

