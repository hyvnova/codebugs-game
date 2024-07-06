/* TODO:

- disallow variable reading before value has been set (global vars are difficult)
- array declaration might fail if the first definition in a scope is an array with size 0? probably not
- a way to track stack meaning for debugging: keep list of scopes, their parent scopes, where the variables are at and, for functions, their total stack size
- add methods for adding constants and enums to environement
- add a way to manually add global variables? would require @root to start at higher `stack_vars`, as well as a proper interface (since unfortunately, the Abs Refence stuff needs to be shifted - maybe shift before any normal/root scopes? that could work)
- allow arrayindex to be parsed as a referenced variable
- array slicing (would require parser changes too)
- conditional compilation of if/ifelse/while statements for constant conditions
*/

/*

DOCUMENTATION

About the compilation process:

The main premisse: to take a set of statements, and turn them into instructions, in the context of a set of nested scopes.

Scopes keep track of identifiers; names that refer to things.
They also keep track of the (extra) stack space that needs to be allocated for them, including temporary variables.

For a set of instructions that need to be parsed, two main steps occur:
- The identifications of identifiers (i.e. all variable, constant, and array definitions are parsed at the start of a scope)
    This includes computing the values of constants. Constants cannot use function calls, only standard operators, constants, and enum variants.
    Functions aren't parsed yet, just identified.
- The parsing of the instructions.
    This often includes parsing expressions.
    Functions, code blocks, if/while statements etc. are parsed in new scopes.

Expression parsing happens in two steps as well:
- Evaluation and constant reduction
    Operators for constants are reduced to their constant result
    Identifiers are checked to be of the right type, and if possible, reduced to constants
- Instruction compilation
    Many expressions have sub-expressions. For example, in x = y+z, the addition has subexpressions for variable access.
    Variables and constants are turned into Regs, while other subexpressions are compiled recusively.
    This generally requires the intermediate results to be stored on the stack.
    In these cases, temporary variables are requested at from the toplevel Scope.
    After use, they are released again.


During the compilation process, stack sizes and instruction indices are not yet known.
Therefore, a final post-processing step is executed, in which these are replaced by actual numbers.
This includes:
- shifting the absolute stack references by the stack size of the global scope
- replacing the stack sizes in function calls/return statements with the correct size
- replacing jump indices with the indices of the instructions they refer to

Operands of instructions are Regs. These are sources of data (constants, variables), but can also be saved to.


*/



use std::collections::HashMap;
use log::debug;

use crate::parser::{Statement,Expr,FnParam,FnParamVariant};
use crate::operators::{UnaryOperator,BinaryOperator};

const MAX_ARRAY_LEN:usize=512;

type Error = String;
type CompileError = String;









/// Function argument
/// Can be Value, VarRef, ArrayRef
#[derive(Debug,Clone)]
pub enum FnArg {
    Val(Reg),
    VarRef(Reg),
    ArrayRef(ArrayReg),
}

// Marker or Instruction
#[derive(Debug)]
enum MkOrInstr<SC> {
    Instr(Instr<String,String,SC>,InstrMeta),
    Marker(String),
}

// Instructions
#[derive(Debug,Clone)]
pub enum Instr<ProgramIndex,StackSize,SystemCall> {
    BinaryOperator{ // operator with two arguments
        op:BinaryOperator,
        lhs:Reg,
        rhs:Reg,
        res:Reg
    },
    UnaryOperator{ // operator with one argument
        op:UnaryOperator,
        rhs:Reg,
        res:Reg
    },
    ArrayIndex{ // indexing an array
        array:ArrayReg,
        index:Reg,
        res:Reg
    },
    ArrayAssign{ // assigning a value to an index in an array
        array:ArrayReg,
        index:Reg,
        value:Reg
    },
    Jump{ // jump to index
        index:ProgramIndex,
    },
    JumpUnless{ // jump to index if condition is 0
        index:ProgramIndex,
        condition:Reg,
    },
    FnCall{ // function call
        index:ProgramIndex,
        stack:StackSize,
        params:Vec<FnArg>,
        res:Reg,
    },
    SystemCall{ // like a function call, but is executed outside of 
        call:SystemCall,
        params:Vec<FnArg>,
        res:Reg,
    },
    Return{ // write value of Reg to location in Rel(-1), then pop <stack> from stack
        stack:StackSize,
        value:Reg,
    },
    Init{ // initialise stack size and indu
        stack:StackSize,
        index:ProgramIndex,
    },
}




/*

get value:
    const(x)    x
    var(a)      mem[a]
    varref(a)   mem[mem[a]]

    array(a,n)  mem[a:+n]
    arref(a)    mem[mem[a]:+n]

set value:
    const(x)    does nothing
    *           see GET

get reference:
    const(x)    impossible
    var(a)      a
    varref(a)   mem[a]
    
    array(a,n)  (a,n)
    arref(a)    mem[a].split()

*/

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    Const(i32), //constant value. storing at this address does nothing
    Var(StackRef), //take value at this place
    VarRef(StackRef), //take value at place stored in this place
}

#[derive(Debug, Clone, Copy)]
pub enum ArrayReg {
    Array(StackRef,usize), //array starting at this place with this length
    ArrayRef(StackRef), // Array pointed to by this place; upper 16bits denote array length
}

/// Reference to value on stack
#[derive(Debug, Clone, Copy)]
pub enum StackRef {
    Rel(i32), // relative to top of stack
    Abs(i32), // stack index
}







#[derive(Debug,Clone)]
pub struct Identifier<SystemCall> {
    name: String, // full name, ie @root::foo::@if_0::bar
    variant: IdentifierVariant<SystemCall>,
}

#[derive(Debug,Clone)]
pub enum IdentifierVariant<SystemCall> {
    Variable{
        r#type:Option<String>, //type will be used later for display purposes, but is otherwise unused
        reg:Reg,
    },
    Array{
        r#type:Option<String>,
        reg:ArrayReg,
    },
    Function{
        params:Vec<FnParam>, //TODO: FnParam rework? currently just val/ref/array, without type
    },
    SysCall{
        param_check:SysCallParamCheck,
        call: SystemCall, // associated system call data
    },
    Constant{
        r#type:Option<String>,
        value:i32,
    },
    Enum{
        variants:HashMap<String,i32>,
    }
}

#[derive(Debug,Clone)]
pub enum SysCallParamCheck {
    Pattern(Vec<FnParamVariant>),
    Match(fn(&Vec<FnParamVariant>) -> Result<(),Error>),
    Runtime,
}


#[derive(Debug,Clone,Copy,PartialEq)]
pub enum ScopeVariant {
    If,
    // IfElse,
    While,
    Loop,
    Function,
    Block,
}
impl ScopeVariant {
    fn prefix(&self) -> String {
        (match self {
            Self::If => "@if_",
            // Self::IfElse => "ifelse",
            Self::While => "@while_",
            Self::Loop => "@loop_",
            Self::Function => "",
            Self::Block => "@blk_",
        }).to_string()
    }
}

struct Scope<SystemCall> {
    name: String,
    global: bool,
    identifiers: HashMap<String,Identifier<SystemCall>>,
    variant: ScopeVariant,
    
    stack_vars:usize, //amount of stack space allocated for variables, arrays etc., including those of parent scopes
    stack_tmp:usize, //extra space required for temporary variables
    stack_max:usize, //maximum total stack size at any moment
    tmp_vars: Vec<Reg>, //temporary variables that are currently available
}



/// Meta data about instruction, for debug purposes.
#[derive(Debug,Clone)]
pub struct InstrMeta {
    pub scope: String, //maybe replace with usize for efficiency
    pub tmp_vars: Vec<i32>, //or StackRef
}

/// Meta data about scopes, for debug purposes.
#[derive(Debug,Clone)]
pub struct ScopeMeta {
    pub variant: ScopeMetaVariant,
    pub variables: Vec<VarMeta>,
}
/// Meta data about memory psition
#[derive(Debug,Clone)]
pub struct VarMeta {
    pub pos: i32, // or StackRef?
    pub name: String,
    pub r#type: Option<String>,
    pub variant: VarMetaVariant,

}
#[derive(Debug,Clone)]
pub enum VarMetaVariant {
    Val, //memory has a value
    Ref, //memory holds index of value
    Array(usize), //memory holds values, for this length
    ArrayRef, //memory holds index and length of array
}
#[derive(Debug,Clone)]
pub enum ScopeMetaVariant {
    Function(usize), //size of function
    Sub(String), //parent scope
}
/// Meta data about a program, for debug purposes.
#[derive(Debug,Clone)]
pub struct ProgramMeta {
    pub scopes: HashMap<String,ScopeMeta>,
    pub instr: Vec<InstrMeta>,
}





pub struct Environment<SC> {
    scopes: Vec<Scope<SC>>,
    fn_sizes: HashMap<String,usize>, //stack size per function
    global_size: usize, //stack size of root
    meta: ProgramMeta,
}


impl<SC:Clone+std::fmt::Debug> Environment<SC> {
// compilation:

    /// Helper function for getting current instruction meta data.
    fn imeta(&self) -> InstrMeta {
        InstrMeta{
            scope: self.get_scope_name().clone(),
            tmp_vars: vec![], //TODO: keep track of tmp vars currently in use
        }
    }

    /// Compile statements into current scope
    /// Instructions are added to `instr`.
    /// Instructions for nested function definitions are added to `instr_fn`.
    fn compile_statements(
        &mut self,
        mut statements:Vec<Statement>,
        instr:&mut Vec<MkOrInstr<SC>>,
        instr_fn:&mut Vec<MkOrInstr<SC>>
    ) -> Result<(),Error> {
        debug!("Starting compilation of scope {}",&self.get_scope_name());

        // Register definitions
        self.register_definition_statements(&mut statements)?;

        // Compile statements
        self.compile_action_statements(statements, instr, instr_fn)?;

        debug!("Finished compiling scope");
        // return stuff
        Ok(())
    }


    /// Register definitions of statements
    fn register_definition_statements(&mut self, statements:&mut Vec<Statement>) -> Result<(),CompileError> {
        // first, register all elements in the current scope
        // should we do this? just functions maybe?
        for statement in statements.iter_mut() {
            debug!("Precompiling statement {:?}",statement);
            match statement {
                Statement::VarDef{vars,r#type} => {
                    // put variables into top scope
                    for name in vars {
                        let scope = self.scopes.last_mut().unwrap();
                        let pos = -(scope.stack_vars as i32);
                        scope.insert_ident(
                            name.clone(),
                            IdentifierVariant::Variable{
                                r#type:r#type.clone(),
                                reg:Reg::Var(
                                    if scope.global {
                                        StackRef::Abs(pos) // depends on stack size of main program, which is unknown - maybe take abs0!=stack0... temporarily? fix them later? is ugly but oh well I suppose it needs to be done
                                    } else {
                                        StackRef::Rel(pos)
                                    }
                                )
                            }
                        )?;
                        scope.stack_vars += 1;
                        scope.stack_max += 1;

                        self.meta.scopes.get_mut(&self.scopes.last().unwrap().name).unwrap().variables.push(VarMeta{
                            pos,
                            name: name.clone(),
                            r#type: r#type.clone(),
                            variant: VarMetaVariant::Val,
                        });
                    }
                    
                }
                Statement::ArrayDef{arrays,r#type} => {
                    // put arrays into top scope
                    for (name,size) in arrays {
                        let size = self.compile_const_expr(size)?;
                        let scope = self.scopes.last_mut().unwrap();
                        if size<0 || size as usize > MAX_ARRAY_LEN {
                            return Err(format!("Array length {size} for array {name} in scope {} invalid",scope.name));
                        }
                        let size = size as usize;
                        
                        let pos = -((scope.stack_vars+size-1) as i32);
                        scope.insert_ident(
                            name.clone(),
                            IdentifierVariant::Array{
                                r#type:r#type.clone(),
                                reg:ArrayReg::Array(
                                    if scope.global {
                                        StackRef::Abs(pos)
                                    } else {
                                        StackRef::Abs(pos)
                                    },
                                    size
                                )
                            }
                        )?;
                        scope.stack_vars += size;
                        scope.stack_max += size;

                        self.meta.scopes.get_mut(&self.scopes.last().unwrap().name).unwrap().variables.push(VarMeta{
                            pos: pos,
                            name: name.clone(),
                            r#type: r#type.clone(),
                            variant: VarMetaVariant::Array(size),
                        });
                    }
                }
                Statement::FnDef{name,params,..} => {
                    // put function name and parameters into top scope
                    self.insert_ident(
                        ScopeVariant::Function.prefix() + name, 
                        IdentifierVariant::Function{params:params.clone()}
                    )?;
                }
                Statement::ConstDef{name,value} => {
                    // put const into top scope
                    // constants are evaluated NOW so they can only use constants that were defined previously
                    let value = self.compile_const_expr(value)?;
                    self.insert_ident(
                        name.clone(),
                        IdentifierVariant::Constant{value, r#type:None}
                    )?;
                }
                Statement::EnumDef{name,variants} => {
                    // enums
                    // note that multiple enum variants may have the same value, but not name
                    // they essentially act as bundled constants
                    let mut counted_variants:HashMap<String,i32> = HashMap::new();
                    let mut i:i32=-1;
                    for (variant,value) in variants.iter_mut() {
                        i = match value {Some(v)=>self.compile_const_expr(v)?, _=>i+1};
                        let old = counted_variants.insert(variant.clone(),i);
                        if let Some(_) = old {return Err(format!("Enum variant {variant} of enum {name} in scope {} is already defined",self.get_scope_name()));}
                    }
                    // put enum into top scope
                    self.insert_ident(
                        name.clone(),
                        IdentifierVariant::Enum{
                            variants:counted_variants
                        }
                    )?;
                }
                _ => {}
            }
        }
        Ok(())
    }
    /// Actually compile statements
    fn compile_action_statements(
        &mut self,
        statements:Vec<Statement>,
        instr:&mut Vec<MkOrInstr<SC>>,
        instr_fn:&mut Vec<MkOrInstr<SC>>
    ) -> Result<(),CompileError> {
        let mut cnt_blk = 0u32;
        let mut cnt_if  = 0u32;
        let mut cnt_loop = 0u32;
        let mut cnt_while = 0u32;



        // then, do the actual compilation to a format full of references (as number of variables on stack etc. and indices of functions are still uncertain)
        for statement in statements.into_iter() {
            debug!("Fully compiling statement {:?}",statement);
            match statement {
                Statement::VarAssign{name,mut value} => {
                    debug!(" > starting compiling varassign");
                    debug!(" > reduce const");
                    self.preprocess_expr(&mut value)?;

                    debug!(" > find variable");
                    let reg = match self.find_ident(&name) {
                        Some(Identifier{variant:IdentifierVariant::Variable{reg,..},..}) => *reg,
                        _ => unreachable!(),
                    };

                    debug!(" > compile expr");
                    self.compile_expr(value, instr,Some(reg))?;
                    debug!(" > finished compiling varassign");

                }
                Statement::ArrayAssign{name,mut index,mut value} => {
                    // compute index
                    // compute value
                    // assign to array
                    self.preprocess_expr(&mut index)?;
                    self.preprocess_expr(&mut value)?;

                    let (reg_index,tmp_index) = self.compile_expr(index, instr, None)?;
                    let (reg_value,tmp_value) = self.compile_expr(value, instr,None)?;
                    self.release_tmp(tmp_value);
                    self.release_tmp(tmp_index);

                    let reg = match self.find_ident(&name) {
                        Some(Identifier{variant:IdentifierVariant::Array{reg,..},..}) => *reg,
                        _ => unreachable!(),
                    };

                    instr.push(MkOrInstr::Instr(Instr::ArrayAssign{array:reg,index:reg_index,value:reg_value},self.imeta()));
                }

                Statement::If{mut condition,code} => {
                    // open up IF scope
                    // add computations for computing expression
                    // add conditional jump to END of scope
                    // add instructions for code
                    // register scope data
                    // pop scope
                    self.create_subscope(format!("{}",cnt_if),ScopeVariant::If);
                    cnt_if+=1;
                    instr.push(MkOrInstr::Marker(self.get_scope_name().clone()+".START")); // not really needed, but useful for clarity

                    self.preprocess_expr(&mut condition)?;

                    let (reg,tv) = self.compile_expr(condition, instr,None)?;
                    instr.push(MkOrInstr::Instr(Instr::JumpUnless{index:self.get_scope_name().clone()+".END",condition:reg},self.imeta()));
                    self.release_tmp(tv);

                    self.compile_statements(vec![*code],instr,instr_fn)?;

                    instr.push(MkOrInstr::Marker(self.get_scope_name().clone()+".END"));
                    self.pop_scope();
                }
                Statement::IfElse{mut condition,yes,no} => {

                    // like if, but at end of first code jump to end of second instr
                    self.create_subscope(format!("{}",cnt_if),ScopeVariant::If);
                    cnt_if+=1;
                    instr.push(MkOrInstr::Marker(self.get_scope_name().clone()+".START")); //not really needed

                    self.preprocess_expr(&mut condition)?;

                    let (reg,tv) = self.compile_expr(condition,instr, None)?;
                    instr.push(MkOrInstr::Instr(Instr::JumpUnless{index:self.get_scope_name().clone()+".ELSE",condition:reg},self.imeta()));
                    self.release_tmp(tv);

                    self.compile_statements(vec![*yes],instr,instr_fn)?;

                    instr.push(MkOrInstr::Instr(Instr::Jump{index:self.get_scope_name().clone()+".END"},self.imeta()));
                    instr.push(MkOrInstr::Marker(self.get_scope_name().clone()+".ELSE"));

                    self.compile_statements(vec![*no],instr,instr_fn)?;

                    instr.push(MkOrInstr::Marker(self.get_scope_name().clone()+".END"));
                    self.pop_scope();
                }
                Statement::While{mut condition,code} => {
                    // open up while scope
                    // add computations for computing expression
                    // add conditional JUMP to end of scope
                    // add instructions for code
                    // add JUMP to START of while scope
                    // register scope data
                    // pop scope
                    self.create_subscope(format!("{}",cnt_while),ScopeVariant::Loop);
                    cnt_while+=1;
                    instr.push(MkOrInstr::Marker(self.get_scope_name().clone()+".START"));

                    self.preprocess_expr(&mut condition)?;

                    let (reg,tv) = self.compile_expr(condition,instr,None)?;
                    instr.push(MkOrInstr::Instr(Instr::JumpUnless{index:self.get_scope_name().clone()+".END",condition:reg},self.imeta()));
                    self.release_tmp(tv);

                    self.compile_statements(vec![*code],instr,instr_fn)?;
                    instr.push(MkOrInstr::Instr(Instr::Jump{index:self.get_scope_name().clone()+".START"},self.imeta()));
                    
                    instr.push(MkOrInstr::Marker(self.get_scope_name().clone()+".END"));
                    
                    self.pop_scope();
                }
                Statement::Loop(code) => {
                    // open up loop scope
                    // parse code
                    // register max number of variables in scope
                    // pop loop scope
                    // add JUMP to start of current scope
                    self.create_subscope(format!("{}",cnt_loop),ScopeVariant::Loop);
                    cnt_loop+=1;
                    instr.push(MkOrInstr::Marker(self.get_scope_name().clone()+".START"));

                    self.compile_statements(vec![*code],instr,instr_fn)?;
                    instr.push(MkOrInstr::Instr(Instr::Jump{index:self.get_scope_name().clone()+".START"},self.imeta()));

                    instr.push(MkOrInstr::Marker(self.get_scope_name().clone()+".END"));
                    self.pop_scope();
                }
                Statement::Break => {
                    // find nearest scope that is loop or while
                    // fail when encountering fn scope
                    // jumpt to END of scope (i.e. exactly afterwards)
                    let loop_scope = self.find_scope_variant(&[ScopeVariant::While,ScopeVariant::Loop]);
                    if let Some(loop_scope)=loop_scope {
                        instr.push(MkOrInstr::Instr(Instr::Jump{index:loop_scope.name.clone()+".END"},self.imeta()));
                    } else {
                        return Err(format!("Break statement outside of loop in scope {}",self.get_scope_name()))
                    }
                }
                Statement::Continue => {
                    // find nearest scope that is loop or while
                    // fail when encountering fn scope
                    // loop: jump to START of that scope (i.e. first instruction after)
                    // while: jump to before computation of condition
                    let loop_scope = self.find_scope_variant(&[ScopeVariant::While,ScopeVariant::Loop]);
                    if let Some(loop_scope)=loop_scope {
                        instr.push(MkOrInstr::Instr(Instr::Jump{index:loop_scope.name.clone()+".START"},self.imeta()));
                    } else {
                        return Err(format!("Continue statement outside of loop in scope {}",self.get_scope_name()))
                    }
                }
                Statement::Return(value) => {
                    let mut value = value.unwrap_or_else(||Expr::Num(0));
                    self.preprocess_expr(&mut value)?;
                    // find parent fn scope
                    // return (value, parent_fn) if it is not global!!!
                    let fn_scope = self.find_scope_variant(&[ScopeVariant::Function]).unwrap();
                    if fn_scope.global {
                        return Err(format!("Return statement outside of function in scope {}",self.get_scope_name()))
                    }
                    let fn_name = fn_scope.name.clone();

                    let (reg,tv) = self.compile_expr(value,instr,None)?;
                    self.release_tmp(tv);
                    instr.push(MkOrInstr::Instr(Instr::Return{stack:fn_name,value:reg},self.imeta()));
                }

                Statement::Expr(mut expr) => {
                    // just an expression not assigned to a variable (mostly function calls)
                    // basically the same as variable assignment
                    // but use fake target address
                    // also, ignore if expr is constant
                    self.preprocess_expr(&mut expr)?;
                    self.compile_expr(expr,instr,Some(Reg::Const(0)))?;
                }

                Statement::CodeBlock(statements) => {
                    // create scope
                    // parse things in block
                    // register scope
                    // pop scope

                    self.create_subscope(format!("{}",cnt_blk),ScopeVariant::Block);
                    cnt_blk+=1;

                    self.compile_statements(statements,instr,instr_fn)?;

                    self.pop_scope();
                }

                // definitions already handled
                Statement::VarDef{..} => {}
                Statement::ArrayDef{..} => {}
                Statement::ConstDef{..} => {}
                Statement::EnumDef{..} => {}
                
                // compute function 
                Statement::FnDef{name,params,code} => {
                    // determine code for function 
                    // must be appended to script after

                    // create function scope
                    // add parameters to scope
                    // parse the code of the function
                    // add instructions to functions
                    // register function size
                    // reabsorb scope
                    debug!(" > start compiling function");
                    
                    // make scope
                    self.create_fn_scope(name);
                    instr_fn.push(MkOrInstr::Marker(self.get_scope_name().clone()+".START"));

                    // let scope = ;
                    // add params
                    for param in params.into_iter() {
                        match param.variant {
                            FnParamVariant::Value => {
                                let pos:i32 = -(self.scopes.last().unwrap().stack_vars as i32);
                                self.meta.scopes.get_mut(&self.scopes.last().unwrap().name).unwrap().variables.push(VarMeta{
                                    pos: pos,
                                    name: param.name.clone(),
                                    r#type: None,
                                    variant: VarMetaVariant::Val,
                                });
                                self.insert_ident(
                                    param.name,
                                    IdentifierVariant::Variable{
                                        reg:Reg::Var(StackRef::Rel(pos)),
                                        r#type:None}).unwrap()
                            }
                            FnParamVariant::Reference => {
                                let pos:i32 = -(self.scopes.last().unwrap().stack_vars as i32);
                                self.meta.scopes.get_mut(&self.scopes.last().unwrap().name).unwrap().variables.push(VarMeta{
                                    pos: pos,
                                    name: param.name.clone(),
                                    r#type: None,
                                    variant: VarMetaVariant::Ref,
                                });
                                self.insert_ident(
                                    param.name,
                                    IdentifierVariant::Variable{
                                        reg:Reg::VarRef(StackRef::Rel(pos)),
                                        r#type:None}).unwrap()
                            }
                            FnParamVariant::Array => {
                                let pos:i32 = -(self.scopes.last().unwrap().stack_vars as i32);
                                self.meta.scopes.get_mut(&self.scopes.last().unwrap().name).unwrap().variables.push(VarMeta{
                                    pos: pos,
                                    name: param.name.clone(),
                                    r#type: None,
                                    variant: VarMetaVariant::ArrayRef,
                                });
                                self.insert_ident(
                                    param.name,
                                    IdentifierVariant::Array{
                                        reg:ArrayReg::ArrayRef(StackRef::Rel(pos)),
                                        r#type:None}).unwrap()
                            }
                        }
                        let scope = self.scopes.last_mut().unwrap();
                        scope.stack_vars+=1;
                        scope.stack_max+=1;
                    }

                    // parse code and add to functions
                    let mut instr_fn2 = Vec::new();
                    self.compile_statements(code,instr_fn,&mut instr_fn2)?;
                    instr_fn.push(MkOrInstr::Marker(self.get_scope_name().clone()+".END")); //mark end
                    
                    self.fn_sizes.insert(self.get_scope_name().clone(),self.scopes.last().unwrap().stack_max); //register size
                    self.pop_scope();

                    instr_fn.append(&mut instr_fn2);

                    debug!(" > finished compiling function");
                }
            }
        }


        // if this is a function, and if it does not already end on a return statement, and it is not the global scope, add a return statement
        if matches!(self.scopes.last().unwrap().variant,ScopeVariant::Function{..}) && !self.scopes.last().unwrap().global{
            if !matches!(
                instr.iter().rev().filter(|mki| matches!(mki,MkOrInstr::Instr(_,_))).next(),
                Some(MkOrInstr::Instr(Instr::Return{..},..))
            ) { //last instr (not marker!) is not a Return statement
                instr.push(MkOrInstr::Instr(Instr::Return{stack:self.get_scope_name().clone(),value:Reg::Const(0)},self.imeta()));
            }
        }

        Ok(())
    }

    /// Preprocess expression: reduce to constants where possible, and check identifiers.
    /// For functions/syscalls, also check if function parameters match descriptions.
    pub fn preprocess_expr(&self,expr:&mut Expr)-> Result<(),Error> { //TODO: mut borrow expr, and change in place??? probably better; requires less cloning
        match expr {
            Expr::Num(_) => {}//Ok(Expr::Num(*x)),
            Expr::Op{op,lhs,rhs} => {
                self.preprocess_expr(lhs)?;
                self.preprocess_expr(rhs)?;

                if let (Expr::Num(x),Expr::Num(y)) = (&**lhs,&**rhs) {
                    *expr = Expr::Num(op.eval(*x,*y)?);
                }
            }
            Expr::Unary{op,rhs} => {
                self.preprocess_expr(rhs)?;
                if let Expr::Num(y) = **rhs {
                    *expr = Expr::Num(op.eval(y))
                }
            }
            Expr::Variable(name) => {
                match self.find_ident(name) {
                    Some(Identifier{variant:IdentifierVariant::Constant{value,..},..}) => *expr=Expr::Num(*value),
                    Some(Identifier{variant:IdentifierVariant::Variable{..},..}) => {}
                    _ => return Err(format!("'{name}' does not name a variable or constant in scope '{}'",self.get_scope_name())),
                }
            }
            Expr::EnumVariant{name,variant} => {
                match self.find_ident(name) {
                    Some(Identifier{name,variant:IdentifierVariant::Enum{variants}}) => {
                        match variants.get(variant) {
                            Some(value) => *expr=Expr::Num(*value),
                            _ => return Err(format!("Variant '{variant}' not found in enum '{name}'"))
                        }
                    },
                    _ => return Err(format!("'{name}' does not name an enum in scope '{}'",self.get_scope_name())),
                }
            }
            Expr::ArrayIndex{name,index} => {
                match self.find_ident(name) {
                    Some(Identifier{variant:IdentifierVariant::Array{..},..}) => 
                        self.preprocess_expr(index)?,
                    _ => return Err(format!("'{name}' does not name an array in scope '{}'",self.get_scope_name())),
                }
            }
            Expr::FnCall{name,args} => {

                // TODO
                // make this better

                /*
                
                Look at the arguments
                Var -> check array or variable -> make ref or array
                other expr -> preprocess -> val

                then check against pattern, where
                - array matches array
                - val/ref matches val
                - ref matches ref

                syscalls have more advanced checking (i.e. pattern, during runtime, or arbitrary function)

                */


                match self.find_ident(name) {
                    Some(Identifier{variant:IdentifierVariant::Function{params,..},..}) => {
                        // only compile arguments that are of VALUE/REF type, since arrays are seens as variables, that aren't variables in this scope

                        let arg_variants = args.iter_mut().map(|arg| {
                            if let Expr::Variable(var) = arg {
                                if matches!(self.find_ident(var),Some(Identifier{variant:IdentifierVariant::Array{..},..})) {
                                    return Ok(FnParamVariant::Array)
                                }
                            }
                            self.preprocess_expr(arg)?;
                            match arg {
                                Expr::Variable(_) => Ok(FnParamVariant::Reference), // TODO <= add Expr::ArrayIndex here to allow array index parsing by reference
                                _ => Ok(FnParamVariant::Value),
                            }
                        }).collect::<Result<Vec<FnParamVariant>,CompileError>>()?;

                        Environment::<SC>::check_fn_args(&params.iter().map(|param| param.variant).collect(),&arg_variants)?;
                    }
                    Some(Identifier{variant:IdentifierVariant::SysCall{param_check,..},..}) => {
                        // same code as function; somehow merge the two? TODO; only final check is different
                        let arg_variants = args.iter_mut().map(|arg| {
                            if let Expr::Variable(var) = arg {
                                if matches!(self.find_ident(var),Some(Identifier{variant:IdentifierVariant::Array{..},..})) {
                                    return Ok(FnParamVariant::Array)
                                }
                            }
                            self.preprocess_expr(arg)?;
                            match arg {
                                Expr::Variable(_) => Ok(FnParamVariant::Reference), // TODO <= add Expr::ArrayIndex here to allow array index parsing by reference
                                _ => Ok(FnParamVariant::Value),
                            }
                        }).collect::<Result<Vec<FnParamVariant>,CompileError>>()?;

                        match param_check {
                            SysCallParamCheck::Pattern(p) =>
                                Environment::<SC>::check_fn_args(p,&arg_variants)?,
                            SysCallParamCheck::Match(check) => check(&arg_variants)?,
                            SysCallParamCheck::Runtime => {}
                        }
                    }
                    _ => return Err(format!("{name} does not name a function in scope {}",self.get_scope_name())),
                }
            }
        }
        Ok(())
    }

    /// Attempt to evaluate expression as a constant, given a list of scopes.
    /// This includes the preprocessing step.
    pub fn compile_const_expr(&mut self,expr:&mut Expr) -> Result<i32,CompileError> {
        self.preprocess_expr(expr)?;
        if let Expr::Num(x) = expr {
            Ok(*x)
        } else {
            Err(format!("Expression {expr:?} cannot be computed as a constant"))
        }
    }

    /// Compile pre-processed(!!!) expression into instr.
    /// If a target register is supplied, use that, otherwise, return.
    /// 
    /// Generate instructions for processing an expression
    /// Assumes expression has been reduced to constants beforehand (i.e. variables are not constants, no enum variants)
    /// and variables/arrays have been checked to exist
    fn compile_expr(
        &mut self,
        expr:Expr,
        instr: &mut Vec<MkOrInstr<SC>>,
        dest:Option<Reg> /* memory location to store result in */
    ) -> Result<(Reg,Option<Reg>),Error> {
        // debug!(" > CALC EXPR {:?}",expr);

        /// Get destination (either the original dest, or a temporary variable)
        fn get_reg<SC:Clone+std::fmt::Debug>(env:&mut Environment<SC>,reg:Option<Reg>) -> (Reg,Option<Reg>) {
            if let Some(r) = reg {
                (r,None)
            } else {
                let tv=env.get_tmp();
                (tv,Some(tv))
            }
        }

        Ok(match expr {
            Expr::Num(x) => {
                if let Some(r)=dest {
                    instr.push(MkOrInstr::Instr(Instr::UnaryOperator{op:UnaryOperator::Nop,rhs:Reg::Const(x),res:r},self.imeta()));
                    (r,None) // store constant in reg
                } else {
                    (Reg::Const(x),None) //just return the constant
                }
            }
            Expr::Variable(name) => {
                let var = match self.find_ident(&name) {
                    Some(Identifier{variant:IdentifierVariant::Variable{reg:r,..},..}) => *r,
                    // Some(Identifier{variant:IdentifierVariant::VariableRef{reg:r},..}) => *r,
                    _ => unreachable!(), // code was checked during reduce_const step, and should only have (referenced) variables
                };

                if let Some(d)=dest {
                    instr.push(MkOrInstr::Instr(Instr::UnaryOperator{op:UnaryOperator::Nop,rhs:var,res:d},self.imeta())); // store variable
                    (d,None)
                } else {
                    (var,None) // just return variable
                }
            }
            Expr::Op{op,lhs,rhs} => {
                let (reg_lhs,tmp_lhs) = self.compile_expr(*lhs,instr,None)?;
                let (reg_rhs,tmp_rhs) = self.compile_expr(*rhs,instr,None)?;
                self.release_tmp(tmp_rhs);
                self.release_tmp(tmp_lhs);
                
                let (reg,tv) = get_reg(self,dest);

                instr.push(MkOrInstr::Instr(Instr::BinaryOperator{op,lhs:reg_lhs,rhs:reg_rhs,res:reg},self.imeta()));
                (reg,tv)
            },
            Expr::Unary{op,rhs} => {
                let (reg_rhs,tmp_rhs) = self.compile_expr(*rhs,instr,None)?;
                self.release_tmp(tmp_rhs);
                
                let (reg,tv) = get_reg(self,dest);
            
                instr.push(MkOrInstr::Instr(Instr::UnaryOperator{op,rhs:reg_rhs,res:reg},self.imeta()));
                (reg,tv)
            },
            Expr::ArrayIndex{name,index} => {
                // compute index, add indexing instr
                let (reg_index,tmp_index) = self.compile_expr(*index,instr,None)?;
                self.release_tmp(tmp_index);
                
                let (reg,tv) = get_reg(self,dest);

                let array = match self.find_ident(&name) {
                    Some(Identifier{variant:IdentifierVariant::Array{reg:r,..},..}) => *r,
                    _ => unreachable!(),
                };
            
                instr.push(MkOrInstr::Instr(Instr::ArrayIndex{array:array,index:reg_index,res:reg},self.imeta()));
                (reg,tv)
            },
            Expr::FnCall{name,args} => {
                // get function from scopes
                // check number of arguments
                // per argument:
                    // check argument type
                        // evaluate expr -> reg, of which the VALUE will be copied - store any temp vars created
                        // ref -> reg, of which the STACK POS will be copied
                        // array -> reg:array, 
                // get return register
                // call function
                // free temp registers

                let mut tmp_vars:Vec<Option<Reg>> = Vec::new(); //to store temp vars
            
                // get function
                let fnid = self.find_ident(&name).unwrap().clone();

                // split between sys call and function call
                match fnid {
                    Identifier{variant:IdentifierVariant::Function{params},..} => {
                        // map arguments to parameters
                        let params = args.into_iter().zip(params.iter()).map(|(val,param)| 
                            Ok::<FnArg,CompileError>(match param.variant {
                                FnParamVariant::Value => {
                                    // argument can be any expression
                                    let (reg,tv) = self.compile_expr(val,instr,None)?;
                                    tmp_vars.push(tv);
                                    FnArg::Val(reg)
                                }
                                FnParamVariant::Reference => {
                                    // argument must be a variable(name) where name refers to any (ref)variable
                                    if let Expr::Variable(name) = val {
                                        match self.find_ident(&name).unwrap() {
                                            Identifier{variant:IdentifierVariant::Variable{reg,..},..} =>
                                                FnArg::VarRef(*reg),
                                            _=>unreachable!(),
                                        }
                                    } else {unreachable!();}
                                }
                                FnParamVariant::Array => {
                                    // argument must be Variable(name) where name refers to an array
                                    if let Expr::Variable(name) = val {
                                        match self.find_ident(&name).unwrap() {
                                            Identifier{variant:IdentifierVariant::Array{reg,..},..} =>
                                                FnArg::ArrayRef(*reg),
                                            _=>unreachable!(),
                                        }
                                    } else {unreachable!();}
                                }
                            })
                        ).collect::<Result<Vec<FnArg>,_>>()?;
        
                        // free variables
                        for tv in tmp_vars.into_iter().rev() {self.release_tmp(tv);}
                        
                        let (reg,tv) = get_reg(self,dest);
                        instr.push(MkOrInstr::Instr(Instr::FnCall {
                            index: fnid.name.clone()+".START",
                            params,
                            res: reg,
                            stack: fnid.name.clone()
                        },self.imeta()));
                        (reg,tv)
                    },
                    Identifier{variant:IdentifierVariant::SysCall {  call,.. },..} => {
                        // same code as function; somehow merge the two? TODO; only final check is different
                        let params = args.into_iter().map(|arg| {
                            Ok(if let Expr::Variable(var) = arg {
                                match self.find_ident(&var) {
                                    Some(Identifier{variant:IdentifierVariant::Array{reg,..},..}) => FnArg::ArrayRef(*reg),
                                    Some(Identifier{variant:IdentifierVariant::Variable{reg,..},..}) => FnArg::VarRef(*reg),
                                    _=>unreachable!(),
                                }
                            } else {
                                let (reg,tv) = self.compile_expr(arg,instr,None)?;
                                tmp_vars.push(tv);
                                FnArg::Val(reg)
                            })
                        }).collect::<Result<Vec<FnArg>,CompileError>>()?;


                        // free variables
                        for tv in tmp_vars.into_iter().rev() { self.release_tmp(tv);}

                        let (reg,tv) = get_reg(self,dest);
                        instr.push(MkOrInstr::Instr(Instr::SystemCall {params, res:reg, call:call.clone()},self.imeta()));

                        (reg,tv)
                    }
                    _=>unreachable!(),
                }
            },
            Expr::EnumVariant{..} => unreachable!(), // enum variants should have been converted to const already
        })
    }

    /// Post-process instructions, filling in markers, shifting abs, filling in indices etc.
    /// Fill in ProgramIndex markers, StackSizes, and shift Abs positions by root's stacksize
    fn postprocess_instr(
        &mut self,
        instr: Instr<String,String,SC>,
        markers:&HashMap<String,usize>,
        absshift:i32,
    ) -> Instr<usize,usize,SC> {
        match instr {
            Instr::Jump{index} => Instr::Jump{index:*markers.get(&index).unwrap()},
            Instr::JumpUnless{index,condition} =>
                Instr::JumpUnless{index:*markers.get(&index).unwrap(),condition:condition.absshift(absshift)},
            Instr::FnCall{index,params,res,stack} => 
                Instr::FnCall{
                    index:*markers.get(&index).unwrap(),//_or_else(|| {debug!("{:?} {:?}",markers,index);panic!()}),
                    params:params.into_iter().map(|a| a.absshift(absshift)).collect(),
                    res:res.absshift(absshift),
                    stack:*self.fn_sizes.get(&stack).unwrap(), //unwrap_or_else(|| {debug!("{:?} {}",fnsizes,stack);panic!()}),
                },
            Instr::SystemCall{params,res,call} => 
                Instr::SystemCall{
                    params:params.into_iter().map(|a| a.absshift(absshift)).collect(),
                    res:res.absshift(absshift),
                    call:call,
                },

            Instr::Return { stack, value } =>
                Instr::Return { stack:*self.fn_sizes.get(&stack).unwrap(), value:value.absshift(absshift) }, // get stack size of function by name

            //i => i as Instr<usize>,
            Instr::ArrayAssign { array, index, value } =>
                Instr::ArrayAssign { array, index:index.absshift(absshift), value:value.absshift(absshift) },
            Instr::ArrayIndex { array, index, res } =>
                Instr::ArrayIndex { array, index:index.absshift(absshift), res:res.absshift(absshift) },
            Instr::BinaryOperator { op, lhs, rhs, res } =>
                Instr::BinaryOperator { op, lhs:lhs.absshift(absshift), rhs:rhs.absshift(absshift), res:res.absshift(absshift) },
            Instr::UnaryOperator { op, rhs, res } =>
                Instr::UnaryOperator { op, rhs:rhs.absshift(absshift), res:res.absshift(absshift) },

            Instr::Init{stack, index} => Instr::Init{stack:*self.fn_sizes.get(&stack).unwrap(),index:*markers.get(&index).unwrap()},
        }
    }


// scopes:

    /// Create a subscope (not for functions)
    fn create_subscope(&mut self,name: String, variant:ScopeVariant) {
        let scope = self.scopes.last().unwrap().create_subscope(name,variant);
        self.meta.scopes.insert(
            scope.name.clone(),
            ScopeMeta {
                variant: ScopeMetaVariant::Sub(self.scopes.last().unwrap().name.clone()),
                variables: vec![],
            }
        );
        self.scopes.push(scope);
    }

    /// Create a function subscope
    fn create_fn_scope(&mut self,name: String) {
        let scope = self.scopes.last().unwrap().create_fn_scope(name);
        self.meta.scopes.insert(
            scope.name.clone(),
            ScopeMeta {
                variant: ScopeMetaVariant::Function(0),
                variables: vec![],
            }
        );
        self.scopes.push(scope);
    }

    /// Pop top scope and let it be absorbed by its parent
    fn pop_scope(&mut self,) {
        let scope = self.scopes.pop().unwrap();
        assert!(scope.tmp_vars.len()==scope.stack_tmp,"Attempt to close scope before all temp vars are released");
        if let ScopeMetaVariant::Function(size) = &mut self.meta.scopes.get_mut(&scope.name).unwrap().variant {
            *size = scope.stack_max;
        }
        self.scopes.last_mut().unwrap().absorb_subscope(scope);
    }

    /// Return top scope matching any of the supplied scope variants
    /// Stop after finding a function scope
    fn find_scope_variant<'a>(&'a self,variants:&[ScopeVariant]) -> Option<&'a Scope<SC>> {
        for scope in self.scopes.iter().rev() {
            if variants.contains(&scope.variant) {
                return Some(scope);
            } else if scope.variant==ScopeVariant::Function {
                return None;
            }
        }
        unreachable!(); //root should always be Function type
    }


// vars and identifiers:

    /// Insert an identifier into the current scope
    fn insert_ident(&mut self,name: String, variant:IdentifierVariant<SC>) -> Result<(),Error> {
        self.scopes.last_mut().unwrap().insert_ident(name,variant)
    }

    /// Find identity in scopes
    /// Variables/arrays are only visible in the current function and global scope
    fn find_ident<'a>(&'a self,name:&String) -> Option<&'a Identifier<SC>> {
        let mut allow_vars = true; //still in same function
        for scope in self.scopes.iter().rev() { //top to bottom
            match scope.identifiers.get(name) {
                // always visible
                Some(id@Identifier{variant:
                    IdentifierVariant::Function{..}|
                    IdentifierVariant::Constant{..}|
                    IdentifierVariant::Enum{..},
                    ..
                }) => {return Some(&id);}
                // variable/array
                Some(id) if allow_vars || scope.global => {return Some(&id);}
                _ => {}
            }
            // turn off after function scope
            if matches!(scope.variant,ScopeVariant::Function) {
                allow_vars=false;
            }
        }
        None
    }


    /// Get a temporary variable
    fn get_tmp(&mut self) -> Reg {
        self.scopes.last_mut().unwrap().get_tmp()
    }
    /// Release temporary variable again
    /// Must be released in reverse order of get_tmp!!!
    fn release_tmp(&mut self, tv:Option<Reg>) {
        if let Some(tv) = tv {
            self.scopes.last_mut().unwrap().release_tmp(tv);
        }
    }

// public access

    /// Create new instance
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            fn_sizes: HashMap::new(),
            global_size: 0,
            meta: ProgramMeta{scopes: HashMap::new(), instr: vec![]}
        }
    }
    /// Create base scope from scratch, with no variables
    pub fn create_basescope(&mut self, name:String) -> &mut Self {
        self.scopes.push(Scope::new_base_scope(name));
        self
    }
    /// Add system call to the scopes
    pub fn add_syscall(&mut self, name:&str, call: SC, check: SysCallParamCheck) -> &mut Self{
        self.scopes.last_mut().unwrap().insert_ident(name.to_string(),IdentifierVariant::SysCall {param_check: check, call}).unwrap();
        self
    }
    /// Compile statements into instructions; this also adds the root scope
    pub fn compile(mut self, program:Vec<Statement>) -> Result<(Vec<Instr<usize,usize,SC>>,ProgramMeta),CompileError> {
        // Create instructions
        let mut instr = Vec::new();
        let mut functions = Vec::new();
        instr.push(MkOrInstr::Instr(
            Instr::Init{index:"@root.START".to_string(),stack:"@root".to_string()},
            InstrMeta{ scope: "@init".to_string(), tmp_vars: vec![] }
        ));
        self.meta.scopes.insert("@init".to_string(),ScopeMeta{ //specifically for first instruction, when the mepty stack functions as a single program counter
            variant: ScopeMetaVariant::Function(1),
            variables: vec![]
        });


        // First, set up root scope
        let mut root = Scope::new_base_scope("@root".to_string());
        root.stack_vars=1; // allocate main PC
        root.stack_max=1;
        self.scopes.push(root);
        self.meta.scopes.insert("@root".to_string(),ScopeMeta{
            variant: ScopeMetaVariant::Function(0),
            variables: vec![]
        });
        
        


        debug!("C: initial instructions");
        instr.push(MkOrInstr::Marker("@root.START".to_string()));
    
        // Recursively move through statements
        debug!("C: recusive compile");
        self.compile_statements(program,&mut instr,&mut functions)?;
        self.fn_sizes.insert("@root".to_string(),self.scopes.last().unwrap().stack_max);
        if let ScopeMetaVariant::Function(size)= &mut self.meta.scopes.get_mut(&self.scopes.last().unwrap().name).unwrap().variant {
            *size = self.scopes.last().unwrap().stack_max;
        }


        // Add JUMP to start of program (i.e. loop)
        debug!("C: add jump to start and functions");
        instr.push(MkOrInstr::Instr(Instr::Jump{index:"@root.START".to_string()},self.imeta()));
    
        // Add functions
        instr.append(&mut functions);
    
        // Determine final locations of jumps
        debug!("C: create markers");

        let mut markers:HashMap<String,usize> = HashMap::new();
    
        let mut i:usize = 0;
        for mk_instr in instr.iter() {
            match mk_instr {
                MkOrInstr::Marker(m) => {markers.insert(m.clone(),i);}
                MkOrInstr::Instr(_,_) => {i+=1;}
            }
        }
        
        // Determine size of global scope
        self.global_size = *self.fn_sizes.get("@root").unwrap();
    
        // Create final code, without markers, and with valid jump values, and with shifted ABS values and correct stack sizes
        debug!("C: replace markers, stack sizes and offset absolute stack relations");
        
        Ok((instr
            .into_iter()
            .filter_map(|mki|
                match mki {
                    MkOrInstr::Marker(_) => None,
                    MkOrInstr::Instr(i,meta) => {
                        self.meta.instr.push(meta);
                        Some(self.postprocess_instr(i,&markers,self.global_size as i32-1))
                    }
                })
            .collect(),
            self.meta
        ))
    }
    
    
    

// helper functions

    /// Helper function for comparing function arguments with the list of required parameters
    pub fn check_fn_args(params:&Vec<FnParamVariant>,args:&Vec<FnParamVariant>) -> Result<(),CompileError> {
        if params.len() != args.len() {
            return Err(format!("Expected {} arguments for function, got {}",params.len(),args.len()));
        }
        for i in 0..params.len() {
            match (params[i],args[i]) {
                (FnParamVariant::Array,FnParamVariant::Array) |
                (FnParamVariant::Reference,FnParamVariant::Reference) |
                (FnParamVariant::Value,FnParamVariant::Reference) |
                (FnParamVariant::Value,FnParamVariant::Value) => {}
                (p,a) => return Err(format!("Parameter type {a:?} cannot be interpreted as type {p:?}"))
            }
        }
        Ok(())
    }
    /// Get name of the current top scope
    fn get_scope_name(&self) -> &String {
        &self.scopes.last().unwrap().name
    }
}
















////////// SCOPES

impl<SC: Clone+std::fmt::Debug> Scope<SC> {
// subscopes
    pub fn new_base_scope(name:String) -> Self {
        Self {
            name,
            global: true,
            identifiers: HashMap::new(),
            variant: ScopeVariant::Function,
            stack_vars: 0,
            stack_tmp: 0,
            stack_max: 0, 
            tmp_vars: vec![],
        }
    }

    /// Create subscope, assuming the type is not function
    pub fn create_subscope(&self, name: String, variant:ScopeVariant) -> Self {
        assert!(self.stack_tmp==self.tmp_vars.len(),"Attempt to create subscope before all temporary variables have been released");
        Self {
            name: format!("{}::{}{}",self.name,variant.prefix(),name).to_string(),
            global: self.global, //TODO: should this be allowed? or should only the absolute root level scope be global?
            identifiers: HashMap::new(),
            variant: variant,
            tmp_vars: Vec::new(),
            stack_vars: self.stack_vars,// TODO: check if this works (this would mean a scope cannot be opened while temp vars are in use, as their use is non-linear)
            stack_max: self.stack_vars-self.tmp_vars.len(),
            stack_tmp: 0,
        }
    }
    /// Create function subscope
    pub fn create_fn_scope(&self, name:String) -> Self {
        let mut scope = self.create_subscope(name,ScopeVariant::Function);
        scope.stack_vars = 2; // PC + return addr
        scope.stack_max = 2;
        scope.global=false;
        scope
    }
    /// Absorb subscope (register the maximum stack size required in subscope)
    pub fn absorb_subscope(&mut self, sub: Scope<SC>) {
        if !matches!(sub.variant,ScopeVariant::Function) {
            self.stack_max = self.stack_max.max(sub.stack_max);
        }
    }

// identifiers & variables

    /// Insert identifier into scope
    pub fn insert_ident(&mut self, name: String, variant:IdentifierVariant<SC>) -> Result<(),CompileError> {
        let prev = self.identifiers.insert(name.clone(),Identifier{
            name: format!("{}::{}",self.name,name),
            variant: variant,
        });
        if prev.is_some() {
            Err(format!("Identifier {name} already in use: {prev:?}"))
        } else {Ok(())}
    }
    
    /// Returns a temp variable
    /// If previously used tmp vars are available, use those.
    /// Else, create new one.
    pub fn get_tmp(&mut self) -> Reg {
        if self.tmp_vars.len()!=0 {
            return self.tmp_vars.pop().unwrap();
        } else {
            let tv = self.stack_vars+self.stack_tmp;
            self.stack_tmp+=1;
            self.stack_max = self.stack_max.max(self.stack_vars+self.stack_tmp);
            Reg::Var(StackRef::Rel(-(tv as i32)))
        }
    }
    /// Release a temporary variable so it can be used again later.
    /// Temp vars should be released in the reverse order of get_tmp.
    pub fn release_tmp(&mut self, tv:Reg) {
        self.tmp_vars.push(tv);
    }
}
















///////// POST PROCESSING

impl FnArg {
    fn absshift(self,shift:i32) -> Self {
        match self {
            FnArg::Val(r) => FnArg::Val(r.absshift(shift)),
            FnArg::VarRef(r) => FnArg::VarRef(r.absshift(shift)),
            FnArg::ArrayRef(r) => FnArg::ArrayRef(r.absshift(shift)),
        }
    }
}

impl Reg {
    /// Shift all absolute references
    fn absshift(self,shift:i32) -> Self {
        match self {
            Reg::Const(x) => Reg::Const(x),
            Reg::Var(sr) => Reg::Var(sr.absshift(shift)),
            Reg::VarRef(sr) => Reg::VarRef(sr.absshift(shift)),
        }
    }
}

impl ArrayReg {
    /// Shift all absolute references
    fn absshift(self,shift:i32) -> Self {
        match self {
            ArrayReg::Array(sr, sz) => ArrayReg::Array(sr.absshift(shift),sz),
            ArrayReg::ArrayRef(sr) => ArrayReg::ArrayRef(sr.absshift(shift)),
        }
    }
}

impl StackRef {
    /// Shift absolute references
    fn absshift(self,shift:i32) -> Self {
        match self {
            StackRef::Rel(x) => StackRef::Rel(x),
            StackRef::Abs(x) => StackRef::Abs(x+shift),
        }
    }
}


