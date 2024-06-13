use std::collections::HashMap;

use crate::parser::{Statement,Expr,FnParam};
use crate::operators::{UnaryOperator,BinaryOperator};

const MAX_ARRAY_LEN:usize=512;

type Error = String;


#[derive(Debug)]
enum MkInstr {
    Instr(Instr<String>),
    Marker(String),
}

type X = (); // TODO references/const

#[derive(Debug,Clone)]
pub enum Instr<L> {
    BinaryOperator{op:BinaryOperator,lhs:Reg,rhs:Reg,res:Reg},
    UnaryOperator{op:UnaryOperator,rhs:Reg,res:Reg},
    ArrayIndex{array:Reg,index:Reg,res:Reg},
    ArrayAssign{array:Reg,index:Reg,value:Reg},
    Jump{index:L},
    JumpUnless{index:L,condition:Reg},
    FnCall{index:L, params:Vec<Reg>, res:Reg, stack:L},
    SystemCall{}, //todo
    Return{stack:L},
}

impl Instr<String> {
    fn fill_marker(self,markers:&HashMap<String,usize>) -> Instr<usize> {
        match self {
            Instr::Jump{index} => Instr::Jump{index:*markers.get(&index).unwrap()},
            Instr::JumpUnless{index,condition} => Instr::JumpUnless{index:*markers.get(&index).unwrap(),condition},
            Instr::FnCall{index,params,res,stack} => Instr::FnCall{index:*markers.get(&index).unwrap(),params,res,stack:todo!()},

            Instr::Return { stack } => Instr::Return { stack: todo!() }, // get stack size of function by name

            //i => i as Instr<usize>,
            Instr::ArrayAssign { array, index, value } => Instr::ArrayAssign { array, index, value },
            Instr::ArrayIndex { array, index, res } => Instr::ArrayIndex { array, index, res },
            Instr::BinaryOperator { op, lhs, rhs, res } => Instr::BinaryOperator { op, lhs, rhs, res },
            Instr::SystemCall {  } => Instr::SystemCall {  },
            Instr::UnaryOperator { op, rhs, res } => Instr::UnaryOperator { op, rhs, res },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    Const(i32), //constant value. storing at this address does nothing
    Var(StackRef), //take value at this place
    VarRef(StackRef), //take value at place stored in this place
    Array(StackRef,usize), //array starting at this place with this length
    ArrayRef(StackRef), //.... the array pointed to by this place
}

#[derive(Debug, Clone, Copy)]
pub enum StackRef {
    Rel(i32), // relative to top of stack
    Abs(i32), // stack index
}








#[derive(Debug,Clone)]
pub struct Identifier {
    name: String,
    variant: IdentifierVariant,
}

#[derive(Debug,Clone)]
pub enum IdentifierVariant {
    Variable{
        r#type:Option<String>,
        reg:Reg,
    },
    VariableRef{
        reg:Reg,
    },
    Array{
        r#type:Option<String>,
        size:usize,
        reg:Reg,
    },
    ArrayRef{
        reg:Reg,
    },
    Function{
        params:Vec<FnParam>
    },
    Constant{
        value:i32,
    },
    Enum{
        variants:HashMap<String,i32>,
    }
}






#[derive(Debug)]
pub enum ScopeVariant {
    If,
    IfElse,
    While,
    Loop,
    Function,
    Block,
}
impl ScopeVariant {
    fn prefix(&self) -> String {
        ("@".to_owned()+match self {
            Self::If => "if",
            Self::IfElse => "ifelse",
            Self::While => "while",
            Self::Loop => "loop",
            Self::Function => "fn",
            Self::Block => "blk",
        }+"_").to_string()
    }
}

pub struct Scope {
    name: String,
    global: bool,
    identifiers: HashMap<String,Identifier>,
    variant: ScopeVariant,
    tmp_vars: Vec<usize>,
    variables: usize,
    max_variables: usize,
}


impl Scope {
    /// Insert identifier into scope
    pub fn insert(&mut self, name: String, variant:IdentifierVariant) -> Result<(),Error> {
        let prev = self.identifiers.insert(name.clone(),Identifier{
            name: format!("{}::{}",self.name,name),
            variant: variant,
        });
        if prev.is_some() {
            Err(format!("Identifier {name} already in use: {prev:?}"))
        } else {Ok(())}
    }
    /// create subscope, assuming the type is not function
    pub fn create_sub(&self, name: String, variant:ScopeVariant) -> Self {
        Self {
            name: format!("{}::{}{}",self.name,variant.prefix(),name).to_string(),
            global: self.global, //TODO: should this be allowed? or should only the absolute root level scope be global?
            identifiers: HashMap::new(),
            variant: variant,
            tmp_vars: Vec::new(),
            variables: self.variables-self.tmp_vars.len(),// TODO: check if this works (this would mean a scope cannot be opened while temp vars are in use, as their use is non-linear)
            max_variables: self.variables-self.tmp_vars.len(),
        }
    }
    /// create function scope
    pub fn create_fn(&self, name:String) -> Self {
        let mut scope = self.create_sub(name,ScopeVariant::Function);
        scope.variables = 2; // PC + return addr
        scope.max_variables = 2;
        scope
    }
    /// create temporary variable
    /// if previously used tempvars are available, use those (pop)
    /// else, increase the number of temp vars used and create new one
    pub fn get_tmp(&mut self) -> usize {
        if self.tmp_vars.len()!=0 {
            return self.tmp_vars.pop().unwrap();
        } else {
            let tv = self.variables;
            self.variables+=1;
            self.max_variables = self.max_variables.max(self.variables);
            tv
        }
    }
    pub fn release_tmp(&mut self, tv:usize) {
        self.tmp_vars.push(tv);
    }
}








/// Find identifier in scopes
/// Variables and arrays are only visible in the current function and global scope
pub fn find_in_scopes<'a>(scopes: &'a Vec<Scope>,name:&String) -> Option<&'a Identifier> {
    let mut allow_vars = true;
    for scope in scopes.iter().rev() {
        match scope.identifiers.get(name) {
            Some(id@Identifier{variant:
                IdentifierVariant::Function{..}|
                IdentifierVariant::Constant{..}|
                IdentifierVariant::Enum{..},
                ..
            }) => {return Some(&id);}
            Some(id) if allow_vars || scope.global => {return Some(&id);}
            _ => {}
        }
        // non-constants outside of current function scope and not in global scopes are not allowed
        // so turn off after function scope
        allow_vars = allow_vars && !matches!(scope.variant,ScopeVariant::Function);
    }
    None
}







/// Reduce parts of expression that can be evaluated as constants
pub fn reduce_const(scopes:&Vec<Scope>,expr:&Expr)-> Result<Expr,Error> {
    match expr {
        Expr::Num(x) => Ok(Expr::Num(*x)),
        Expr::Op{op,lhs,rhs} => {
            let lhs = reduce_const(scopes,lhs)?;
            let rhs = reduce_const(scopes,rhs)?;

            Ok(if let (Expr::Num(x),Expr::Num(y)) = (&lhs,&rhs) {
                Expr::Num(op.eval(*x,*y)?)
            } else {
                Expr::Op{op:*op,lhs:Box::new(lhs),rhs:Box::new(rhs)}
            })
        }
        Expr::Unary{op,rhs} => {
            let rhs = reduce_const(scopes,rhs)?;
            Ok(if let Expr::Num(y) = rhs {
                Expr::Num(op.eval(y))
            } else {
                Expr::Unary{op:*op,rhs:Box::new(rhs)}
            })
        }
        Expr::Variable(name) => {
            match find_in_scopes(scopes,name) {
                Some(Identifier{variant:IdentifierVariant::Constant{value},..}) => Ok(Expr::Num(*value)),
                Some(Identifier{variant:IdentifierVariant::Variable{..},..}) => Ok(Expr::Variable(name.clone())),
                Some(Identifier{variant:IdentifierVariant::VariableRef{..},..}) => Ok(Expr::Variable(name.clone())),
                _ => Err(format!("{name} does not name a variable or constant in scope {}",scopes.last().unwrap().name)),
            }
        }
        Expr::EnumVariant{name,variant} => {
            match find_in_scopes(&scopes,name) {
                Some(Identifier{name,variant:IdentifierVariant::Enum{variants}}) => {
                    match variants.get(variant) {
                        Some(value) => Ok(Expr::Num(*value)),
                        _ => Err(format!("Variant {variant} not found in enum {name}"))
                    }
                },
                _ => Err(format!("{name} does not name an enum in scope {}",scopes.last().unwrap().name)),
            }
        }
        Expr::ArrayIndex{name,index} => {
            match find_in_scopes(&scopes,name) {
                Some(Identifier{variant:IdentifierVariant::Array{..}|IdentifierVariant::ArrayRef{..},..}) => 
                    Ok(Expr::ArrayIndex{name:name.clone(),index:Box::new(reduce_const(scopes,index)?)}),
                _ => Err(format!("{name} does not name an array in scope {}",scopes.last().unwrap().name)),
            }
        },
        Expr::FnCall{name,args} => {
            match find_in_scopes(&scopes,name) {
                Some(Identifier{variant:IdentifierVariant::Function{..},..}) => 
                    Ok(Expr::FnCall{name:name.clone(),args:args.iter().map(|arg| reduce_const(scopes,arg)).collect::<Result<Vec<_>,_>>()?}),
                _ => Err(format!("{name} does not name a function in scope {}",scopes.last().unwrap().name)),
            }
        },
    }
}








/// Attempt to evaluate expression as a constant, given a list of scopes
pub fn const_expr_eval(scopes:&Vec<Scope>,expr:&Expr) -> Result<i32,Error> {
    let expr = reduce_const(scopes,expr)?;
    if let Expr::Num(x) = expr {
        Ok(x)
    } else {
        Err(format!("Expression {expr:?} cannot be computed as a constant"))
    }
}











/*

    idea:
        calculate and expression, storing the result in REG


    howwww

    const -> write const to REG
    variable -> write value of variable to REG
    op -> 
        for all inputs, calculate their values
            if inputs are const or var, use those
            else, create/claim temp variable and use that
        calculate and store results of op, using inputs
        release temp variables, if appliccable
    unary -> see op
    enum -> unreachable
    arrayindex ->
        essentially like unary
            input is index
    fncall ->
        check if input format matches function parameters
            reference must be a variable
            array must be "variable"->array
        for all pass-by-values:
            treat like op/unary
        add fncall
            special for system call?
        release vars



    implementation using Option<Reg> -> Option<usize>
    if Reg is not defined (used in recursive calls):
        if variable/constant:
            return that,None
        else:
            get temp var as new reg
        
    calculate
    if reg WAS provided
        return instr, None
    else
        return instr, temp


    does this introduce the temp vars too early?
    maybe
    get temp var only AFTER processing the data


    if function is called with reg:
        result is stored in reg
    if not:
        function returns reg containing result
        and possibly temp var that has to be released after reading

    if result is stored in reg:
        reg_lhs,temp_lhs = calc(lhs)
        reg_rhs,temp_rhs = calc(rhs)
        release temp_rhs
        release temp_lhs

        reg_out,temp_out = (reg,none or Rel(>),temp())
        op(reg_lhs,reg_rhs,reg_out)
        return reg_out,temp_out




*/



/// Generate instructions for processing an expression
/// Assumes expression has been reduced to constants beforehand (i.e. variables are not constants, no enum variants)
/// and variables/arrays have been checked to exist
fn calc_expression(
    scopes: &mut Vec<Scope>,
    expr:&Expr,
    reg:Option<Reg> /* memory location to store result in */
) -> Result<(Vec<MkInstr>,Reg,Option<usize>),Error> {

    fn get_reg(scopes:&mut Vec<Scope>,reg:Option<Reg>) -> (Reg,Option<usize>) {
        if let Some(r) = reg {
            (r,None)
        } else {
            let tv=scopes.last_mut().unwrap().get_tmp();
            (Reg::Var(StackRef::Rel(-(tv as i32))),Some(tv))
        }
    }

    Ok(match expr {
        Expr::Num(x) => {
            if let Some(r)=reg {
                let mut instr = Vec::new();
                instr.push(MkInstr::Instr(Instr::UnaryOperator{op:UnaryOperator::Nop,rhs:Reg::Const(*x),res:r}));
                (instr,r,None) // store constant in reg
            } else {
                (Vec::new(),Reg::Const(*x),None) //just return the constant
            }
        }
        Expr::Variable(name) => {
            let rhs = match find_in_scopes(scopes,name) {
                Some(Identifier{variant:IdentifierVariant::Variable{reg:r,..},..}) => *r,
                Some(Identifier{variant:IdentifierVariant::VariableRef{reg:r},..}) => *r,
                _ => unreachable!(), // code was checked during reduce_const step, and should only have (referenced) variables
            };

            if let Some(r)=reg {
                let mut instr = Vec::new();
                instr.push(MkInstr::Instr(Instr::UnaryOperator{op:UnaryOperator::Nop,rhs,res:r})); // store variable
                (instr,r,None)
            } else {
                (Vec::new(),rhs,None) // just return variable
            }
        }
        Expr::Op{op,lhs,rhs} => {
            let (    instr_lhs,reg_lhs,tmp_lhs) = calc_expression(scopes,lhs,None)?;
            let (mut instr_rhs,reg_rhs,tmp_rhs) = calc_expression(scopes,rhs,None)?;
            if let Some(tv)=tmp_rhs {scopes.last_mut().unwrap().release_tmp(tv);}
            if let Some(tv)=tmp_lhs {scopes.last_mut().unwrap().release_tmp(tv);}
            
            let (reg,tv) = get_reg(scopes,reg);

            let mut instr = instr_lhs;
            //instr.append(instr_lhs);
            instr.append(&mut instr_rhs);
            instr.push(MkInstr::Instr(Instr::BinaryOperator{op:*op,lhs:reg_lhs,rhs:reg_rhs,res:reg}));
            (instr,reg,tv)
        },
        Expr::Unary{op,rhs} => {
            let (mut instr,reg_rhs,tmp_rhs) = calc_expression(scopes,rhs,None)?;
            if let Some(tv)=tmp_rhs {scopes.last_mut().unwrap().release_tmp(tv);}
            
            let (reg,tv) = get_reg(scopes,reg);
        
            instr.push(MkInstr::Instr(Instr::UnaryOperator{op:*op,rhs:reg_rhs,res:reg}));
            (instr,reg,tv)
        },
        Expr::ArrayIndex{name,index} => {
            // compute index, add indexing instr
            let (mut instr,reg_index,tmp_index) = calc_expression(scopes,index,None)?;
            if let Some(tv)=tmp_index {scopes.last_mut().unwrap().release_tmp(tv);}
            
            let (reg,tv) = get_reg(scopes,reg);

            let array = match find_in_scopes(scopes,name) {
                Some(Identifier{variant:IdentifierVariant::Array{reg:r,..},..}) => *r,
                Some(Identifier{variant:IdentifierVariant::ArrayRef{reg:r},..}) => *r,
                _ => unreachable!(),
            };
        
            instr.push(MkInstr::Instr(Instr::ArrayIndex{array:array,index:reg_index,res:reg}));
            (instr,reg,tv)
        },
        Expr::FnCall{name,args} => {
            todo!();
        },
        Expr::EnumVariant{..} => unreachable!(), // enum variants should have been converted to const already
    })
}









fn recursive_compile(statements:Vec<Statement>,scopes:&mut Vec<Scope>) -> Result<(Vec<MkInstr>,Vec<MkInstr>),Error> {

    let mut instr:Vec<MkInstr> = Vec::new();
    let mut functions:Vec<MkInstr> = Vec::new(); //for functions placed after this one

    // first, register all elements in the current scope
    // should we do this? just functions maybe?
    for statement in statements.iter() {
        match statement {
            Statement::VarDef{vars,r#type} => {
                // put variables into top scope
                let scope = scopes.last_mut().unwrap();
                for name in vars {
                    scope.insert(
                        name.clone(),
                        IdentifierVariant::Variable{
                            r#type:r#type.clone(),
                            reg:Reg::Var(
                                if scope.global {
                                    StackRef::Abs(todo!()) // depends on stack size of main program, which is unknown - maybe take abs0!=stack0... temporarily? fix them later? is ugly but oh well I suppose it needs to be done
                                } else {
                                    StackRef::Rel(-(scope.variables as i32))
                                }
                            )
                        }
                    )?;
                    scope.variables += 1;
                    scope.max_variables += 1;
                }
            }
            Statement::ArrayDef{arrays,r#type} => {
                // put arrays into top scope
                for (name,size) in arrays {
                    let size = const_expr_eval(&scopes, &size)?;
                    let scope = scopes.last_mut().unwrap();
                    if size<0 || size as usize > MAX_ARRAY_LEN {
                        return Err(format!("Array length {size} for array {name} in scope {} invalid",scope.name));
                    }
                    let size = size as usize;
                    scope.insert(
                        name.clone(),
                        IdentifierVariant::Array{
                            r#type:r#type.clone(),
                            size:size,
                            reg:Reg::Array(
                                if scope.global {
                                    StackRef::Abs(todo!())
                                } else {
                                    StackRef::Abs(-((scope.variables+size-1) as i32))
                                },
                                size
                            )
                        }
                    )?;
                    scope.variables += size;
                    scope.max_variables += size;
                }
            }
            Statement::FnDef{name,params,..} => {
                // put function name and parameters into top scope
                scopes.last_mut().unwrap().insert(
                    name.clone(), 
                    IdentifierVariant::Function{params:params.clone()}
                )?;
            }
            Statement::ConstDef{name,value} => {
                // put const into top scope
                // constants are evaluated NOW so they can only use constants that were defined previously
                let value = const_expr_eval(scopes,&value)?;
                scopes.last_mut().unwrap().insert(
                    name.clone(),
                    IdentifierVariant::Constant{value}
                )?;
            }
            Statement::EnumDef{name,variants} => {
                // enums
                // note that multiple enum variants may have the same value, but not name
                // they essentially act as bundled constants
                let mut counted_variants:HashMap<String,i32> = HashMap::new();
                let mut i:i32=-1;
                for (variant,value) in variants.into_iter() {
                    i = match value {Some(v)=>const_expr_eval(scopes,&v)?, _=>i+1};
                    let old = counted_variants.insert(variant.clone(),i);
                    if let Some(_) = old {return Err(format!("Enum variant {variant} of enum {name} in scope {} is already defined",scopes.last().unwrap().name));}
                }
                // put enum into top scope
                scopes.last_mut().unwrap().insert(
                    name.clone(),
                    IdentifierVariant::Enum{
                        variants:counted_variants
                    }
                )?;
            }
            _ => {}
        }
    }





    // then, do the actual compilation to a format full of references (as number of variables on stack etc. and indices of functions are still uncertain)
    for statement in statements.into_iter() {
        match statement {
            Statement::VarAssign{name,value} => {
                let value = reduce_const(scopes,&value)?;
                // compute value
                // assign to variable
                let reg = match find_in_scopes(scopes,&name) {
                    Some(Identifier{variant:IdentifierVariant::Variable{reg,..},..}) => {
                        reg
                    }
                    _ => return Err(format!("{name} does not name a variable in scope {}",scopes.last().unwrap().name)),
                };
                todo!();
                let mut occupied = scopes.last().unwrap().variables;
//                instr.append(calc_expression(scopes,occupied,value,var));
            }
            Statement::ArrayAssign{name,index,value} => {
                // compute index
                // compute value
                // assign to array
                todo!();
            }

            Statement::If{condition,code} => {
                let condition = reduce_const(&scopes,&condition);
                // open up if scope
                // add computations for computing expression
                // add conditional jump to END of scope
                // add instructions for code
                // register scope data
                // pop scope
                todo!();
            }
            Statement::IfElse{condition,yes,no} => {
                let condition = reduce_const(scopes,&condition);
                // like if, but at end of first code jump to end of second instr
                todo!();
            }
            Statement::While{condition,code} => {
                // open up while scope
                // add computations for computing expression
                // add conditional JUMP to end of scope
                // add instructions for code
                // add JUMP to START of while scope
                // register scope data
                // pop scope
                todo!();
            }
            Statement::Loop(code) => {
                // open up loop scope
                // parse code
                // register max number of variables in scope
                // pop loop scope
                // add JUMP to start of current scope
                todo!();
            }
            Statement::Break => {
                // find nearest scope that is loop or while
                // fail when encountering fn scope
                // jumpt to END of scope (i.e. exactly afterwards)
                todo!();
            }
            Statement::Continue => {
                // find nearest scope that is loop or while
                // fail when encountering fn scope
                // loop: jump to START of that scope (i.e. first instruction after)
                // while: jump to before computation of condition
                todo!();
            }
            Statement::Return(value) => {
                // find parent fn scope
                // return (value, parent_fn) if it is not global!!!
                todo!();
            }

            Statement::Expr(expr) => {
                // just an expression not assigned to a variable (mostly function calls)
                // basically the same as variable assignment
                // but use fake target address
                // also, ignore if expr is constant
                todo!();
                //calc_expression(scopes,,,Reg::Const(0));
            }

            Statement::CodeBlock(statements) => {
                // create scope
                // parse things in block
                // register scope
                // pop scope
                todo!();
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
                todo!();

                // set aside some things for system calls? they are special. need proper way to send output to outside of bugs

            }
        }
    }


    // if this is a function, and if it does not already end on a return statement, and it is not the global scope, add a return statement
    if matches!(scopes.last().unwrap().variant,ScopeVariant::Function{..}) && !scopes.last().unwrap().global{
        if !matches!(
            instr.iter().rev().filter(|mki| matches!(mki,MkInstr::Instr(_))).next(),
            Some(MkInstr::Instr(Instr::Return{..}))
        ) { //last instr (not marker!) is not a Return statement
            instr.push(MkInstr::Instr(Instr::Return{stack:todo!()}));
        }
    }
    
    // return stuff
    Ok((instr,functions))
}




pub fn compile(statements:Vec<Statement>) -> Result<Vec<Instr<usize>>,Error> {

    // initialise global scope
    let mut scopes: Vec<Scope> = vec![
        Scope{
            name: "@root".to_string(),
            global: true,
            identifiers: HashMap::new(),
            variant: ScopeVariant::Function,
            tmp_vars: Vec::new(),
            variables: 2, //PC and ref dump
            max_variables: 2,
        }
    ];

    // recursively move through statements
    let (mut code,mut functions) = recursive_compile(statements, &mut scopes)?;

    // add JUMP to start of program (reboot after main has finished)
    code.push(MkInstr::Instr(Instr::Jump{index:"@root.START".to_string()}));

    // add functions
    code.append(&mut functions);

    // determine final values of jumps
    let mut markers:HashMap<String,usize> = HashMap::new();
    markers.insert("@root.START".to_string(),0);

    let mut i:usize = 0;
    for mkinstr in code.iter() {
        match mkinstr {
            MkInstr::Marker(m) => {markers.insert(m.clone(),i);}
            MkInstr::Instr(_) => {i+=1;}
        }
    }

    // create final code, without markers, and with valid jump values
    Ok(code
        .into_iter()
        .filter_map(|mki|
            match mki {
                MkInstr::Marker(_) => None,
                MkInstr::Instr(i) => Some(i.fill_marker(&markers)),
            })
        .collect())
        
}