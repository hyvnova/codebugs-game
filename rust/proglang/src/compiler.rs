use std::collections::HashMap;

use crate::parser::{Statement,Expr,FnParam};
use crate::operators::{UnaryOperator,BinaryOperator};

const MAX_ARRAY_LEN:usize=512;

type Error = String;


#[derive(Debug,Clone)]
pub struct FnArg {
    reg:Reg,
    mode:FnArgMode,
}

#[derive(Debug,Clone)]
pub enum FnArgMode {Value,Reference}


#[derive(Debug)]
enum MkInstr {
    Instr(Instr<String>),
    Marker(String),
}


#[derive(Debug,Clone)]
pub enum Instr<L> {
    BinaryOperator{op:BinaryOperator,lhs:Reg,rhs:Reg,res:Reg},
    UnaryOperator{op:UnaryOperator,rhs:Reg,res:Reg},
    ArrayIndex{array:Reg,index:Reg,res:Reg},
    ArrayAssign{array:Reg,index:Reg,value:Reg},
    Jump{index:L},
    JumpUnless{index:L,condition:Reg},
    FnCall{index:L, params:Vec<FnArg>, res:Reg, stack:L},
    SystemCall{}, //todo
    Return{stack:L,value:Reg},
}

impl Instr<String> {
    fn fill_markers(self,markers:&HashMap<String,usize>,fnsizes:&HashMap<String,usize>,absshift:i32) -> Instr<usize> {
        match self {
            Instr::Jump{index} => Instr::Jump{index:*markers.get(&index).unwrap()},
            Instr::JumpUnless{index,condition} =>
                Instr::JumpUnless{index:*markers.get(&index).unwrap(),condition:condition.absshift(absshift)},
            Instr::FnCall{index,params,res,stack} => 
                Instr::FnCall{
                    index:*markers.get(&index).unwrap(),
                    params:params.into_iter().map(|x| FnArg{reg:x.reg.absshift(absshift),mode:x.mode}).collect(),
                    res:res.absshift(absshift),
                    stack:*fnsizes.get(&stack).unwrap()},

            Instr::Return { stack, value } =>
                Instr::Return { stack:*fnsizes.get(&stack).unwrap(), value:value.absshift(absshift) }, // get stack size of function by name

            //i => i as Instr<usize>,
            Instr::ArrayAssign { array, index, value } =>
                Instr::ArrayAssign { array, index:index.absshift(absshift), value:value.absshift(absshift) },
            Instr::ArrayIndex { array, index, res } =>
                Instr::ArrayIndex { array, index:index.absshift(absshift), res:res.absshift(absshift) },
            Instr::BinaryOperator { op, lhs, rhs, res } =>
                Instr::BinaryOperator { op, lhs:lhs.absshift(absshift), rhs:rhs.absshift(absshift), res:res.absshift(absshift) },
            Instr::SystemCall {  } => Instr::SystemCall {  },
            Instr::UnaryOperator { op, rhs, res } =>
                Instr::UnaryOperator { op, rhs:rhs.absshift(absshift), res:res.absshift(absshift) },
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

impl Reg {
    pub fn absshift(self,shift:i32) -> Self {
        match self {
            Reg::Const(x) => Reg::Const(x),
            Reg::Var(sr) => Reg::Var(sr.absshift(shift)),
            Reg::VarRef(sr) => Reg::VarRef(sr.absshift(shift)),
            Reg::Array(sr, sz) => Reg::Array(sr.absshift(shift),sz),
            Reg::ArrayRef(sr) => Reg::ArrayRef(sr.absshift(shift)),
        }
    }
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
pub enum StackRef {
    Rel(i32), // relative to top of stack
    Abs(i32), // stack index
}

impl StackRef {
    pub fn absshift(self,shift:i32) -> Self {
        match self {
            StackRef::Rel(x) => StackRef::Rel(x),
            StackRef::Abs(x) => StackRef::Abs(x+shift),
        }
    }
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
    // VariableRef{
    //     reg:Reg,
    // },
    Array{
        r#type:Option<String>,
        // size:usize,
        reg:Reg,
    },
    // ArrayRef{
    //     reg:Reg,
    // },
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
        ("@".to_owned()+match self {
            Self::If => "if",
            // Self::IfElse => "ifelse",
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
        scope.global=false;
        scope
    }
    /// reabsorb subscope
    pub fn absorb_sub(&mut self, sub: Scope) {
        if !matches!(sub.variant,ScopeVariant::Function) {
            self.max_variables = self.max_variables.max(sub.max_variables);
        }
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

/// Return top scope matching any of the supplied scope variants
/// Stop after finding a function scope
pub fn find_scope_variant<'a>(scopes: &'a Vec<Scope>,variants:&[ScopeVariant]) -> Option<&'a Scope> {
    for scope in scopes.iter().rev() {
        if variants.contains(&scope.variant) {
            return Some(scope);
        } else if scope.variant==ScopeVariant::Function {
            return None;
        }
    }
    unreachable!();
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
                // Some(Identifier{variant:IdentifierVariant::VariableRef{..},..}) => Ok(Expr::Variable(name.clone())),
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
                Some(Identifier{variant:IdentifierVariant::Array{..},..}) => 
                    Ok(Expr::ArrayIndex{name:name.clone(),index:Box::new(reduce_const(scopes,index)?)}),
                _ => Err(format!("{name} does not name an array in scope {}",scopes.last().unwrap().name)),
            }
        },
        Expr::FnCall{name,args} => {
            match find_in_scopes(&scopes,name) {
                Some(Identifier{variant:IdentifierVariant::Function{params},..}) => // only compile arguments that are of VALUE/REF type, since arrays are seens as variables, that aren't variables in this scope
                    Ok(Expr::FnCall{
                        name:name.clone(),
                        args:args.iter().zip(params.iter()).map(|(arg,param)|
                            if !matches!(param,FnParam::Array(_)) {
                                reduce_const(scopes,arg)
                            } else {Ok(arg.clone())}
                        ).collect::<Result<Vec<_>,_>>()?
                    }),
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
                // Some(Identifier{variant:IdentifierVariant::VariableRef{reg:r},..}) => *r,
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
                // Some(Identifier{variant:IdentifierVariant::ArrayRef{reg:r},..}) => *r,
                _ => unreachable!(),
            };
        
            instr.push(MkInstr::Instr(Instr::ArrayIndex{array:array,index:reg_index,res:reg}));
            (instr,reg,tv)
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

            let mut instr = Vec::new();
            let mut tvs:Vec<Option<usize>> = Vec::new(); //to store temp vars
        
            // get function
            let fnid = find_in_scopes(scopes,name).unwrap().clone();

            // get args
            let params = match fnid {
                Identifier{variant:IdentifierVariant::Function{params},..} => params,
                _=>unreachable!(),
            };

            // check number of args
            if params.len() != args.len() {
                return Err(format!("Function {name} expected {} arguments but received {}",params.len(),args.len()));
            }

            // parse arguments
            let params = args.iter().zip(params.iter()).map(|(val,param)| 
                Ok::<FnArg,Error>(match param {
                    FnParam::Value(_) => {
                        // argument can be any expression
                        let (mut i,reg,tv) = calc_expression(scopes,expr,None)?;
                        instr.append(&mut i);
                        tvs.push(tv);
                        FnArg{reg,mode:FnArgMode::Value}
                    }
                    FnParam::Reference(_) => {
                        // argument must be a variable(name) where name refers to any (ref)variable
                        if let Expr::Variable(name) = val {
                            match find_in_scopes(scopes,name).unwrap() {
                                Identifier{variant:IdentifierVariant::Variable{reg,..},..} =>
                                    FnArg{reg:*reg,mode:FnArgMode::Reference},
                                _=>unreachable!(),
                            }
                        } else {unreachable!();}
                    }
                    FnParam::Array(_) => {
                        // argument must be Variable(name) where name refers to an array
                        if let Expr::Variable(name) = val {
                            match find_in_scopes(scopes,name).unwrap() {
                                Identifier{variant:IdentifierVariant::Array{reg,..},..} =>
                                    FnArg{reg:*reg,mode:FnArgMode::Reference},
                                _=>unreachable!(),
                            }
                        } else {unreachable!();}
                    }
                })
            ).collect::<Result<Vec<FnArg>,_>>()?;

            // get output register
            let (reg,tv) = get_reg(scopes,reg);

            // call function
            instr.push(MkInstr::Instr(Instr::FnCall { index: name.clone(), params, res: reg, stack: name.clone() }));

            // free variables
            for tv in tvs {
                if let Some(tv)=tv {scopes.last_mut().unwrap().release_tmp(tv);}
            }


            (instr,reg,tv)
        },
        Expr::EnumVariant{..} => unreachable!(), // enum variants should have been converted to const already
    })
}









fn recursive_compile(statements:Vec<Statement>,scopes:&mut Vec<Scope>,fnsizes:&mut HashMap<String,usize>) -> Result<(Vec<MkInstr>,Vec<MkInstr>),Error> {

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
                                    StackRef::Abs(-(scope.variables as i32)) // depends on stack size of main program, which is unknown - maybe take abs0!=stack0... temporarily? fix them later? is ugly but oh well I suppose it needs to be done
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
                            // size:size,
                            reg:Reg::Array(
                                if scope.global {
                                    StackRef::Abs(-((scope.variables+size-1) as i32))
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


    let mut cnt_blk = 0u32;
    let mut cnt_if  = 0u32;
    let mut cnt_loop = 0u32;
    let mut cnt_while = 0u32;



    // then, do the actual compilation to a format full of references (as number of variables on stack etc. and indices of functions are still uncertain)
    for statement in statements.into_iter() {
        match statement {
            Statement::VarAssign{name,value} => {
                let value = reduce_const(scopes,&value)?;

                let reg = match find_in_scopes(scopes,&name) {
                    Some(Identifier{variant:IdentifierVariant::Variable{reg,..},..}) => *reg,
                    _ => unreachable!(),
                };

                let (mut i,_,_) = calc_expression(scopes,&value,Some(reg))?;
                instr.append(&mut i);

            }
            Statement::ArrayAssign{name,index,value} => {
                // compute index
                // compute value
                // assign to array
                let index = reduce_const(scopes,&index)?;
                let value = reduce_const(scopes,&value)?;

                let (mut i,reg_index,tv_index) = calc_expression(scopes,&index,None)?;
                instr.append(&mut i);

                let (mut i,reg_value,tv_value) = calc_expression(scopes,&value,None)?;
                instr.append(&mut i);

                if let Some(tv) = tv_value {scopes.last_mut().unwrap().release_tmp(tv);}
                if let Some(tv) = tv_index {scopes.last_mut().unwrap().release_tmp(tv);}

                let reg = match find_in_scopes(scopes,&name) {
                    Some(Identifier{variant:IdentifierVariant::Array{reg,..},..}) => *reg,
                    _ => unreachable!(),
                };

                instr.push(MkInstr::Instr(Instr::ArrayAssign{array:reg,index:reg_index,value:reg_value}));
            }

            Statement::If{condition,code} => {
                let condition = reduce_const(&scopes,&condition)?;
                // open up if scope
                // add computations for computing expression
                // add conditional jump to END of scope
                // add instructions for code
                // register scope data
                // pop scope
                let scope = scopes.last().unwrap().create_sub(format!("{}",cnt_if),ScopeVariant::If);
                cnt_if+=1;

                instr.push(MkInstr::Marker(scope.name.clone()+".START")); // not really needed, but useful for clarity

                let (i,reg,tv) = calc_expression(scopes,&condition,None)?;
                instr.push(MkInstr::Instr(Instr::JumpUnless{index:scope.name.clone()+".END",condition:reg}));
                if let Some(tv) = tv {scopes.last_mut().unwrap().release_tmp(tv);}

                let (mut i,mut i_fn)=recursive_compile(vec![*code],scopes,fnsizes)?;
                instr.append(&mut i);
                functions.append(&mut i_fn);

                instr.push(MkInstr::Marker(scope.name.clone()+".END"));

                let scope = scopes.pop().unwrap();
                scopes.last_mut().unwrap().absorb_sub(scope);
            }
            Statement::IfElse{condition,yes,no} => {
                let condition = reduce_const(scopes,&condition)?;
                // like if, but at end of first code jump to end of second instr
                let scope = scopes.last().unwrap().create_sub(format!("{}",cnt_if),ScopeVariant::If);
                cnt_if+=1;

                instr.push(MkInstr::Marker(scope.name.clone()+".START")); //not really needed

                let (i,reg,tv) = calc_expression(scopes,&condition,None)?;
                instr.push(MkInstr::Instr(Instr::JumpUnless{index:scope.name.clone()+".ELSE",condition:reg}));
                if let Some(tv) = tv {scopes.last_mut().unwrap().release_tmp(tv);}

                let (mut i,mut i_fn)=recursive_compile(vec![*yes],scopes,fnsizes)?;
                instr.append(&mut i);
                functions.append(&mut i_fn);

                instr.push(MkInstr::Instr(Instr::Jump{index:scope.name.clone()+".END"}));
                instr.push(MkInstr::Marker(scope.name.clone()+".ELSE"));

                let (mut i,mut i_fn)=recursive_compile(vec![*no],scopes,fnsizes)?;
                instr.append(&mut i);
                functions.append(&mut i_fn);

                instr.push(MkInstr::Marker(scope.name.clone()+".END"));

                let scope = scopes.pop().unwrap();
                scopes.last_mut().unwrap().absorb_sub(scope);
            }
            Statement::While{condition,code} => {
                let condition = reduce_const(scopes,&condition)?;
                // open up while scope
                // add computations for computing expression
                // add conditional JUMP to end of scope
                // add instructions for code
                // add JUMP to START of while scope
                // register scope data
                // pop scope
                let scope = scopes.last().unwrap().create_sub(format!("{}",cnt_while),ScopeVariant::Loop);
                cnt_while+=1;

                instr.push(MkInstr::Marker(scope.name.clone()+".START"));

                let (i,reg,tv) = calc_expression(scopes,&condition,None)?;
                instr.push(MkInstr::Instr(Instr::JumpUnless{index:scope.name.clone()+".END",condition:reg}));
                if let Some(tv) = tv {scopes.last_mut().unwrap().release_tmp(tv);}

                let (mut i,mut i_fn)=recursive_compile(vec![*code],scopes,fnsizes)?;
                instr.append(&mut i);
                functions.append(&mut i_fn);

                let scope = scopes.pop().unwrap();

                instr.push(MkInstr::Instr(Instr::Jump{index:scope.name.clone()+".START"}));
                instr.push(MkInstr::Marker(scope.name.clone()+".END"));
                
                scopes.last_mut().unwrap().absorb_sub(scope);
            }
            Statement::Loop(code) => {
                // open up loop scope
                // parse code
                // register max number of variables in scope
                // pop loop scope
                // add JUMP to start of current scope
                let scope = scopes.last().unwrap().create_sub(format!("{}",cnt_loop),ScopeVariant::Loop);
                cnt_loop+=1;

                instr.push(MkInstr::Marker(scope.name+".START"));

                let (mut i,mut i_fn)=recursive_compile(vec![*code],scopes,fnsizes)?;
                instr.append(&mut i);
                functions.append(&mut i_fn);

                let scope = scopes.pop().unwrap();

                instr.push(MkInstr::Instr(Instr::Jump{index:scope.name.clone()+".START"}));
                instr.push(MkInstr::Marker(scope.name.clone()+".END"));
                
                scopes.last_mut().unwrap().absorb_sub(scope);
            }
            Statement::Break => {
                // find nearest scope that is loop or while
                // fail when encountering fn scope
                // jumpt to END of scope (i.e. exactly afterwards)
                let loop_scope = find_scope_variant(scopes,&[ScopeVariant::While,ScopeVariant::Loop]);
                if let Some(loop_scope)=loop_scope {
                    instr.push(MkInstr::Instr(Instr::Jump{index:loop_scope.name.clone()+".END"}));
                } else {
                    return Err(format!("Break statement outside of loop in scope {}",scopes.last().unwrap().name))
                }
            }
            Statement::Continue => {
                // find nearest scope that is loop or while
                // fail when encountering fn scope
                // loop: jump to START of that scope (i.e. first instruction after)
                // while: jump to before computation of condition
                let loop_scope = find_scope_variant(scopes,&[ScopeVariant::While,ScopeVariant::Loop]);
                if let Some(loop_scope)=loop_scope {
                    instr.push(MkInstr::Instr(Instr::Jump{index:loop_scope.name.clone()+".START"}));
                } else {
                    return Err(format!("Continue statement outside of loop in scope {}",scopes.last().unwrap().name))
                }
            }
            Statement::Return(value) => {
                let value = reduce_const(scopes,&value.unwrap_or_else(||Expr::Num(0)))?;
                // find parent fn scope
                // return (value, parent_fn) if it is not global!!!
                let fn_scope = find_scope_variant(scopes,&[ScopeVariant::Function]).unwrap();
                if fn_scope.global {
                    return Err(format!("Return statement outside of function in scope {}",scopes.last().unwrap().name))
                }
                let fn_name = fn_scope.name.clone();

                let (mut i,reg,tv) = calc_expression(scopes,&value,None)?;
                instr.append(&mut i);
                if let Some(tv) = tv {scopes.last_mut().unwrap().release_tmp(tv);}
                instr.push(MkInstr::Instr(Instr::Return{stack:fn_name,value:reg}));
            }

            Statement::Expr(expr) => {
                // just an expression not assigned to a variable (mostly function calls)
                // basically the same as variable assignment
                // but use fake target address
                // also, ignore if expr is constant
                let expr = reduce_const(scopes,&expr)?;
                let (mut i,_,_) = calc_expression(scopes,&expr,Some(Reg::Const(0)))?;
                instr.append(&mut i);
            }

            Statement::CodeBlock(statements) => {
                // create scope
                // parse things in block
                // register scope
                // pop scope

                let scope = scopes.last().unwrap().create_sub(format!("{}",cnt_blk),ScopeVariant::Block);
                cnt_blk+=1;

                scopes.push(scope);

                let (mut i,mut i_fn) = recursive_compile(statements,scopes,fnsizes)?;
                instr.append(&mut i);
                functions.append(&mut i_fn);

                let scope = scopes.pop().unwrap();
                scopes.last_mut().unwrap().absorb_sub(scope);
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
                
                // make scope
                let mut scope = scopes.last().unwrap().create_fn(name);
                functions.push(MkInstr::Marker(scope.name.clone()+".START"));

                // add params
                for param in params.into_iter() {
                    match param {
                        FnParam::Value(name) =>
                            scope.insert(
                                name,
                                IdentifierVariant::Variable{
                                    reg:Reg::Var(StackRef::Rel(-(scope.variables as i32))),
                                    r#type:None}).unwrap(),
                        FnParam::Reference(name) =>
                            scope.insert(
                                name,
                                IdentifierVariant::Variable{
                                    reg:Reg::VarRef(StackRef::Rel(-(scope.variables as i32))),
                                    r#type:None}).unwrap(),
                        FnParam::Array(name) =>
                        scope.insert(
                            name,
                            IdentifierVariant::Array{
                                reg:Reg::ArrayRef(StackRef::Rel(-(scope.variables as i32))),
                                r#type:None}).unwrap(),
                    }
                    scope.variables+=1;
                    scope.max_variables+=1;
                }

                // parse code and add to functions
                let (mut i,mut i_fn) = recursive_compile(code, scopes, fnsizes)?;
                functions.append(&mut i);

                let scope = scopes.pop().unwrap();
                functions.push(MkInstr::Marker(scope.name.clone()+".END")); //mark end
                fnsizes.insert(scope.name.clone(),scope.max_variables); //register size
                scopes.last_mut().unwrap().absorb_sub(scope);

                functions.append(&mut i_fn); // add subfunctions afterwards


                // TODO set aside some things for system calls? they are special. need proper way to send output to outside of bugs

            }
        }
    }


    // if this is a function, and if it does not already end on a return statement, and it is not the global scope, add a return statement
    if matches!(scopes.last().unwrap().variant,ScopeVariant::Function{..}) && !scopes.last().unwrap().global{
        if !matches!(
            instr.iter().rev().filter(|mki| matches!(mki,MkInstr::Instr(_))).next(),
            Some(MkInstr::Instr(Instr::Return{..}))
        ) { //last instr (not marker!) is not a Return statement
            instr.push(MkInstr::Instr(Instr::Return{stack:scopes.last().unwrap().name.clone(),value:Reg::Const(0)}));
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

    let mut fnsizes: HashMap<String,usize> = HashMap::new();

    // recursively move through statements
    let (mut code,mut functions) = recursive_compile(statements, &mut scopes, &mut fnsizes)?;

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
    let global_size = *fnsizes.get("@root").unwrap();
    Ok(code
        .into_iter()
        .filter_map(|mki|
            match mki {
                MkInstr::Marker(_) => None,
                MkInstr::Instr(i) => Some(i.fill_markers(&markers,&fnsizes,global_size as i32)),
            })
        .collect())
        
}