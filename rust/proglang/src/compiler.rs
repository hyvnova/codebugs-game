use parser::{Statement,Expr,FnParam,UnaryOperator,BinaryOperator};

const MAX_ARRAY_LEN:usize=512;




#[derive(Debug)]
enum MkInstr {
    Instr(Instr<String>),
    Marker(String),
}

type X = (); // TODO references/const

#[derivE(Debug)]
enum Instr<L> {
    BinaryOperator{op:BinaryOperator,lhs:X,rhs:X,res:X},
    UnaryOperator{op:UnaryOperator,rhs:X,res:X},
    ArrayOperator{array:X,index:X,res:X},
    ArrayAssign{array:X,index:X,value:X},
    Jump{index:L},
    JumpUnless{index:L,condition:X},
    FnCall{index:L, params:Vec<X>, res:X}, //todo
    SystemCall{}, //todo
    Return{stack:L},
}

impl Instr<String> {
    fn fill_marker(self,markers:HashMap<String,usize>) -> Instr<usize> {
        match self {
            Instr::Jump{index} => Instr::Jump{index:markers.get(index).unwrap()},
            i@Instr::JumpUnless{index,..} => Instr::JumpUnless{index:markers.get(index).unwrap(),..i},
            i@FnCall{index,..} => Instr::FnCall{index:markers.get(index).unwrap(),..i},
            i => i as Instr<usize>,
        }
    }
}








#[derive(Debug)]
pub struct Identifier {
    name: String,
    variant: IdentifierVariant,
}

#[derive(Debug)]
pub enum IdentifierVariant {
    Variable{
        r#type:Option<String>,
    },
    RefVariable,
    Array{
        r#type:Option<String>,
        size:usize,
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
    /*If,
    Else,
    While,
    Loop,*/
    Function,
    Block,
}

pub struct Scope {
    name: String,
    global: bool,
    identifiers: HashMap<String>,
    variant: ScopeVariant,
}

impl Scope {
    fn insert(&mut self, name: String, variant:IdentifierVariant) -> Result<(),Error> {
        let prev = self.identifiers.insert(Identifier{
            name: self.name+"::"+name,
            variant: variant,
        });
        if let Some(id) = prev {
            return Err(format!("Identifier {name} already in use: {prev:?}"));
        }
    }
}






/// Attempt to evaluate expression as a constant, given a list of scopes
pub fn const_expr_eval(&scopes:Vec<scope>,&expr:Expr) -> Result<i32,Error> {
    match expr {
        Num(x) => Ok(x),
        Op{op,lhs,rhs} => op.eval(const_expr_eval(lhs)?,const_expr_eval(rhs)?),
        Unary{op,rhs} => op.eval(const_expr_eval(rhs)?),
        Variable(name) => {
            match find_in_scopes(&scopes,name) {
                Ok(Identifier{name,variant:IdentifierVariant::Constant{value}}) => Ok(value),
                _ => Err(format!("{name} does not name a constant in scope {scopes.last().unwrap().name}")),
            }
        }
        EnumVariant{name:String,variant:String} => {
            match find_in_scopes(&scopes,name) {
                Ok(Identifier{name,variant:IdentifierVariant::Enum{variants}}) => {
                    match variants.get(variant) {
                        Some(value) => Ok(value),
                        _ => Err("Variant {variant} not found in enum {name}")
                    }
                },
                _ => Err(format!("{name} does not name a constant in scope {scopes.last().unwrap().name}")),
            }
        }
        e => Err("Expression {e:?} cannot be computed as a constant"),
    }
}







/// Reduce parts of expression that can be evaluated as constants
pub fn reduce_const(&scopes:Vec<scope>,&expr:Expr)-> Result<Expr,Error> {
    match expr {
        Num(x) => Num(x),
        Op{op,lhs,rhs} => {
            let lhs = reduce_const(scopes,lhs);
            let rhs = reduce_const(scopes,rhs);
            Ok(if let Num(x) = lhs && let Num(y) = rhs {
                Num(op.eval(x,y)?)
            } else {
                Op{op,lhs,rhs}
            })
        }
        Unary{op,rhs} => {
            let rhs = reduce_const(scopes,rhs);
            Ok(if let Num(y) = rhs {
                Num(op.eval(y))
            } else {
                Unary{op,lhs,rhs}
            })
        }
        Variable(name) => {
            match find_in_scopes(scopes,name) {
                Ok(Identifier{variant:IdentifierVariant::Constant{value}}) => Ok(Num(value)),
                Ok(Identifier{variant:IdentifierVariant::Variable}) => Ok(Variable(name)),
                _ => Err(format!("{name} does not name a constant in scope {scopes.last().unwrap().name}")),
            }
        }
        EnumVariant{name:String,variant:String} => {
            match find_in_scopes(&scopes,name) {
                Ok(Identifier{name,variant:IdentifierVariant::Enum{variants}}) => {
                    match variants.get(variant) {
                        Some(value) => Ok(Num(value)),
                        _ => Err(format!("Variant {variant} not found in enum {name}"))
                    }
                },
                _ => Err(format!("{name} does not name a constant in scope {scopes.last().unwrap().name}")),
            }
        }
        ArrayIndex{name,index} => Ok(ArrayIndex{name,index:reduce_const(scopes,index)?}),
        FnCall{name,args} => Ok(FnCall{name,args:args.iter().map(|arg| reduce_const(scopes,arg)?).collect()}),
    }
}





/// Find identifier in scopes
/// Variables and arrays are only visible in the current function and global scope
pub fn find_in_scopes(&scopes: Vec<Scope>,name:String) -> Option<&Identifier> {
    let mut allow_vars = true;
    for scope in scopes.iter().rev() {
        match scope.identifiers.get(name) {
            Some(id@Identifier{_name,variant:IdentifierVariant::Function|IdentifierVariant::Constant|IdentifierVariant::Enum}) => {return Some(&id);}
            Some(id) if allow_vars || scope.global => {return Some(&id);}
            _ => {}
        }
        // non-constants outside of current function scope and not in global scopes are not allowed
        // so turn off after function scope
        allow_vars = allow_vars && !matches!(scope.variant,ScopeVariant::Function);
    }
    None
}












fn recursive_compile(mut statements:Vec<Statement>,&mut scopes:Vec<Scope>) -> Result<(Vec<MkInstr>,Vec<MkInstr>),Error> {

    let mut instr:Vec<MkInstr> = Vec::new();

    // first, register all elements in the current scope
    // should we do this? just functions maybe?
    for statement in statements.iter() {
        match statement {
            Statement::VarDef{vars,r#type} => {
                // put variables into top scope
                let mut scope = scopes.last_mut();
                for name in names {
                    scope.insert(name, IdentifierVariant::Variable{r#type})?;
                }
            }
            Statement::ArrayDef{arrays,r#type} => {
                // put arrays into top scope
                for (name,size) in arrays {
                    let size = const_expr_eval(&scopes, &size)?;
                    let mut scope = scopes.last_mut();
                    if size<0 || size as usize > MAX_ARRAY_LEN {
                        return Err(format!("Array length {size} for array {name} in scope {scope.name} invalid"));
                    }
                    scope.insert(name, IdentifierVariant::Array{r#type,size})?;
                }
            }
            Statement::FnDef{name,params,code} => {
                // put function name and parameters into top scope
                scopes.last_mut().unwrap().insert(name, IdentifierVariant::Function{params})?;
            }
            Statement::ConstDef{name,value} => {
                // put const into top scope
                // constants are evaluated NOW so they can only use constants that were defined previously
                scopes.last_mut().unwrap().insert(
                    name,
                    IdentifierVariant::Constant{
                        value:const_expr_eval(&scopes,&value)?
                    }
                )?;
            }
            Statement::EnumDef{name,variants} => {
                // enums
                // note that multiple enum variants may have the same value, but not name
                // they essentially act as bundled constants
                let mut counted_variants = HashMap::new();
                let mut i:i32=-1;
                for (variant,value) in variants.iter() {
                    i = match value {Some(v)=>v,_=>i+1};
                    let old = counted_variants.insert(variant,i);
                    if let Some(_) = old {return Err(format!("Enum variant {variant} of enum {name} in scope {scopes.last().unwrap().name} is already defined"));}
                }
                // put enum into top scope
                scopes.last_mut().unwrap().insert(
                    name,
                    IdentifierVariant::Enum{
                        variants:counted_variants
                    }
                )?;
            }
            _ => {}
        }
    }





    // then, do the actual compilation to a format full of references (as number of variables on stack etc. and indices of functions are still uncertain)
    for statement in statements.iter() {
        match statement {
            VarAssign{name,value} => {
                // compute value
                // assign to variable
                todo!();
            }
            ArrayAssign{name,index,value} => {
                // compute index
                // compute value
                // assign to array
                todo!();
            }

            If{condition,code} => {
                let condition = reduce_const(&scopes,&condition);
                // open up if scope
                // add computations for computing expression
                // add conditional jump to END of scope
                // add instructions for code
                // register scope data
                // pop scope
                todo!();
            }
            IfElse{condition,yes,no} => {
                let condition = reduce_const(condition);
                // like if, but at end of first code jump to end of second instr
                todo!();
            }
            While{condition,code} => {
                // open up while scope
                // add computations for computing expression
                // add conditional JUMP to end of scope
                // add instructions for code
                // add JUMP to START of while scope
                // register scope data
                // pop scope
                todo!();
            }
            Loop(code) => {
                // open up loop scope
                // parse code
                // register max number of variables in scope
                // pop loop scope
                // add JUMP to start of current scope
                todo!();
            }
            Break => {
                // find nearest scope that is loop or while
                // fail when encountering fn scope
                // jumpt to END of scope (i.e. exactly afterwards)
                todo!();
            }
            Continue => {
                // find nearest scope that is loop or while
                // fail when encountering fn scope
                // loop: jump to START of that scope (i.e. first instruction after)
                // while: jump to before computation of condition
                todo!();
            }
            Return(value) => {
                // find parent fn scope
                // return (value, parent_fn)
                todo!();
            }

            Expr(expr) => {
                // just an expression not assigned to a variable (mostly function calls)
                // basically the same as variable assignment
                // but use fake target address
                // also, ignore if expr is constant
                todo!();
            }

            CodeBlock(statements) => {
                // create scope
                // parse things in block
                // register scope
                // pop scope
                todo!();
            }

            // definitions already handled
            VarDef => {}
            ArrayDef => {}
            ConstDef => {}
            EnumDef => {}
            
            // compute function 
            FnDef{name,params,code} => {
                // determine code for function 
                // must be appended to script after
                todo!();

                // set aside some things for system calls? they are special. need proper way to send output to outside of bugs

            }
        }
    }


    // if this is a function and NOT root (global), add a return statement
    if let ScopeVariant::Function = scopes.last().unwrap().variant && !scopes.last().unwrap().global {
        instr.push(MkInstr::Instr(Instr::Return{..}));
        todo!();
    }
    
    // return stuff
    instr
}




pub fn compile(&mut statements:Vec<Statement>) -> Result<Vec<Instr<usize>>,Error> {

    // initialise global scope
    let mut scopes: Vec<Scope> = {
        Scope{
            name: "root",
            global: true,
            identifiers: HashMap::new(),
            variant: ScopeVariant::Function,
        }
    };

    // recursively move through statements
    let (mut code,functions) = recursive_compile(&mut statements, &mut scopes)?;

    // add JUMP to start of program (reboot after main has finished)
    code.push(MkInstr(Instr::Jump("root.START")));

    // add functions
    code.append(functions);

    // determine final values of jumps
    let mut markers:HashMap<String,usize> = HashMap::new();
    markers.insert("root.START",0);

    let i:usize = 0;
    for mkinstr in instr {
        match mkinstr {
            Marker(m) => markers.insert(m,i),
            Instr(_) => {i+=1;}
        }
    }

    // create final code, without markers, and with valid jump values
    code
        .into_iter()
        .filter_map(|mki|
            match mki {
                MkInstr::Marker() => None,
                MkInstr::Instr(i) => Some(i.fill_marks(&markers)),
            })
        .collect()
        
}