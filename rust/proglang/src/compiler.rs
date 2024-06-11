use parser::{Statement,Expr,FnParam,UnaryOperator,BinaryOperator};

const MAX_ARRAY_LEN:usize=512;



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

pub fn find_in_scopes(&scopes: Vec<Scope>,name:String) -> Option<&Identifier> {
    let mut allow_vars = true;
    for scope in scopes.iter().rev() {
        match scope.identifiers.get(name) {
            Some(id@Identifier{_name,variant:IdentifierVariant::Function|IdentifierVariant::Constant|IdentifierVariant::Enum}) => {return Some(&id);}
            Some(id) if allow_vars || scope.global => {return Some(&id);}
            None => {}
        }
        // non-constants outside of current function scope and not in global scopes are not allowed
        // so turn off after function scope
        allow_vars = allow_vars && !matches!(scope.variant,ScopeVariant::Function);
    }
    None
}



fn recursive_compile(&mut statements:Vec<Statement>,&mut scopes:Vec<Scope>) -> Result<(),Error> {
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


}


pub fn compile(&mut statements:Vec<Statement>) -> Result<(),Error> {

    let mut scopes: Vec<Scope> = {
        Scope{
            name: "root",
            global: true,
            identifiers: HashMap::new(),
        }
    };

    recursive_compile(&mut statements, &mut scopes)
}