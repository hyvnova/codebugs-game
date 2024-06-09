

use chumsky::prelude::*;


#[derive(Clone, Debug)]
pub enum ProgramStatement { //limited code allowed in program
    VarDef(Vec<String>,Option<String>),
    ArrayDef(Vec<(String,Expr)>),
    ConstDef(String,Expr),
    EnumDef(String,Vec<(String,Option<i32>)>),
    FnDef(String,Vec<FnParam>,Vec<Statement>),
}

#[derive(Clone,Debug)]
pub enum FnParam {
    Value(String),
    Reference(String),
    Array(String),
}

#[derive(Clone, Debug)]
pub enum Statement {
    VarAssign{name:String,value:Expr}, //x=...;
    ArrayAssign{name:String,index:Expr,value:Expr}, //x[...]=...;
    IfElse{condition:Expr,yes:Box<Self>,no:Box<Self>}, // if ... else ...
    If{condition:Expr,yes:Box<Self>}, // if ...
    While(Expr,Box<Self>), // while (...) ...
    Loop(Box<Self>), // loop ...
    Break, // break;
    Continue, // continue;
    VarDef{vars:Vec<String>,r#type:Option<String>}, // var ...* [:...];
    ArrayDef{arrays:Vec<(String,Expr)>,r#type:Option<String>}, // array ...[...]* [:...];
    CodeBlock(Vec<Self>), // {...*}
}

#[derive(Clone, Debug)]
pub enum Expr {
    ArrayIndex{name:String,index:Box<Self>}, // ...[...]
    Op{op:String,lhs:Box<Self>,rhs:Box<Self>}, // ... ... ...
    Unary{op:String,rhs:Box<Self>}, // ... ...
    EnumVariant{name:String,variant:String}, // ...::...
    FnCall{name:String,args:Vec<Expr>}, // ...(...)
    Num(i32), // ...
    Variable(String), // ...
    // Parens(Box<Self>), // (...)
}


pub fn exprparser() -> impl Parser<char, Expr, Error=Simple<char>> {
    let expr: Recursive<_,Expr,_> = recursive (|expr: Recursive<_, Expr,_>| {
        let op = |c| just(c).padded();

        let int = text::int::<_,Simple<char>>(10)
            .map(|s: String| Expr::Num(s.parse().unwrap()))
            .padded();

        let arrayindex = text::ident::<_,Simple<char>>()
            .then_ignore(just('['))
            .then(expr.clone())
            .then_ignore(just(']'))
            .map(|(name, index)| Expr::ArrayIndex{name,index:Box::new(index)})
            .padded(); 

        let enumvariant = text::ident::<_,Simple<char>>()
            .then_ignore(just("::"))
            .then(text::ident())
            .map(|(name,variant)| Expr::EnumVariant{name,variant})
            .padded();

        let fncall = text::ident()
            .then_ignore(just('('))
            .then(expr.clone().separated_by(just(','))) //list of expressions + list slices
            .then_ignore(just(')'))
            .map(|(name, args)| Expr::FnCall{name,args})
            .padded();

        let variable = text::ident()
            .map(|s:String| Expr::Variable(s))
            .padded();

        let parens = just('(')
            .ignore_then(expr.clone())
            .then_ignore(just(')'))
            //.map(|e| e)
            .padded();


        

        let unary = op("!")
            .or(op("~"))
            .or(op("-"))
            .repeated()
            .then(expr.clone())
            .foldr(|op, rhs| Expr::Unary{op:op.to_string(),rhs:Box::new(rhs)})
            .padded();


        //let operator = todo!();

        unary.or(parens).or(variable).or(fncall).padded()
    });

    expr.then_ignore(end())
}

// fn parser() -> impl Parser<char, Vec<CodeBlock>, Error=Simple<char>> {

//     let int = text::int(10)
//         .map(|s: String| Expr::Num(s.parse().unwrap()))
//         .padded();



//     recursive(|code| {
//         let digits = text::digits(10).to_slice();
//         let number = just('-')
//                     .or_not()
//                     .then();
//         let identifier = todo!(); //[A-Za-z][A-Za-z_0-9]* not part of "if else "
//         let 
//     })
// }