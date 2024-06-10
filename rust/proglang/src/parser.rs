

use chumsky::prelude::*;
//use separated_by_save::separated_by_save;

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

        let atomic = parens
            .or(fncall)
            .or(enumvariant)
            .or(arrayindex)
            .or(variable)
            .or(int);
        

        let unary = op("!")
            .or(op("~"))
            .or(op("-"))
            .repeated()
            .then(atomic.clone())
            .foldr(|op, rhs| Expr::Unary{op:op.to_string(),rhs:Box::new(rhs)})
            .padded();

        // let product = unary.clone()
        //     .then(
        //         op("*").or(op("/").or(op("%")))
        //         .then(unary)
        //         .repeated())
        //     .foldl(|lhs, (op, rhs)| Expr::Op{op:op.to_string(),lhs:Box::new(lhs), rhs:Box::new(rhs)});

        let op_fold = |(items,ops):(Vec<Expr>,Vec<&str>)| {
            let mut items = items.into_iter();
            let mut e:Expr = items.next().unwrap();
            for i in 0..ops.len() {
                e=Expr::Op{op:ops[i].to_string(), lhs:Box::new(e), rhs:Box::new(items.next().unwrap())};
            }
            e
        };

        let product = unary
            .separated_by_save(op("*").or(op("/")).or(op("%")))
            .at_least(1)
            .map(op_fold);

        let sum = product
            .separated_by_save(op("+").or(op("-")))
            .at_least(1)
            .map(op_fold);

        let shift = sum
            .separated_by_save(op("<<").or(op(">>")))
            .at_least(1)
            .map(op_fold);

        let bitprod = shift
            .separated_by_save(op("&").or(op("^")))
            .at_least(1)
            .map(op_fold);

        let bitor = bitprod
            .separated_by_save(op("|"))
            .at_least(1)
            .map(op_fold);

        let comp = bitor
            .separated_by_save(op("==").or(op("!=")).or(op(">")).or(op("<")).or(op(">=")).or(op("<=")))
            .at_least(1)
            .map(op_fold);

        let logprod = comp
            .separated_by_save(op("&&").or(op("^^")))
            .at_least(1)
            .map(op_fold);

        let logor = logprod
            .separated_by_save(op("||"))
            .at_least(1)
            .map(op_fold);

        logor
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



// let vardef = text::keyword("var")
//     .then_ignore(text::whitespace())
//     .then(text::ident().padded().separated_by(just(',')).at_least(1))
//     .then(just(':').ignore_then(text::ident()).or_not())
//     .then_ignore(just(';'))
//     .map(|(names,r#type)| Statement::VarDef{names,r#type})
//     .padded();

// let arraydef = text::keyword("array")
//     .then_ignore(text::whitespace())
//     .then(
//         text::ident()
//         .then_ignore(just('['))
//         .then(expr)
//         .then_ignore(just(']'))
//         .padded()
//         .separated_by(just(',')).at_least(1))
//     .then(just(':').ignore_then(text::ident()).or_not())
//     .then_ignore(just(';'))
//     .map(|(arrays,r#type)| Statement::ArrayDef{arrays,r#type})
//     .padded();

// let varassign = text::ident().padded()
//     .then_ignore(just('='))
//     .then(expr)
//     .then_ignore(just(';'))
//     .map()
//     .padded();

// let arrayassign = arrayindex.clone()
//     .then_ignore(just('='))
//     .then(expr)
//     .then_ignore(just(';'))
//     .map(|((name,index),value)|Statement::ArrayAssign{name,index,value})
//     .padded();

// let codeblock = just('{')
//     .ignore_then(
//         statement.repeated()
//     )
//     .then_ignore(just('}'))
//     .map(|statements| Statement::CodeBlock(statements))
//     .padded();
