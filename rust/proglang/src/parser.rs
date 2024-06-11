

use chumsky::prelude::*;

use crate::operators::{BinaryOperator as BOp,UnaryOperator as UOp};

//use separated_by_save::separated_by_save;


<<<<<<< HEAD
#[derive(Clone, Debug)]
pub enum ProgramStatement { //limited code allowed in program
    /// Variable declaration
    /// Typeless:   var {ident};          || var {ident}, {ident}, ...;
    /// Typed:      var {ident}: {type};  || var {ident}, {ident}, ... :{type};
    VarDef(Vec<String>,Option<String>), 
    
    ArrayDef(Vec<(String,Expr)>), // Can this be done with VarDef too?
    
    ConstDef(String,Expr),
    EnumDef(String,Vec<(String,Option<i32>)>),
    FnDef(String,Vec<FnParam>,Vec<Statement>),
}
=======
>>>>>>> 56fbececa6bc65de5a73274eaa007ce3a8c88c54

#[derive(Clone,Debug)]
pub enum FnParam {
    Value(String), // {ident} I assume it can only handle identifiers for now but but Value could be an Expr
    Reference(String), // &{ident} 
    Array(String),
}

#[derive(Clone, Debug)]
pub enum Statement {
    VarAssign{name:String,value:Expr}, // {ident} = {expr};
    ArrayAssign{name:String,index:Expr,value:Expr}, //x[...]=...;

    IfElse{condition:Expr,yes:Box<Self>,no:Box<Self>}, // if ... else ...
    If{condition:Expr,code:Box<Self>}, // if ...
    While{condition:Expr,code:Box<Self>}, // while (...) ...
    Loop(Box<Self>), // loop ...
    Break, // break;
    Continue, // continue;
    Return(Option<Expr>),

    VarDef{vars:Vec<String>,r#type:Option<String>}, // var ...* [:...];
    ArrayDef{arrays:Vec<(String,Expr)>,r#type:Option<String>}, // array ...[...]* [:...];
    FnDef{name:String,params:Vec<FnParam>,code:Vec<Statement>}, // fn ...(...,...) {...}
    ConstDef{name:String,value:Expr},
    EnumDef{name:String,variants:Vec<(String,Option<Expr>)>},

    Expr(Expr), //just an expression not assigned to a variable (mostly function calls)
    CodeBlock(Vec<Self>), // {...*}
}




#[derive(Clone, Debug)]
pub enum Expr {
<<<<<<< HEAD
    ArrayIndex{name:String,index:Box<Self>}, // {identifer | array expr}[{expr}]
    Op{op:String,lhs:Box<Self>,rhs:Box<Self>}, // {expr} {op} {expr}
    Unary{op:String,rhs:Box<Self>}, // {op} {expr}
    EnumVariant{name:String,variant:String}, // {expr}::{identifer}
    FnCall{name:String,args:Vec<Expr>}, // {identfier}({expr},*)
    Num(i32), // -1, 0, 1, ...
    Variable(String), // ... <-- ????
    // Parens(Box<Self>), // ({expr})
=======
    ArrayIndex{name:String,index:Box<Self>}, // ...[...]
    Op{op:BOp,lhs:Box<Self>,rhs:Box<Self>}, // ... ... ...
    Unary{op:UOp,rhs:Box<Self>}, // ... ...
    EnumVariant{name:String,variant:String}, // ...::...
    FnCall{name:String,args:Vec<Expr>}, // ...(...)
    Num(i32), // ...
    Variable(String), // ...
    // Parens(Box<Self>), // (...)
>>>>>>> 56fbececa6bc65de5a73274eaa007ce3a8c88c54
}



pub fn parser() -> impl Parser<char, Vec<Statement>, Error=Simple<char>> {
    let statement: Recursive<_,Statement,_> = recursive (|statement: Recursive<_,Statement,_>| {
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
            

            let unary = op("!").to(UOp::Not)
                .or(op("~").to(UOp::BitNot))
                .or(op("-").to(UOp::Minus))
                .repeated()
                .then(atomic.clone())
                .foldr(|op, rhs| Expr::Unary{op,rhs:Box::new(rhs)})
                .padded();

            // let product = unary.clone()
            //     .then(
            //         op("*").or(op("/").or(op("%")))
            //         .then(unary)
            //         .repeated())
            //     .foldl(|lhs, (op, rhs)| Expr::Op{op:op.to_string(),lhs:Box::new(lhs), rhs:Box::new(rhs)});

            let op_fold = |(items,ops):(Vec<Expr>,Vec<BOp>)| {
                let mut items = items.into_iter();
                let mut e:Expr = items.next().unwrap();
                for i in 0..ops.len() {
                    e=Expr::Op{op:ops[i], lhs:Box::new(e), rhs:Box::new(items.next().unwrap())};
                }
                e
            };

            let product = unary
                .separated_by_save(op("*").to(BOp::Mul).or(op("/").to(BOp::Div)).or(op("%").to(BOp::Mod)))
                .at_least(1)
                .map(op_fold);

            let sum = product
                .separated_by_save(op("+").to(BOp::Add).or(op("-").to(BOp::Sub)))
                .at_least(1)
                .map(op_fold);

            let shift = sum
                .separated_by_save(op("<<").to(BOp::LShift).or(op(">>>").to(BOp::LRShift)).or(op(">>").to(BOp::ARShift)))
                .at_least(1)
                .map(op_fold);

            let bitprod = shift
                .separated_by_save(op("&").to(BOp::BitAnd).or(op("^").to(BOp::BitXor)))
                .at_least(1)
                .map(op_fold);

            let bitor = bitprod
                .separated_by_save(op("|").to(BOp::BitOr))
                .at_least(1)
                .map(op_fold);

            let comp = bitor
                .separated_by_save(
                        op("==").to(BOp::Eq)
                    .or(op("!=").to(BOp::Neq))
                    .or(op(">" ).to(BOp::Gt))
                    .or(op("<" ).to(BOp::Lt))
                    .or(op(">=").to(BOp::Ge))
                    .or(op("<=").to(BOp::Le))
                )
                .at_least(1)
                .map(op_fold);

            let logprod = comp
                .separated_by_save(op("&&").to(BOp::And).or(op("^^").to(BOp::Xor)))
                .at_least(1)
                .map(op_fold);

            let logor = logprod
                .separated_by_save(op("||").to(BOp::Or))
                .at_least(1)
                .map(op_fold);

            logor
        });


        let vardef = text::keyword("var").ignore_then(text::whitespace())
            .ignore_then(text::whitespace())
            .ignore_then(text::ident().padded().separated_by(just(',')).at_least(1))
            .then(just(':').ignore_then(text::ident()).or_not())
            .then_ignore(just(';'))
            .map(|(vars,r#type)| Statement::VarDef{vars,r#type})
            .padded();

        let arraydef = text::keyword("array").ignore_then(text::whitespace())
            .ignore_then(text::whitespace())
            .ignore_then(
                text::ident()
                .then_ignore(just('['))
                .then(expr.clone())
                .then_ignore(just(']'))
                .padded()
                .separated_by(just(',')).at_least(1))
            .then(just(':').ignore_then(text::ident()).or_not())
            .then_ignore(just(';'))
            .map(|(arrays,r#type)| Statement::ArrayDef{arrays,r#type})
            .padded();

        

        let codeblock = statement.clone()
            .repeated()
            .delimited_by(just('{'),just('}'))
            .map(|statements| Statement::CodeBlock(statements))
            .padded();


        let fnparam = text::ident().then_ignore(just("[]")).map(|s| FnParam::Array(s)) //array
            .or(just('&').ignore_then(text::ident()).map(FnParam::Reference as fn(_)->_)) //var by ref
            .or(text::ident().map(|s|FnParam::Value(s)))
            .padded(); //var

        let fndef = text::keyword("fn").ignore_then(text::whitespace())
            .ignore_then(text::ident())
            .then(
                fnparam.separated_by(just(','))
                .delimited_by(just('('),just(')'))
                .padded()
            )
            .then(statement.clone().repeated().delimited_by(just('{'),just('}')))
            .map(|((name,params),code)| Statement::FnDef{name,params,code})
            .padded();

        let constdef = text::keyword("const").ignore_then(text::whitespace())
            .ignore_then(text::ident())
            .then_ignore(just('=').padded())
            .then(expr.clone())
            .map(|(name,value)| Statement::ConstDef{name,value})
            .then_ignore(just(';'))
            .padded();

        let enumdef = text::keyword("const").ignore_then(text::whitespace())
            .ignore_then(text::ident())
            .then_ignore(just('=').padded())
            .then(
                text::ident().padded()
                .then(just('=').ignore_then(expr.clone()).or_not())
                .separated_by(just(','))
                .delimited_by(just('{'),just('}'))
            )
            .map(|(name,variants)| Statement::EnumDef{name,variants})
            .then_ignore(just(';'))
            .padded();



        let r#if = text::keyword("if").ignore_then(text::whitespace())
            .ignore_then(expr.clone().delimited_by(just('('),just(')')))
            .then(statement.clone())
            .padded();

        let ifelse = r#if.clone()
            .then_ignore(text::keyword("else"))
            .then(statement.clone())
            .map(|((condition,yes),no)| Statement::IfElse{condition,yes:Box::new(yes),no:Box::new(no)})
            .padded();

        let r#if = r#if.map(|(condition,code)| Statement::If{condition,code:Box::new(code)});
        
        let r#while =  text::keyword("while").ignore_then(text::whitespace())
            .ignore_then(expr.clone().delimited_by(just('('),just(')')))
            .then(statement.clone())
            .map(|(condition,code)| Statement::While{condition,code:Box::new(code)})
            .padded();

        let r#loop = text::keyword("loop").ignore_then(text::whitespace())
            .ignore_then(statement.clone())
            .map(|code| Statement::Loop(Box::new(code)))
            .padded();

        let r#break = text::keyword("break").ignore_then(text::whitespace::<_,Simple<char>>())
            .ignore_then(just(';')).map(|_| Statement::Break).padded();

        let r#continue = text::keyword("continue").ignore_then(text::whitespace::<_,Simple<char>>())
            .ignore_then(just(';')).map(|_| Statement::Continue).padded();

        let r#return = text::keyword("return").ignore_then(text::whitespace())
            .ignore_then(expr.clone().or_not())
            .then_ignore(just(';'))
            .map(Statement::Return as fn(_)->_)
            .padded();




        let varassign = text::ident().padded()
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .map(|(name,value)| Statement::VarAssign{name,value})
            .padded();

        let arrayassign = text::ident()
            .then(
                expr.clone()
                .delimited_by(just('['),just(']'))
                .padded()
            )
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .map(|((name,index),value)|Statement::ArrayAssign{name,index,value})
            .padded();


                vardef
            .or(arraydef)
            .or(fndef)
            .or(constdef)
            .or(enumdef)

            .or(varassign)
            .or(arrayassign)

            .or(ifelse)
            .or(r#if)
            .or(r#while)
            .or(r#loop)
            .or(r#break)
            .or(r#continue)
            .or(r#return)

            .or(codeblock)
            .or(expr.then_ignore(just(';')).map(|e| Statement::Expr(e)).padded())
    });

    statement.repeated().then_ignore(end())
}
