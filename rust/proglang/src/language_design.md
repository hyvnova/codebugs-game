

# Proglang design



// line comment
/*
block comment with /*nesting*/
*/

var x; //global variable definition
var x,y; //multiple variables
var x:type; //type annotation purely used for debug display options (either Int or some enum variant)
var x,y,z:type; //multiple variables of the same type

array x[10]; //array of size 10
array x[5],y[10]; //multiple arrays
array x[10]:type;

const X = 10;

enum Type {A, B, C}; // enums function as grouped constants and types
enum Type {A = 1, B, C}; //with numbers



fn main() irq (Event::ATTACKED => call fname, Event::DAMAGED => none, Event::IDK => return 0) {


}



fn fname(param, &param, param[]) { //function definition with params by value, reference, array (always by reference)
	var x; //local definition; var will be placed on stack
	
	// variable assignment
	x = a+b - 2*(c%4); //standard arithmetic: +-*/% and (), bits: & | ^ ~, bool: ! && || ^^ (yes we have XOR because why is this not a regular thing)
	y = 1 + min(1,s); //function calls can return at most 1 value
	z = p==q; // we have comparisons < > <= >= == !=
	
	loop { //infinite loop
		break; //skip to end
	}
	
	while (cond) { //condintion executed on every cycle
		continue; //skip to beginning
	}
	
	//maybe also add do-while contruction
	do {
	
	} while (cond);
	
	// need a type of for loop
	// for(i=0; i<10; i++) feels a bit weird, especially because the MIDDLE one is an expression and the others are statements
	// can also just be written as init; while (cond) {{} next;}
	// we can keep it out for now
	//	repeat i 1..2 { // ranges? where is the range stored? hmmmmm
	//		break;
	//	}
	
	p[0] = 5; //array index assignment
	q = p[i+1]; //array index reading
	
	x = Type::Value; //enum variants
	
	A[0..5] = B[1..6] // slice copying? maybe keep for later. do we want runtime slice indexing?
	// using slicing to turn an array(slice) into a smaller array slice during runtime might be very useful,
	// especially because array (slices) can be function parameters
	// array slices include their length
	// slice indexing would require slice syntax to be a thing but not really more than that
	// they function as a 3 parameter function (array, start, end) -> array
	// and we can fit an array in 1 int as well (16bit length, 16bit index is plenty)
	
	// logging for debug purposes - would basically function as a special function call if done with params
	// params are sth for later though, as they require special syntax
	// basically just a print function without having to add the concept of strings
	// hmmmm they might be problematic because they do not have ; at the end...? -> they are newline terminated f* it
	! Log statement with {params}?
	
	if (cond) {}
	
	} else if (cond) {
	
	} else {
	
	}

	// or just
	if (cond) statement;
	
	
	{} //random scope is fine; it is a statement that contains zero or more other statements
	
	
	return 0; //function has exactly 1 return value by default
}




# language description

var def:
`var <ident>[,<ident>]* [:<ident>];`
array def:
`array <ident>\[<const>\] [,<ident>\[<const>\]]* [:<ident>];`

function arg:
`(<ident>)|(&<ident>)|(<ident>[])`
function def:
`fn <ident> ([<function_arg>[,<function_arg>]*]) <codeblock>`

codeblock:
`{<statement>*}`
statement:
`<varassign>|<arrayassign>|<ifelse>|<if>|<while>|<loop>|<break>|<continue>|<vardef>|<arraydef>|<codeblock>`

varassign:
`<ident> = <expr>;`
arrayassign:
`<ident>\[<expr\] = <expr>;`
ifelse:
`<if> else <statement>`
if:
`if (<expr>) <statement>`
while:
`while (<expr>) <statement>`
loop:
`loop <statement>`
break:
`break;`
continue:
`continue;`

expr:
`<functioncall>`
`<expr> <op> <expr>`
`<ident>`
`<const>`

operators, left associative with this priority:
`||` or
`&& ^^` and xor
`== != >= > <= <` eq ne ge gt le lt
`<< >>` sl sr
`|` bitor
`&^` bitand bitxor
`+ -` add sub
`* / %` mul div mod
   and single:
`! - ~` not minus bitnot

functioncall:
`<ident>((<ident>|<expr>),*)`

const:
`[-]\d+`



(all are const if inputs are const)




# Some CPU design that's related


STACK:
<named variables>
<temp variables>
<return value address>
<pc INCREMENTED BEFORE EVERY EXECUTION>

multiple times, once for every function call (yes, every function call has its own PC - it functions as the return address)