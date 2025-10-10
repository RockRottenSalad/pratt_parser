# WIP implementation of pratt parser

This project implements a simple pratt parser which produces an AST
that can be evaluated.

The plan is to turn this into a simple interpreted (maybe functional) language.

## Using the program

In not-so-good repl mode. There is no support for arrow keys. Exit with Ctrl-C.
```
cargo run -- cli
```

The recommended way is to make a txt file with all your expressions as seen in
the `demo_files` directory and provide the relative path to the file like so.

```
cargo run -- demo_files/example.txt
```

## Notes about latest commits

Scope now exists, see `demo_files/scope_example.txt` for an example.

The '!' negation operator does not exist yet, use '-' for negating
booleans.

Using numeric operators with booleans will implicitly typecast the boolean to
an integer or real depending on the expression. Note that false == 0 and true
== 1 for integers. For reals, any value greater than 0 is treated as true.


In expressions that evaluate to a numeric results, numbers which are > 0 will
be considered true in the ternary operator and if statement. Although it may
not seem like it, internally booleans and numerics are in fact represented as
bools, ints or floats.


## The supported language thus far

Variables must have values. The 'let' keyword must be used everytime, even when
changing pre-existing variable, the old value is simply overriden.

You do not have to type 'print' whilst in REPL mode, simply running the
expression will implicity print it.

```
Statement -> Statement*
Statement -> PrintStatement
Statement -> DeclareVarStatement
Statement -> IfStatement
Statement -> BlockStatement

PrintStatement -> 'print' Expr
DeclareVarStatement -> 'let' Identifier '=' Expr 
IfStatement -> 'if' Expr Statement ('else' Statement)?
BlockStatement -> '{' Statement '}'

Identifier -> ['a'-'z'|'A'-'Z']+

Expr -> Expr ['+'|'-'|'*'|'/'|'=='|'!='|'<'|'>'|'<='|'>='] Expr
Expr -> '(' Expr ')'
Expr -> Literal
Expr -> Expr '?' Expr ':' Expr

Literal -> Boolean
Literal -> Number

Boolean -> ['true'|'false']

Number -> Integer
Number -> Real

Integer -> ['+'|'-']*['0'-'9']+
Real -> ['+'|'-']*['0'-'9']+'.'['0'-'9']*
```

