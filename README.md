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
`demo_files/example.txt` and provide the relative path to the file like so.

```
cargo run -- demo_files/example.txt
```

## Notes about latest commits

Booleans, ternary operators, if statements and variables have been recently
added and are error prone.

The '!' negation operator does not exist yet, use '-' for negating
booleans.

Using numeric operators with booleans will implicitly typecast the boolean to an
integer or real depending on the expression. Note that false == 0 and true == 1.

In expressions that evaluate to a numeric results, numbers which are > 0 will
be considered true in the ternary operator and if statement. Although it may
not seem like it, internally booleans and numerics are in fact represented as
bools, ints or floats.


## The supported language thus far

Note that the if statement is actually not a proper if statement, but is
currently implemented as a ternary expression. This will be changed.

Variables must have values. The 'let' keyword must be used everytime, even when
changing pre-existing variable, the old value is simply overriden.
The concept of scope does not exist yet. All variables are global.

You do not have to use 'print' whilst in REPL mode.

```
Statement -> Statement*
Statement -> 'let' Identifier '=' Expr 
Statement -> print '{' Expr '}' # Statements in blocks coming soon
Statement -> print Expr

Identifier -> ['a'-'z'|'A'-'Z']+

Expr -> Expr['+'|'-'|'*'|'/'|'=='|'!='|'<'|'>'|'<='|'>=']Expr
Expr -> (Expr)
Expr -> Literal
Expr -> IfStatement # Not a real statement, just a placeholder
Expr -> Expr '?' Expr ':' Expr

IfStatement -> 'if' Expr '{' Expr '}' 'else' '{' Expr '}'

Literal -> Boolean
Literal -> Number

Boolean -> ['true'|'false']

Number -> Integer
Number -> Real

Integer -> ['+'|'-']*['0'-'9']+
Real -> ['+'|'-']*['0'-'9']+'.'['0'-'9']*
```

