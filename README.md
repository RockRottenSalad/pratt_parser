# WIP implementation of pratt parser

This project implements a simple pratt parser which produces an AST
that can be evaluated.

The plan is to turn this into a simple interpreted (maybe functional) language.

## Using the program

In not-so-good repl mode. There is no support for arrow keys. Exit with Ctrl-C.
You do not have to type 'print' whilst in REPL mode, simply running the
expression will implicity print it.
```
cargo run -- cli
```

The recommended way is to make a txt file with all your expressions as seen in
the `demo_files` directory and provide the relative path to the file like so.

```
cargo run -- demo_files/example.txt
```

## Notes about latest commits

Added support for single line comments using the '#' symbol.

Scope now exists, see `demo_files/scope_example.txt` for an example.
Simple expression functions now exists, see `demo_files/func.txt` for an example.

Note that the body of a function can currently only consist of an expression.
The current implementation is somewhat hacky and relies on the use of the
"unsafe" keyword in five places. Once that is fixed, adding support for statements
in the body of a function will be the next goal.

The '!' negation operator does not exist yet, use '-' for negating
booleans.

## The supported language thus far

The following syntax indicates that the production rule must follow a certain path.
```
(Prod -> Result) 'type specific operator' (Prod -> Result)
```

Variables must have values. The 'let' keyword must be used everytime, even when
changing pre-existing variable, the old value is simply overriden.

```
Statement -> Statement*
Statement -> PrintStatement
Statement -> DeclareVarStatement
Statement -> DeclareFuncStatement
Statement -> IfStatement
Statement -> BlockStatement

PrintStatement -> 'print' Expr
DeclareVarStatement -> 'let' Identifier '=' Expr 
DeclareFuncStatement -> 'let' Identifier '=' 'fn' '(' (Identifier Type(,)?)* ')' '->' Expr
IfStatement -> 'if' Expr Statement ('else' Statement)?
BlockStatement -> '{' Statement '}'

Identifier -> ['a'-'z'|'A'-'Z']+

Expr -> Expr ['+'|'-'|'*'|'/'|'=='|'!='|'<'|'>'|'<='|'>='] Expr
Expr -> Expr 'as' Type
Expr -> (Expr -> Boolean) ['and'|'or'] (Expr -> Boolean)
Expr -> Literal
Expr -> Expr '?' Expr ':' Expr
Expr -> '(' Expr ')'

Literal -> Boolean
Literal -> Number

Boolean -> ['true'|'false']

Number -> Integer
Number -> Real

Type -> ['int'|'real'|'bool']

Integer -> ['+'|'-']*['0'-'9']+
Real -> ['+'|'-']*['0'-'9']+'.'['0'-'9']*
```

