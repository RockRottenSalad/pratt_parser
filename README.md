# WIP implementation of pratt parser

This project implements a simple pratt parser which produces an AST
that can be evaluated.

## Using the program

In not-so-good repl mode. There is no support for arrow keys.
```
cargo run -- cli
```

The recommended way is to make a txt file with all your expressions as seen in
`demo_files/example.txt` and provide the relative path to the file like so.

```
cargo run -- demo_files/example.txt
```

## Notes about latest commit

Booleans and ternary operators have been recently added and are error prone.
The '!' negation operator does not exist yet, use '-' for negating booleans.

Using numeric operators with booleans will implicity typecast the boolean to an
integer or real depending on the expression. note that false == 0 and true == 1.

In expressions that evaluate to a numeric results, numbers which are > 0 will
be considered true in the ternary operator.

Although it may not seem like it, internally booleans and numerics are in fact
represented as bools, ints or floats.

## The supported language
```
Statement -> Expr
Statement -> Expr '?' Statement ':' Statement

Expr -> Expr['+'|'-'|'*'|'/'|'=='|'!='|'<'|'>'|'<='|'>=']Expr
Expr -> (Expr)
Expr -> Literal

Literal -> Number
Literal -> Boolean

Boolean -> ['true'|'false']

Number -> Integer
Number -> Real

Integer -> ['+'|'-']*[0-9]+
Real -> ['+'|'-']*[0-9]+'.'[0-9]*
```

