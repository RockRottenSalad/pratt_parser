# WIP implementation of pratt parser

This project implements a simple pratt parser which produces an AST
that can be evaluated.

The plan is to turn this into a simple interpreted (maybe functional) language.

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

## Notes about latest commits

Booleans, ternary operators and if statements have been recently added and are
error prone. The '!' negation operator does not exist yet, use '-' for negating
booleans.

Using numeric operators with booleans will implicitly typecast the boolean to an
integer or real depending on the expression. Note that false == 0 and true == 1.

In expressions that evaluate to a numeric results, numbers which are > 0 will
be considered true in the ternary operator and if statement. Although it may
not seem like it, internally booleans and numerics are in fact represented as
bools, ints or floats.

Note that there are no block statements, even if you use '{}', there can only
be a single statement in there. 

There is a special type known as 'Void'. An if statement which evaluates to
false and has no else clauses will always evaluate to 'Void'. This is a very
stupid placeholder result which behaves very strangely. It acts as an identity
element when used with numeric binary operators, but always evaluates to Void
when used with comparison operators. The plan is to simply abolish this type
and mandate that all paths evaluate to a tangible value.

## The supported language thus far
```
Statement -> Expr
Statement -> '{' Statement '}'
Statement -> Expr '?' Statement ':' Statement
Statement -> IfStatement

Expr -> Expr['+'|'-'|'*'|'/'|'=='|'!='|'<'|'>'|'<='|'>=']Expr
Expr -> (Expr)
Expr -> Literal

IfStatement -> 'if' Statement '{' Statement '}' ('else' '{' Statement '}')?

Literal -> Boolean
Literal -> Number
Literal -> 'Void'

Boolean -> ['true'|'false']

Number -> Integer
Number -> Real

Integer -> ['+'|'-']*[0-9]+
Real -> ['+'|'-']*[0-9]+'.'[0-9]*
```

