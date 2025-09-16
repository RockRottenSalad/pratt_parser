# WIP implementation of pratt parser


## Notes about latest commit
Booleans have been recently added and are error prone.
The '!' operator does not exist yet, use '-' for negating booleans.

Using numeric operators with booleans will implicity typecast the boolean to an
integer or real depending on the expression.

## The supported language
Can currently only parse the following language.
```
Expr -> Expr['+'|'-'|'*'|'/'|'=='|'!='|'<'|'>'|'<='|'>=']Expr
Expr -> (Expr)
Expr -> Literal

Literal -> Number
Literal -> Boolean

Boolean -> ['T'|'F']

Number -> Integer
Number -> Real

Integer -> ['+'|'-']*[0-9]+
Real -> ['+'|'-']*[0-9]+'.'[0-9]*
```

The parser can produce an AST and the AST can be evaluated.

No arguments parsing yet, the parsed expression is a string literal defined in
a variable inside the main function.

