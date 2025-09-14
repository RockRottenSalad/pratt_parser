# WIP implementation of pratt parser

Can currently only parse the following language.
Note that Real numbers are currently experimental and are error prone

```
Expr -> Expr['+'|'-'|'*'|'/']Expr
Expr -> (Expr)
Expr -> Number

Number -> Integer
Number -> Real

Integer -> ['+'|'-']*[0-9]+
Real -> ['+'|'-']*[0-9]+'.'[0-9]*
```

The parser can produce an AST and the AST can be evaluated.

