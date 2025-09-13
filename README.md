# WIP implementation of pratt parser

Can currently only parse the following language.

```
Expr -> (Expr)
Expr -> Integer['+'|'-'|'*'|'/']Expr
Expr -> Integer
Integer -> ['+'|'-']*[0-9]+
```

The parser can produce an AST and the AST can be evaluated.

