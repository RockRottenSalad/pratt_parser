# WIP implementation of pratt parser

Can currently only parse the following language with absolutely no whitespace anywhere in the input

```
expr -> Integer['+'|'-']expr
expr -> Integer
```

