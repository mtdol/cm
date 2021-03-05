# cm
Implementation of the cm language.

Development in progress.

## Installation
[Racket is required to run cm.](https://racket-lang.org/)

Use the following to install cm as a package:
```
cd "path to cm dir"
raco pkg install
```

Optionally, use
```
gcc cm.c -o cm.exe
```
To compile the cm excecutable, which can make it easier to run files or the repl.

## Repl
When build is stable, use:
```
racket ./repl.rkt
```
in language directory (core) to test language expressions.

Also you can simply run cm.exe to load the repl.
```
cd "path to cm directory"
./cm.exe
```

## Running Files
The cm.rkt file in the core directory contains racket functions to interpret/parse/tokenize  
cm files/expressions and statements.

### Examples
```
racket -e '(require cm/core/cm) (cm-run "language statement")'
racket -e '(require cm/core/cm) (cm-run-file "file here")'
racket -e '(require cm/core/cm) (cm-run-file-silent "language statement")'
racket -e '(require cm/core/cm) (cm-parse "statement here")'
racket -e '(require cm/core/cm) (cm-parse-expr "expression here")'
```
Aditionally, cm.exe can be used to run cm files:
```
./cm.exe "path to file"
```
There are example files in the `examples` directory, although some may not work at the moment.

### Testing
```
cd "path to cm dir"
raco test test/*
```

## Language form
Substitute expr for any expression.

All statements must end in the "dot" operator:
```
# Invalid
let x = lam n = n + 1 in 1 : n
# Valid
let x = lam n = n + 1 in 1 : n.
```

Construct | Effect | Example | Yields | Explanation
------------ | -----|--------|--------| -----------
if expr then expr else expr | if expression | `if 1+2 = 4 then -7 else "wrong"` | "wrong"
expr + expr | addition | 1 + 3 | 4 | Operands must be of same type
expr * expr | multiplication | 2 * 3 | 6 | Operands must be of same type
expr - expr | subtraction | 4 - 6 | -2 | Operands must be of same type
expr / expr | division | 4 / 2 | 2 | Operands must be floats
expr & expr | and | 3 & false | 0 | Operands must be of same type
expr ^ expr | exponentiation | 2 ^ 3 | 8 | Operands must be of same type
expr % expr | modulus | 4 % 3 | 1 | Operands must be of same type
! expr | not | ! true | 0 | Operands must be of bools
expr, expr | cons | 1,4,(5,6) | (1 4 . ( 5 . 6))
` expr | head | `(1, 2, 3) | 1 
~ expr | tail | ~(1, 2, 3) | 2,3
null | empty list () | 1,2,3,null | (1 2 3)
; | cons null | 1,2,3; | (1 2 3)
[] | square parens | \[1 + 2\] | 3 | same as ()
{} | curly parens | {1 + 2} | 3 | same as ()
print expr | print | print 1 + 2 | 3 
@ expr | print | @ 1 + 2 | 3 | alias of print
\# | comment | 4 + 1 + 2 # 5 + 6 | 7
int expr | int coercion | int 7.9 | 7
int? expr | is int? | int? 6 | 1
```cond case, case = "\|" Expr1 "->" Expr2 case \| "\|" Expr1 "->" Expr2 Else Expr3``` | cond expression | cond \| false -> 3 \| 3 -> 5 else 7 | 5 | the "cond" marker is actually optional.
def var = Expr | global binding of var | def x = 1 + 3 | 4 | def will return the value of var, in addition to binding it
def guard var = Expr | guarded binding of var | def int x = 3.5 | type exception | var is only guarded once and can be rewritten as float later
let var = Expr in Expr | local binding of var | let x = 3 in x + 1 | 4 
let guard var = Expr in Expr | guarded local binding of var | let int x = not true in x + 1 | type exception
lambda var = Expr | lambda expression | lam x = x + 1 | function | `lambda` can  be shortened to `lam`
lambda var1, var2,... = Expr | multiple lambda expression | lam x, y = x + y | function | equivalent to lam x = lam y = ...
Expr1 : Expr2, where Expr2 -> Function | guarded lambda expression | 3 : lam x = x + 1 | 4 | also written as `apply`
lambda guard var = Expr | guarded lambda expression | 3 : lam float x = x + 1.0 | exception
def var1 = lambda var2 = ... | global mapping to function | def add1 = lam n = n + 1 | Function | add1 can be applied at any time

### Examples
```
# Find Factorial n
def fact = lam int n =
    | n < 2 -> 1
    else n * (n - 1 : fact).
    
# prints 24
@ 4 : fact
```
