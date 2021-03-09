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

_Note that currently the cm executable is having trouble running on windows._

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
let x = lam n = n + 1 in 1 : x
# Valid
let x = lam n = n + 1 in 1 : x.
```

Construct | Effect | Example | Yields | Explanation
------------ | -----|--------|--------| -----------
if expr then expr else expr | if expression | `if 1+2 = 4 then -7 else "wrong"` | "wrong"
expr + expr | addition | `1 + 3` | 4 | Operands must be of same type
expr * expr | multiplication | `2 * 3` | 6 | Operands must be of same type
expr - expr | subtraction | `4 - 6` | -2 | Operands must be of same type
expr / expr | division | `4 / 2` | 2 | Operands must be floats
expr & expr | and | `3 & false` | false | Operands must be of same type
expr ^ expr | exponentiation | `2 ^ 3` | 8 | Operands must be of same type
expr % expr | modulus | `4 % 3` | 1 | Operands must be of same type
! expr | not | `! true` | 0 | Operands must be of bools
expr, expr | cons | `1,4,(5,6)` | (1, 4, (5, 6))
\` expr | head | `head (1, 2, 3)` | 1 
~ expr | tail | `~(1, 2, 3)` | 2,3
null | empty list () | `1,2,3,null` | (1, 2, 3, null)
; | cons null | `1,2,3;` | (1, 2, 3, null)
[] | square parens | `[1 + 2]` | 3 | same as ()
{} | curly parens | `{1 + 2}` | 3 | same as ()
print expr | print | `print 1 + 2` | 3 
@ expr | print | `@ 1 + 2` | 3 | alias of print
\# | comment | `4 + 1 + 2 # 5 + 6` | 7
int expr | int coercion | `int 7.9` | 7
int? expr | is int? | `int? 6` | 1
```cond case, case = "\|" Expr1 "->" Expr2 case \| "\|" Expr1 "->" Expr2 Else Expr3``` | cond expression | `cond \| false -> 3 \| 3 -> 5 else 7` | 5 | the "cond" marker is actually optional.
```match Expr Case, Case -> Expr1 Yields Expr2 Case \| Expr1 Yields Expr2 end``` | match expression | `match 3,5.1 \| int a, int b -> a + b \| int a, float b -> a + int b end` | 8 | variables and guards can be used in a match expr
def var = Expr | global binding of var | `def x = 1 + 3` | 4 | def will return the value of var, in addition to binding it
def guard var = Expr | guarded binding of var | `def int x = 3.5` | type exception | var is only guarded once and can be rewritten as float later
let var = Expr in Expr | local binding of var | `let x = 3 in x + 1` | 4 
let guard var = Expr in Expr | guarded local binding of var | `let int x = not true in x + 1` | type exception
lambda var = Expr | lambda expression | `lam x = x + 1` | function | `lambda` can  be shortened to `lam`
lambda var1, var2,... = Expr | multiple lambda expression | `lam x, y = x + y` | function | equivalent to lam x = lam y = ...
Expr1 : Expr2, where Expr2 -> Function | guarded lambda expression | `3 : lam x = x + 1` | 4 | also written as `apply`
lambda guard var = Expr | guarded lambda expression | `3 : lam float x = x + 1.0` | type exception
def var1 = lambda var2 = ... | global mapping to function | `def add1 = lam n = n + 1` | Function | add1 can be applied at any time
typedef label = list | struct definition | `typedef S = a,b;` | instantiates struct type schema | allows you to create structs of the given type
struct label list | struct | `match struct S 3,5; \| struct S a,b; -> a + b end` | 8 
struct label | struct guard | `struct S2 4,5; : lam struct S x = print x` | type exception | struct S and struct S2 have differing labels
struct? label | struct type question | `struct? S2 (struct S2 4,5;)` | true
appl func list | list to function applier | `appl (lam x, y = x + y) 4,5;` | 9 | applies each element to the result of the previous function application. Equivalent to `5:4:(lam x,y = x + y)`

### Examples
```
# Find Factorial n
def fact = lam int n =
    | n < 2 -> 1
    else n * (n - 1 : fact).
    
# prints 24
@ 4 : fact.
######################################

# yields larger number
def max = lam int n1, int n2 =
    if n1 > n2 then n1 else n2.

# prints 7
@ appl max 4,7;.
# still prints 7
@ appl max 7,4;.
######################################


# Finds last element of list, or returns null if list is null
def get_last = lam list lst = 
    match lst 
    | () -> () 
    | h; -> h 
    | h, t -> t : get_last
    end.
   
# prints 7
@ 3,5,7; : get_last.

# prints 3
@ 3; : get_last.

# prints null
@ null : get_last.
######################################


# creates binary tree node schema
typedef Bn = val, left, right;.
# creates leaf schema (notice no args to type constructor)
typedef Leaf = null.
# simple binary node (parens are necessary around sub struct constructors for syntax reasons)
def b1 = struct Bn (5, (struct Leaf null), (struct Leaf null);).
# another with a deeper subtree
def b2 = struct Bn (5, (struct Bn 3, (struct Leaf null), (struct Leaf null);), (struct Leaf null);).

# yields the height of a binary tree
def bn_height = lam n = 
    match n
    | struct Leaf null -> 0 
    | struct Bn _, left, right; ->
        1 + appl max (left : bn_height), (right : bn_height);
    end.

# prints 1
@ b1 : bn_height
# prints 2
@ b2 : bn_height
```
