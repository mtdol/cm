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

The modules system must also be installed:
```
# windows
cd "path to cm dir"
./install.ps1

# linux
cd "path to cm dir"
./install.sh
```

Optionally, use
```
gcc cm.c -o cm.exe
```
or `make` to compile the cm excecutable, which can make it easier to run files or the repl.

When the executable is compiled, you can use:
```
cd "path to cm dir"
./cm.exe -install
```
to automatically install the module system.

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
To run cm.exe silently (printed output only,) use:
```
./cm.exe -s "path to file"
```

There are example files in the `examples` directory, although some may not work at the moment.

## Modules
There is a file under config/ called `modules.txt`.
This file is used by cm to determine the absolute paths of modules that can be imported with the `load` keyword.

The file has this form:
```
# comment
"module_name":"absolute_module_path"
# another comment
"module2":"path_to_module2"
```

As of right now there is no package manager, so modules must be added manually to `modules.txt`.

The module installation during setup replaces or adds the `modules.txt` file and fills it
with the `std_lib` module (required for the repl.)

To import files in the language use one of the three forms:
```
load "module::path_to_file_in_module".
load "f:absolute_path_to_file".
load "relative_path_to_file".
```
For example, `std_lib/std.cm` can be loaded with:
```
# as a module
load "std_lib::std.cm".
# as an absolute path
load "f:~/code/cm/std_lib/std.cm".
# relative to working directory
load "cm/std_lib/std.cm".
```

## Testing
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
expr ^ expr | exponentiation | `2 ^ 3` | 8 | Operands must be of same type
expr % expr | modulus | `4 % 3` | 1 | Operands must be of same type
= < <= > >= !=  | equality operators | `1 = 2` | false | Operands must be of same type
and or xor | logical binary operators | `true and false` | false | Operands must be bools
! expr | not | `! true` | false | Operands must be of bools
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
```match Expr Case, Case -> Expr1 Yields Expr2 Case \| Expr1 Yields Expr2 end``` | match expression | `match 3,5.1 \| int a, int b -> a - b \| int a, float b -> a + int b end` | 8 | variables and guards can be used in a match expr
```match expr1 when expr2 -> expr3 ``` | match conditional | `match 3,5.1 \| a, b when int? b -> a - b \| a, b -> a + int b end` | 8 | `when` adds an extra condition to the match
_ | wildcard in match | `match 5 \| float a -> "is float" \| _ -> "not a float" end` | "not a float"
def var = Expr | global binding of var | `def x = 1 + 3` | 4 | def will return the value of var, in addition to binding it
def guard var = Expr | guarded binding of var | `def int x = 3.5` | type exception | var is only guarded once and can be rewritten as float later
def dynamic var = Expr | dynamic binding of var | `def dynamic x = 3.5` | binds x to 3.5 | dynamic accepts all bindings and is implied when not present
let var = Expr in Expr | local binding of var | `let x = 3 in x + 1` | 4 
let guard var = Expr in Expr | guarded local binding of var | `let int x = not true in x + 1` | type exception
lambda var = Expr | lambda expression | `lam x = x + 1` | function | `lambda` can  be shortened to `lam`
lambda var1, var2,... = Expr | multiple lambda expression | `lam x, y = x + y` | function | equivalent to lam x = lam y = ...
Expr1 : Expr2, where Expr2 -> Function | function application | `3 : lam x = x + 1` | 4 | also written as `apply`
lambda guard var = Expr | guarded lambda expression | `3 : lam float x = x + 1.0` | type exception
def var1 = lambda var2 = ... | global mapping to function | `def add1 = lam n = n + 1` | Function | add1 can be applied at any time
typedef label = list | struct definition | `typedef S = a,b;` | instantiates struct type schema | allows you to create structs of the given type
struct label list | struct | `match struct S 3,5; \| struct S a,b; -> a + b end` | 8 
struct label | struct guard | `struct S2 4,5; : lam struct S x = print x` | type exception | struct S and struct S2 have differing labels
== | strong equality | `struct S (struct S2 3,4;); == struct S (struct S2 3,4;);` | true | == works for all language objects and yields true if all their sub-components equal
!== | strong inequality | `struct S (struct S2 5,4;); !== struct S (struct S2 3,4;);` | true
struct? label | struct type question | `struct? S2 (struct S2 4,5;)` | true
appl func list | list to function applier | `appl (lam x, y = x + y) 4,5;` | 9 | applies each element to the result of the previous function application. Equivalent to `5:4:(lam x,y = x + y)`
Expr comma Expr | execute first expr and ignore result, then run second and yield its value | `(def x = 4) comma 7` | 7 | x is still bound to 4 in the global scope (a side effect)

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
    | struct Leaf null -> 1 
    | struct Bn _, left, right; ->
        1 + appl max (left : bn_height), (right : bn_height);
    end.

# prints 2
@ b1 : bn_height
# prints 3
@ b2 : bn_height
```
