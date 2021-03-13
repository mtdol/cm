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
let x := lam n := n + 1 in 1 : x
# Valid
let x := lam n := n + 1 in 1 : x.
```

Construct | Effect | Example | Yields | Explanation
------------ | -----|--------|--------| -----------
if expr then expr else expr | if expression | `if 1+2 = 4 then -7 else "wrong"` | "wrong"
expr + expr \| plus | addition | `1 + 3` | 4 | Operands must be of same type
expr * expr \| mult | multiplication | `2 * 3` | 6 | Operands must be of same type
expr - expr \| minus| subtraction | `4 - 6` | -2 | Operands must be of same type
expr / expr \| div | division | `4 / 2` | 2 | Operands must be floats
expr ^ expr \| exp| exponentiation | `2 ^ 3` | 8 | Operands must be of same type
expr % expr \| mod | modulus | `4 % 3` | 1 | Operands must be of same type
= < <= > >= !=  \| (eq\|equal\|equals), lt, le, gt, ge, ne | equality operators | `1 = 2` | false | Operands must be of same type
& \|\| xor \| and or xor | logical binary operators | `true and false` | false | Operands must be bools
! expr \| not | not | `! true` | false | Operands must be of bools
expr $ expr \| cat | string concatenation | "abc" cat 3 | abc3 | auto coerces operands to strings
expr, expr \| cons | cons | `1,4,(5,6)` | (1, 4, (5, 6))
\` expr \| head | head | `head (1, 2, 3)` | 1 
~ expr \| tail | tail | `~(1, 2, 3)` | 2,3
null \| () | empty list () | `1,2,3,null` | (1, 2, 3, null)
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
:= \| assign | assign operator used in certain contexts 
def var := Expr | global binding of var | `def x := 1 + 3` | 4 | def will return the value of var, in addition to binding it
def guard var := Expr | guarded binding of var | `def int x := 3.5` | contract exception | var is only guarded once and can be rewritten as float later
def dynamic var := Expr | dynamic binding of var | `def dynamic x := 3.5` | binds x to 3.5 | dynamic accepts all bindings and is implied when not present
let var := Expr in Expr | local binding of var | `let x := 3 in x + 1` | 4 
let guard var := Expr in Expr | guarded local binding of var | `let int x := not true in x + 1` | contract exception
lambda var := Expr | lambda expression | `lam x := x + 1` | function | `lambda` can  be shortened to `lam`
lambda var1, var2,... = Expr | multiple lambda expression | `lam x, y := x + y` | function | equivalent to lam x = lam y = ...
Expr1 : Expr2, where Expr2 -> Function | function application | `3 : lam x := x + 1` | 4 | also written as `apply`
lambda guard var := Expr | guarded lambda expression | `3 : lam float x := x + 1.0` | contract exception
def var1 := lambda var2 = ... | global mapping to function | `def add1 := lam n := n + 1` | Function | add1 can be applied at any time
defun var vars := ... | shorthand global mapping to function | `defun add2 (x,y) := x + y` | Function | the vars operand must be wrapped if more than one var
types string_list var | multiple types guard | `def types ("int", "float";) x := 5` | 5 | any value within the list is a valid type for var
typedef label := list | struct definition | `typedef S := a,b;` | instantiates struct type schema | allows you to create structs of the given type
struct label list | struct | `match struct S (3,5;) \| struct S (a,b;) -> a + b end` | 8 
struct label | struct guard | `struct S2 4,5; : lam struct S x := print x` | contract exception | struct S and struct S2 have differing labels
== \| eqq | strong equality | `struct S (struct S2 (3,4;);) == struct S (struct S2 (3,4;);)` | true | == works for all language objects and yields true if all their sub-components equal
!== \| neqq | strong inequality | `struct S (struct S2 (5,4;);) !== struct S (struct S2 (3,4;);)` | true
struct? label | struct type question | `struct? S2 (struct S2 4,5;)` | true
appl func list | list to function applier | `appl (lam x, y := x + y) (4,5;)` | 9 | applies each element to the result of the previous function application. Equivalent to `5:4:(lam x,y := x + y)`
Expr comma Expr | execute first expr and ignore result, then run second and yield its value | `(def x := 4) comma 7` | 7 | x is still bound to 4 in the global scope (a side effect)

## Basics and Assignment
`def` is used to create global variables.
```
> def x := 3.
> x.
3
```

Accessing variables before they have been defined yields an error:
```
> x.
1:UNDEFINED: Var x has not yet been defined.
```

Local bindings can be created with the `let` keyword.
```
> let x := 5 in x + 3.
8
```

Local bindings replace global bindings in local scope:
```
> def z := 7.
7
> let z := 4 in z.
4
> z.
7
```

Bindings can be guarded, but will only be guarded for their first assignment
```
> def int x := 3.0.
1:CONTRACT: Recieved type float for var x but expected int.
> def int x := 3.
> x.
3
> def x := "hello".
> x.
hello
```

The guards are `int` `bool` `string` `float` `fun` `pair` `list`
Additionally there are type questions available:
`int?` `bool?` `string?` `float?` `fun?` `pair?` `list?` `null?` `void?`
```
> int? 4.8.
false
> list? null.
true
```

## cond
`cond` and `match` both make use of the `case` keyword (also written `|`.)

`cond` is used to provide a generalized `if` expression with an arbitrary number of conditions:
```
> cond case false -> "wrong" case 3 = 4 -> "also wrong" else "I guess I will have to do.".
I guess I will have to do.
```
Every cond expr must end in an else clause that will be run if all other cases turn out to be false.

The `cond` keyword is technically optional
```
def odd? := lam int n :=
| n % 2 = 0 -> false
else true.

# prints "true"
@ 3:odd?.
```
## match

`match` is designed to pattern match results of computations, namely structs, values, and pairs.
`match` uses cases in its body and is terminated with the `end` keyword:
```
def get_str := lam int i :=
    match i
    | 0 -> "zero"
    | 1 -> "one"
    | 2 -> "two"
    | _ -> "other number"
    end.
```

The `_` symbol is used as a wildcard.
Variables can also be used with or without guards:
```
> match 3,4; | float a, float b; -> a + b | int a, int b; -> a - b end.
-1
```

Finally the `when` keyword can be used to add extra conditions to a match case
```
> match 5 | int a when a:pos? -> -a | int a when not (a:pos?) -> 2*a end.
-5
> match -5 | int a when a:pos? -> -a | int a when not (a:pos?) -> 2*a end.
-10
```

## structs
The type of a structt must first be created using the `typedef` keyword.
```
typedef label := list
```

Struct instances can then be created using the `struct` keyword.
```
typedef S := a,b;.
struct S (4,5;).
```

Struct contents can be accessed using the match keyword:
```
> match struct S (4,5;) | struct S (a,b;) -> a + b end.
9
```

Structs can also contain other structs:
```
> match struct S ((struct S (3,2;)),5;) | struct S ((struct S (a,b;)),c;) -> a + b + c end.
10
```

Also you can ask if something is a certain struct using the `struct?` keyword:
```
> typedef S2 := a;.
> struct? S 5.
false
> struct? S (struct S (5,6;)).
true
> struct? S (struct S2 (5;)).
false
```

Structs can be compared with the "strong equality" operators `==` and `!==`, also known as `eqq` and `neqq`.
```
> struct S (5,6;) == struct S (5,6;).
true
> struct S2 (5;) !== struct S (5,6;).
true
```

## lambdas
Functions can be created with the `lambda` keyword, often shortened to `lam`.
```
# creates a function that adds one to its argument
lam x := x + 1.
```

Functions are applied to using the `apply` keyword, often shortened to `:`
```
# yields 3
2 apply lam x := x + 1.
```

Lambdas can be assigned to variables to be used throughout a program:
```
def add2 := lam x := x + 2.
# yields 7
5:add2.
```

Lambdas can be guarded with type notations:
```
def add2 := lam int x := x + 2.
# contract exception
5.0:add2.
```

Lambdas remember the local context of when they were created:
```
def add_to_3 := 
    let v := 3 in lam x := x + v.
    
# yields 8
5:add_to_3.
```

Lambdas can be chained together:
```
def add_both := lam x := lam y := x + y.

# yields a function
4:add_both.
# yields 6
2:4:add_both.
```

This chaining of lambdas can be simplified with `,`
```
def add_both := lam x, y := x + y.
```

The `defun` keyword can be used as a shorthand for a def assignment to a lambda.
```
defun var vars := expr
```

The vars operand must be wrapped up in parens if it is any more than a single variable.

Examples:
```
defun a1 x := x + 1.
defun a2 (x, y) := x + y.

defun float_a2 
    (types ("int", "float";) x, types ("int", "float";) y) :=
        float x + float y.
```

You can check whether something is a function using the `fun?` question and `fun` guard.
```
> fun? lam x := x.
true
> def fun_app := lam fun f, v := v:f.
> 3 : fun_app.
1:CONTRACT: Recieved type int for var f but expected fun.
> 3 : (lam x := x - 1) : fun_app.
2
```

The `appl` (apply list) keyword can be used to make function applications with multiple items more natural.
```
appl func list
```
for example:
```
> def cat3 := lam x,y,z := x$y$z.

# equivalent to "C":"B":"A":cat3
> appl cat3 ("A", "B", "C";).
ABC
```

## Types guard
The keyword `types` can be used to allow more complicated and flexible variable guards.
```
types string_list var
```

Examples:
```
> def int x := 5.5.
1:CONTRACT: Recieved type float for var x but expected one of (int;).
> def types ("int", "float";) x := 5.5.
5
> def types ("int", "float";) x := "5.5".
1:CONTRACT: Recieved type string for var x but expected one of (int, float;).

> typedef S := types ("int","bool";) a, float b;.

> struct S (true,4.5;).
(struct S (true, 4.5;))
> struct S (5.5, 4.5;).
1:GENERIC: Could not validate struct against type schema: ...
```

## Error Handling
Errors messages have the following form:
```
linenum|NL:id:(space)main message
```

for example:
```
> 5.0 / 0.
1:CONTRACT: Divide by zero.

> 6
1:PARSE: No termination of statement.

> error "my error"
NL:GENERIC: my error
```

`NL` represents the lack of a line number.
The id represents the type of error and the message body is to be used in debugging.

### Try Catch
Exceptions can be caught with the try catch syntax:
```
> try 5.0 / 0.0 catch err with 5.0 / 1.0.
5.0
```

The term after `catch` is a variable and can be changed to whatever you wish.
This var represents an error struct of the following format:
```
struct Error id,message;
```
For example to match an error:

```
# yields 2
try 
    5.0 / 0.0 
catch err with
    match err
    | struct Error ("GENERIC",msg;) -> 1
    | struct Error ("CONTRACT",msg;) -> 2
    | struct Error (_,msg;) -> 3
    end.
```

### Error keyword
The keyword `error` can be used to throw an exception.
The keyword either takes a single argument (string) or a list of two elements
where the first is the error id and the second is the base message.

Examples:
```
> error "my message".
NL:GENERIC: my message

> error "MY_TYPE","my message";.
NL:MY_TYPE: my message
```

Error IDs are written in all caps with underscore seperators.


## Code Examples
```
# Find Factorial n
def fact := lam int n :=
    | n < 2 -> 1
    else n * (n - 1 : fact).
    
# prints 24
@ 4 : fact.
######################################

# yields larger number
def max := lam int n1, int n2 :=
    if n1 > n2 then n1 else n2.

# prints 7
@ appl max (4,7;).
# still prints 7
@ appl max (7,4;).
######################################


# Finds last element of list, or errors if list is null
def get_last := lam list lst := 
    match lst 
    | () -> error "List was empty." 
    | h; -> h 
    | h, t -> t : get_last
    end.
   
# prints 7
@ 3,5,7; : get_last.

# prints 3
@ 3; : get_last.

# throws error
@ null : get_last.
######################################


# creates binary tree node schema
typedef Bn := 
    dynamic val,
    types ("struct Bn","struct Leaf";) left,
    types ("struct Bn","struct Leaf";) right;.
    
# creates leaf schema (notice no args to type constructor)
typedef Leaf := null.

# simple binary node (parens are necessary around sub struct constructors for syntax reasons)
def b1 := struct Bn (5, (struct Leaf null), (struct Leaf null);).
# another with a deeper subtree
def b2 := 
    struct Bn 
        (5,
        (struct Bn 
            (3,
            (struct Leaf null),
            (struct Leaf null);)),
        (struct Leaf null);).

# yields the height of a binary tree
def bn_height := lam types ("struct Bn", "struct Leaf";) n := 
    match n
    | struct Leaf null -> 1 
    | struct Bn (_, left, right;) ->
        1 + appl max ((left : bn_height), (right : bn_height);)
    end.

# prints 2
@ b1 : bn_height.
# prints 3
@ b2 : bn_height.
# prints 1
@ struct Leaf null : bn_height.
```
