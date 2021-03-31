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
The main.rkt file in the core directory contains racket functions to interpret/parse/tokenize  
cm files/expressions and statements.

`display-output` is used to print the results of a statement interp.
`display-expr-output` is used to print the results of a expression interp.
`silent` will return void after running the given proc
`displayln` (racket proc) can be used to print any output in its unaltered form

### Examples
```
racket -e '(require cm/core/main) (run "language statement")'
racket -e '(require cm/core/main) (run-file "file here")'
racket -e '(require cm/core/main) (display-output (run "language statement"))'
racket -e '(require cm/core/main) (display-expr-output (run-expr "language statement"))'
racket -e '(require cm/core/main) (silent (run-file "language statement"))'
racket -e '(require cm/core/main) (displayln (run-parse "statement here"))'
racket -e '(require cm/core/main) (run-parse-expr "expression here")'
...
```
Aditionally, cm.exe can be used to run cm files:
```
./cm.exe "path to file"
```
The -f switch is normally implied and indicates a file arg.
```
# -f is not really necessary
./cm.exe -f "path to file"
```

To learn further about how to use cm.exe, use the `--help` or `-h` switch to display help text.
```
./cm.exe --help
```

There are example files in the `examples` directory, although some may not always work as the language changes.

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

### import

To import files for use in your program use the `import` macro with this form:
```
-- with no prefix
#:import "file1", "file2", "file3"

-- with a prefix of "f_"
#:import:f_ "file1", "file2", "file3"
```

The strings used on the right side of the `import` macro take one of these three forms:
```
"module::path_to_file_in_module"

"path_to_file"

-- same as without the `m:`, but explicit
"m:module::path_to_file_in_module"

-- same as without `f:`, but explicit
"f:path_to_file"
```
For example, `std_lib/std.cm` can be loaded with:
```
-- as a module
#:import "std_lib::std.cm"

-- as an absolute path
#:import "~/code/cm/std_lib/std.cm"

-- relative to working directory
#:import "cm/std_lib/std.cm"

-- with an "std_" prefix
#:import:std_ "std_lib::std"
```

### lazy_import

To avoid cycles in the import graph, there is an additional `lazy_import` macro. `lazy_import` takes items that have
already been imported into the global context and provides references to them in the current module space.

`lazy_import` is used with the following syntax:
```
-- no prefix
#:lazy_import "file" -> var var_name, type type_name, macro macro_name

-- prefix of "pref_"
#:lazy_import:pref_ "file" -> var var_name, type type_name, macro macro_name
```
The element to the left of the `->` must be a file name string and will be the file that the items will be provided from.   
The elements to the right will be a comma seperated list of `var`, `type`, or `macro` followed by the name
of what you wish to import.

`,` and `->` must be escaped by enclosing the affected name in quotes. Besides escaping, quotes are optional.
```
-- yes
#:lazy_import "file.cm" -> macro "a,b"

-- no
#:lazy_import "file.cm" -> macro a,b

-- fine, since macro is simply named
#:lazy_import "file.cm" -> macro ab
```

An example, using `std.cm`:
```
#:lazy_import:std_ "std_lib::std.cm" -> var add1, var pos?, macro "%"

> std_add1:2
3
> {std_% @1|@2}
1
2
```

### #:lang cm
The macro `#:lang cm` can be used (usually at the top of a file)
to load all of the standard modules that are recommended for the language.

```
#:lang cm

-- a function included in std.cm
@ add1:4.
```
The macro imports various files from the `std_lib` folder.

## Testing
```
cd "path to cm dir"
raco test test/*
```

## Language form
Substitute expr for any expression.

All statements must end in the "dot" operator:
Additionally `//` can be used as a more visible version of dot.
```
-- Invalid
let x := lam n := n + 1 in x:1
-- Valid
let x := lam n := n + 1 in x:1.
-- Valid
let x := lam n := n + 1 in x:1//
```

Comments occupy a single line and are written as `--`.
Inline comments can be written with the `{--}` macro.
```
-- line comment
1 + {-- inline comment} 2
```

## Value Types
The fundamental values are ints, floats, bools, strings, pair (cons), list, null, fun (lambda), struct, void, and eof.

Ints are merely positive and negative integers such as `1` `30` and `-458`.  

Floats are like ints but have a `.` followed by another int, ie `3.0` `4.5` and `-76.386`.
Floats must feature a `.`.

Bools are the values `true` and `false`.

Strings are written with two double-quotes such as:
`"str"` or `"longer str"`.
Strings can contain `"` as long as they are prefixed with `\`.  
`"string \"within\" a string"`  

Pairs are created with the `,` operator. They have a head and a tail that can be referenced with the \` and ~ operator respectively.  

Lists are pairs that eventually end in the value null `()`.

`void` indicates the lack of meaningful computation and `eof` indicates the end of a stream.  

Lambdas are anonymous functions and are created with the `lambda` keyword and are discussed later.


## Basics and Assignment
`def` is used to create global variables.
```
> def x := 3.
> x.
3
```

"Private" variables are declared with a leading underscore:
```
def _x := 3.
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

### Local Recursive Vars
To create a local recursive function, use a global binding of the following form:
```
defun _id_ args := ...
-- or alternatively
def _id_ := lambda args := ...
```

For example:
```
let get_last :=
    defun _get_last_ n := match n | () -> () | h, () -> h | h,t -> _get_last_:t end  
in 
    -- prints 3
    @ get_last:(1,2,3;).
```

Variables of this form can be thought of as "local private" and should only be used in this manner.


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

-- prints "true"
@ odd?:3.
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
> match 5 | int a when pos?:a -> -a | int a when not (pos?:a) -> 2*a end.
-5
> match -5 | int a when pos?:a -> -a | int a when not (pos?:a) -> 2*a end.
-10
```

## structs
The type of a struct must first be created using the `typedef` keyword.
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

Also you can ask if something is a struct using the `struct?` keyword:
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
-- creates a function that adds one to its argument
lam x := x + 1.
```

Functions are applied to using the `apply` keyword, often shortened to `:`
```
> (lam x := x + 1) apply 2.
3
```

Lambdas can be assigned to variables to be used throughout a program:
```
> def add2 := lam x := x + 2.
> add2:5.
7
```

Lambdas can be constructed without any arguments and then called with any value.
```
def print5 := lam () := (@ 5) comma void.
-- prints 5
print5:().
-- same
print5:2.
```

The keyword `:>` can be used to call a lambda with `()`
```
> :>print5.
5
> :> lam x := null? x.
true
```

Lambdas can be guarded with type notations:
```
def add2 := lam int x := x + 2.
-- contract exception
add2:5.0.
```

Lambdas remember the local context of when they were created:
```
def add_to_3 := 
    let v := 3 in lam x := x + v.
    
-- yields 8
add_to_3:5.
```

Lambdas can be chained together:
```
def add_both := lam x := lam y := x + y.

-- yields a function
add_both:4.
-- yields 6
add_both:4:2.
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
        
defun print5 () := (@ 5) comma void.
```

You can check whether something is a function using the `fun?` question and `fun` guard.
```
> fun? lam x := x.
true
> def fun_app := lam fun f, v := f:v.
> fun_app : 3.
CONTRACT: Recieved type int for var f but expected fun.
> fun_app : (lam x := x - 1) : 3.
2
```

The `appl` (apply list) keyword can be used to use each element of a list as an argument to a function.
```
appl func list
```
for example:
```
> def cat3 := lam x,y,z := x$y$z.

-- equivalent to "C":"B":"A":cat3
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
CONTRACT: Recieved type float for var x but expected one of (int;).
> def types ("int", "float";) x := 5.5.
5
> def types ("int", "float";) x := "5.5".
CONTRACT: Recieved type string for var x but expected one of (int, float;).

> typedef S := types ("int","bool";) a, float b;.

> struct S (true,4.5;).
(struct S (true, 4.5;))
> struct S (5.5, 4.5;).
GENERIC: Could not validate struct against type schema: ...
```

## Error Handling
Errors messages have the following form:
```
id:(space)main message
```

for example:
```
> 5.0 / 0.
CONTRACT: Divide by zero.

> 6
PARSE: No termination of statement.

> error "my error"
GENERIC: my error
```
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
-- yields 2
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
GENERIC: my message

> error "MY_TYPE","my message";.
MY_TYPE: my message
```

Error IDs are written in all caps with underscore seperators.

## Eval
The `eval` keyword allows you to run cm code within cm as a string.
```
eval str
```

To evaluate an expression, use the `evalxp` keyword.
```
evalxp str
```

With `eval`, the result will be a list, where each list element corresponds to a statement.
With `evalxp`, only a value is returned.
```
> evalxp "4 + 1"
5
> eval "4 + 1."
(5;)
> eval "5. 7."
(5, 7;)
```

## Looping
The `while` keyword is used to form a loop.
```
while e1 do e2
```

The while loop will run the body expression e2 as long as the guard expr e1 evaluates to true.
After running e2 for the final time, the while loop will yield `void`.
```
> (def x := 3) comma while x < 7 do @ def x := x + 1.
3
4
5
6
7

```

Additionally there is a `foreach` loop for referencing members of a list, processing them, and yielding `void` when the list is empty.
The syntax is as follows:
```
foreach guard in expr1 do expr2
```
where `guard` is a match expression as expected for the `match` keyword.
```
> foreach x in 1,2,3; do @ x+1.
2
3
4

> typedef S := a,b;.
> foreach struct S (a,b;), n in (struct S (1,2;), 3), (struct S ("a","b";), "c"); do @ n, b, a.
(3, 2, 1)
(c, b, a)

```

## Hashes
The hashmap data type is used to allow constant time referencing of values with arbitrary keys.
A hashmap is created using the `make_hash` keyword.
There are two types of hashmaps: mutable and immutable.
Mutability is set using the argument to the `make_hash` constructor:
```
> make_hash ().
immutable hash
> make_hash "immutable".
immutable hash
> make_hash "mutable".
mutable hash
```

Mappings are formed with the `hash_set` operator.
For immutable hashes `hash_set` yields another hash with the new mapping added or updated.
For mutable hashes `void` is returned and the mapping is made in the original hash:
```
> hash_to_list : (hash_set (make_hash "immutable") 3 4).
(3, 4;)
> def x := make_hash "mutable".
> (hash_set x 3 4).
> hash_to_list : x.
(3, 4;)
```

Hash mappings can be referenced with the `hash_ref` keyword.
```
> hash_ref (hash_set (make_hash "immutable") 3 4) 3.
4
> def x := make_hash "mutable".
> (hash_set x 3 4).
> hash_ref 3.
4
```

If a mapping cannot be found then a HASHREF error is raised.
```
> hash_ref x 5.
1:HASHREF: Could not find key 5 in hash.
```

To subvert this error, the `hash_ref_check` keyword can be used.
```
> hash_ref_check x 5 (lambda v := "sorry, I could not find " $ v $ " in the hash").
sorry, I could not find 5 in the hash
```

To make this change of behavior automatic, the lambda can be passed into the hash constructor.
```
def x := make_hash ("mutable", (lambda v := "sorry, I could not find " $ v $ " in the hash");).
> hash_ref x 5
sorry, I could not find 5 in the hash
```

To ask if a value is a hash, use the `hash?` keyword.
To ask if a value is a mutable hash, use the `mutable_hash?` keyword.
```
> hash? 5.
false
> hash? x.
true
> mutable_hash? x.
true
> mutable_hash (make_hash ()).
false
```

To get all of the keys (in no particular order) in a hashmap as a list use the `hash_keys` keyword.
To get all of the values in a hashmap as a list use the `hash_values` keyword.
To get the entire hashmap in list format where the list is of form ((key1, value1), (key2, value2), ...;) use the `hash_to_list` keyword
```
> def x := make_hash "mutable".
> (hash_set x 3 5) comma (hash_set x true false).
> hash_keys x.
(true, 3;)
> hash_values x.
(5, false;)
> hash_to_list x.
((3, 5), (true, false);)
```

To make hash construction easier there are two utility functions in `std.cm` called `list_to_hash` and `list_to_mutable_hash`
```
> list_to_hash : ((3,5), (true, false);).
immutable hash
> list_to_mutable_hash : ((3,5), (true, false);).
mutable hash
> let h := list_to_hash : ((3,5), (true, false);) in hash_to_list h.
((3, 5), (true, false);)
```

## Index
The `index` keyword is used to reference elements of lists, strings, and hashes.
`index` with one arg means a simple reference
```
> index "abcd" 1.
b
> index (1,2,3,4;) 1.
2
> index (hash_set (make_hash ()) 1 3) 1.
3
```

Index with a list of two elements means a slice (list and string only)
```
> index "abcd" (1,3;).
bc
> index (1,2,3,4;) (1,3;).
(2,3;)
> index (hash_set (make_hash ()) 1 3) (1,3;).
HASHREF: Could not find key (1, 3;) in hash.
```

Index can also be called using the `::` infix keyword:
```
> "abcd"::1.
b
> (1,2,3,4;)::1.
2
> (hash_set (make_hash ()) 1 3)::1.
3
```

## Macros
Macros have two forms: pre-lexing macro definitions, and pre-parsing macro applications.  
Macro defs have two forms: line defs and multi line defs.  
Line macro defs are declared using the `#:` prefix at the beginning of the line.  
Multi line macro defs are declared with `#:<` and terminated with `>:#`.  

Macro defs have two parts: the name and the body. The name is declared just after the `#:` or `#:<`
and features no spaces.  

The macro body is declared just after the name in a single line macro def, and is placed on the lines after the body
in a multi-line macro def.
```
-- single line macro, imports standard lib
#:lang cm

-- same as above, notice that "lang" is the name and "cm" is the body 
#:<lang
cm
>:#
```

### Inline Macros
To declare the schema and body for an inline macro use the following form:
```
#:def:label{args} body

-- multi-line
#:<def:label{args}
body1
body2
...
>:#
```

The args for the macro def can be zero or more, and seperated by `|`.  
The names for the vars in the args must match the regex `[a-zA-Z0-9_]+`.  

Inline macros are applied with the following form:
```
-- no args
{label}
-- one arg
{label arg}
-- two args...
{label arg1|arg2}
```


Additional definitions for the same label can be added with the `#:def+` macro declaration.  
When the macro is applied, the definitions are searched in the order they were added until a matching macro schema is found
for the given arguments.
```
#:def:add{a|b} (a) + (b)
#:def+:add{a} (a)

> {add 3|4}
7
> {add 3}
3

#:<def+:add{a|b|c}
a +
    b +
        c
>:#

> {add 2|3|4}
9
```

Macro applications can also be nested:
```
> {add {add 4 | 5} | {add 1}}
10
```

The `|` operator is required to seperate macro args within a macro application.  
To escape `|` simply use `\|` instead.
```
> {vari + | \| false -> 1 else 2 | 3}
5
```

### Reursive Macros
The `REST` macro arg can be used to refer to the remaining argments to the macro application.  
`REST` must be the final argument to the macro def.
```
-- variadic infix operator
#:def:vari{op|v|REST} (v) op {vari op|REST}
#:def+:vari{op|v} (v)

> {vari -|1}
1
> {vari -|1|2|3}
-4
> {vari -|1|{vari *|3|4}|3}
-14
```


## Operating System
There are a number of features to use or manipulate the system in cm.

`ls` returns the contents of a directory as a list.
```
> ls ".".
(f.txt, dr, a.c;)
> -- we can use the utility function pprint (pretty print) from std.cm to print the list
> (ls "."):pprint.
f.txt
dr
a.c
> -- we can also use the utility function "lsc" (list current) as a shorthand for this
> :>lsc.
f.txt
dr
a.c
```

`cd` is used with a "" arg to display the working directory, and with a longer arg to change it.
```
> cd "".
/home/usr/dir/
> cd "cm".
> cd "".
/home/usr/dir/cm/
> -- we can use the utility function "cdc" to mean cd ""
> :>cdc.
/home/usr/dir/cm/
```

`rm` is used to delete a file or directory
```
> writestr "" "f.txt".
> rm "f.txt".
```
`mkdir` is used to create a directory
```
> mkdir "f".
```
`cp` is used to copy a file or directory
```
> cp "f.txt" "f/g.txt".
```
`mv` is used to move a file or directory
```
> mv "f.txt" "f2.txt".
```
`file_exists?` yields true if the file exists, else false
```
> file_exists? "f.txt".
true
```
`dir_exists?` yields true if the directory exists, else false
```
> dir_exists? "f".
true
```

`getlinesf` yields the lines in a file as a list of strings
```
> getlinesf "f.txt".
("line 1", "this is line 2", "final line";)
> -- the utility func "catf" will pretty print the file for us
> catf:"f.txt".
line1
this is line 2
final line
```
`writestrf` writes the given string to the given file while displaying `\n` as newline.
`writestrf` will replace the file if it already exists.
```
> writestrf "ab\nc" "f.txt".
> getlinesf "f.txt".
("ab", "c";)
```

`appendstrf` is just like `writestrf` except it will append to the file if it already exists
```
> writestrf "ab\nc" "f.txt".
> appendstrf "d\nef" "f.txt"
> getlinesf "f.txt".
("ab", "cd", "ef";)
```

`system` runs a command to `sh` (linux) or `cmd` (windows) and returns true if successful else false.
```
> system "vim *".
true
```
`sysres` will run a system command and yield the output as a string
```
> sysres "date".
Thu Mar 18 01:05:30 EDT 2021
```

`system_type` will return a string corresponding to the current operating system.
There are 3 possible options:
"windows", "unix", or "macosx"
```
match system_type
| a when a = "macosx" or a = "unix" -> system ("rm "$"f.txt")
| "windows" -> system ("del "$"f.txt")
end.
```

## IO
The `@` aka `print` keyword prints its argument to standard out while also printing a newline and
returning the value that it prints. "\n" is interpreted as newline.   
The `write_string` keyword is just like `@` except that it does not add a newline and returns void.  
The `write_string_raw` keyword prints its argument but does not treat "\n" as newline.  

The `read_string` keyword takes in an int as its argument and returns a string of that length.  
The `peek_string` keyword peeks at standard input (without consuming.) It takes in two ints,
where the first is the number of chars to read, and the second is the number of bytes at the begining of the input to skip.  

The `read_line` keyword will read from standard input until a newline is reached.  
 

## Code Examples
```
-- Find Factorial n
def fact := lam int n :=
    | n < 2 -> 1
    else n * (fact:(n - 1)).
    
-- prints 24
@ fact:4.

--------------------------------

-- yields larger number
def max := lam int n1, int n2 :=
    if n1 > n2 then n1 else n2.

-- prints 7
@ appl max (4,7;).
-- still prints 7
@ appl max (7,4;).


--------------------------------

-- Finds last element of list, or errors if list is null
def get_last := lam list lst := 
    match lst 
    | () -> error "List was empty." 
    | h; -> h 
    | h, t -> get_last:t
    end.
   
-- prints 7
@ get_last:(3,5,7;).

-- prints 3
@ get_last:(3;).

-- throws error
@ get_last:null.


--------------------------------

defun print_range (int r1, int r2) :=
    | r1 > r2 -> void
    else
        (def _ := r1 - 1) comma 
        while _ != r2 - 1 do @ set _ := _ + 1//

-- prints nothing
appl print_range (4,4;).
-- prints 4 5 6 7
appl print_range (4,8;).


--------------------------------

-- types valid for btree
def b_types := ("struct Bn", "struct Leaf";).

-- creates binary tree node schema
typedef Bn := 
    dynamic val,
    types b_types left,
    types b_types right;.
    
-- creates leaf schema (notice no args to type constructor)
typedef Leaf := ().

-- simple binary node (parens are necessary around sub struct constructors for syntax reasons)
def b1 := struct Bn (5, (struct Leaf ()), (struct Leaf ());).
-- another with a deeper subtree
def b2 := 
    struct Bn 
        (5,
        (struct Bn 
            (3,
            (struct Leaf ()),
            (struct Leaf ());)),
        (struct Leaf ());).

-- yields the height of a binary tree
defun bn_height (types b_types n) := 
    match n
    | struct Leaf () -> 1 
    | struct Bn (_, left, right;) ->
        1 + appl max ((bn_height:left), (bn_height:right);)
    end.

-- prints 2
@ bn_height:b1.
-- prints 3
@ bn_height:b2.
-- prints 1
@ bn_height:(struct Leaf ()).
```
