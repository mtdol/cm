# cm
Implementation of the cm language.

Development in progress.

## Repl
When build is stable, use:
```
racket ./repl.rkt
```
in language directory (core) to test language expressions (requires racket).

## Running Files
The cm.rkt file in the core directory contains racket functions to interpret/parse/tokenize  
cm files/expressions and statements.

### Examples
```
racket -e '(require cm/core/cm) (cm-run "language statement")'
racket -e '(require cm/core/cm) (cm-run-file "file here")'
racket -e '(require cm/core/cm) (cm-parse "statement here")'
racket -e '(require cm/core/cm) (cm-parse-expr "expression here")'
```
Use the following in the cm directory to install cm as a package:
```
cd "path to cm dir"
raco pkg install
```
There are example files in the `examples` directory, although
most of the example files are not yet complete (or correct) but will be made runable in time.


## Language form
Substitute expr for any expression.

Construct | Effect | Example | Yields | Explanation
------------ | -----|--------|--------| --
if expr then expr else expr | if expression | if 1+2 = 4 then -7 else "wrong" | "wrong"
expr + expr | addition | 1 + 3 | 4
expr * expr | multiplication | 2 * 3 | 6
expr - expr | subtraction | 4 - 6 | -2
expr / expr | division | 4 / 2 | 2
expr & expr | and | 3 & false | 0
expr ^ expr | exponentiation | 2 ^ 3 | 8
expr % expr | modulus | 4 % 3 | 1
! expr | not | ! true | 0
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
cond list else expr | cond expression | cond \[false, 3;], \[3, 5;]; else 7 | 5 | cond takes in a list of lists where each sublist is of length 2.
