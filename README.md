# cm
Implementation of the cm language.

Development in progress.

When build is stable, use:
```
racket ./repl.rkt
```
in language directory to test language expressions (requires racket).

## language form
Substitute expr for any expression.

Construct | Effect | Example | Yields | Explanation
------------ | -----|--------|--------
if expr then expr else expr | if expression | if (1+2 = 4) then -7 else "wrong" | "wrong"
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
[] | square parens | [1 + 2] | 3 | same as ()
{} | curly parens | {1 + 2} | 3 | same as ()
print expr | print | print 1 + 2 | 3 
@ expr | print | print 1 + 2 | 3 | alias of print
\\# \\\\# | comment | 4 + \\# 1 + 2 \\\\# 5 | 9
int expr | int coercion | int 7.9 | 7
int? expr | is int? | int? 6 | 1
cond list else expr | cond expression | cond [false, 3;], [3, 5;]; else 7 | 5 | cond takes in a list of lists where each sublist is of length 2.
