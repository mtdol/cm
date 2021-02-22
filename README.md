# cm
Implementation of the cm language.

Development in progress.

When build is stable, use:
```
racket -e '(require "parse.rkt" "lex.rkt" "interp.rkt") (interp(parse-expr(tokenize "code here")))'
```
in language directory to test language expressions (requires racket).

## language form
Substitute expr for any expression.

Construct | Effect | Example | Yields
------------ | -----|--------|--------
if expr then expr else expr | if expression | if (1+2 = 4) then -7 else "wrong" | "wrong"
expr + expr | addition | 1 + 3 | 4
expr * expr | multiplication | 2 * 3 | 6
expr - expr | subtraction | 4 - 6 | -2
expr / expr | division | 4 / 2 | 2
expr & expr | and | 3 & false | 0
expr ^ expr | exponentiation | 2 ^ 3 | 8
! expr | not | ! true | 0
expr, expr | cons | 1,4,(5,6) | (1 4 . ( 5 . 6))
` expr | head | `(1, 2, 3) | 1 
~ expr | tail (yields 2,3) | ~(1, 2, 3) | 2,3
int expr | int coercion | int 7.9 | 7
int? expr | is int? | int? 6 | 1
cond cons else expr | cond expression | cond [false, 2], [3, 5] else 7 | 5
