#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (interp (parse-expr (tokenize-string "let x := 3 in 5")))
5)

(check-equal? (interp (parse-expr (tokenize-string "let x := 3 in x")))
3)

(check-equal? (interp (parse-expr (tokenize-string "let x := 3 in x + 5")))
8)

(check-equal? (interp (parse-expr (tokenize-string "let x := 3 + 7 in -7 + x")))
3)

(check-equal? (interp (parse-expr (tokenize-string "let x := 3 + 7 in -2 + x")))
8)

(check-equal? (interp (parse-expr (tokenize-string "let x := 1 + let y := -4 in y + 2 in x - 5")))
-6)
)
