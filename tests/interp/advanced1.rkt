#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(interp (parse-expr (tokenize-string "def find_a = lam a = let sub1 = lam x = x - 1 in a : sub1")))
(check-equal? (interp (parse-expr (tokenize-string "4 : find_a")))
"3")

(interp (parse-expr (tokenize-string "def fact = lam n = | n < 2 -> 1 else n * (n - 1 : fact)")))
(check-equal? (interp (parse-expr (tokenize-string "4 : fact")))
"24")
(check-equal? (interp (parse-expr (tokenize-string "1 : fact")))
"1")
(check-equal? (interp (parse-expr (tokenize-string "0 : fact")))
"1")

(interp (parse-expr (tokenize-string "def get_last = lam list lst = | null? lst -> null | null? ~lst -> `lst else ~lst : get_last")))
(check-equal? (interp (parse-expr (tokenize-string "4,5; : get_last")))
"5")
(check-equal? (interp (parse-expr (tokenize-string "5; : get_last")))
"5")
(check-equal? (interp (parse-expr (tokenize-string "null : get_last")))
"null")
)
