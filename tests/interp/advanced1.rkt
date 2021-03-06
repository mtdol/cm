#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(interp (parse-expr (tokenize-string "def find_a = lam a = let sub1 = lam x = x - 1 in a : sub1")))
(check-equal? (interp (parse-expr (tokenize-string "4 : find_a")))
3)

(interp (parse-expr (tokenize-string "def fact = lam n = | n < 2 -> 1 else n * (n - 1 : fact)")))
(check-equal? (interp (parse-expr (tokenize-string "4 : fact")))
24)
(check-equal? (interp (parse-expr (tokenize-string "1 : fact")))
1)
(check-equal? (interp (parse-expr (tokenize-string "0 : fact")))
1)

(interp (parse-expr (tokenize-string "def get_last = lam list lst = | null? lst -> null | null? ~lst -> `lst else ~lst : get_last")))
(check-equal? (interp (parse-expr (tokenize-string "4,5; : get_last")))
5)
(check-equal? (interp (parse-expr (tokenize-string "5; : get_last")))
5)
(check-equal? (interp (parse-expr (tokenize-string "null : get_last")))
null)

(interp (parse-expr (tokenize-string "def mult2 = lam int n1, int n2 = | n2 = 0 -> 0 | n2 = 1 -> n1 else n1 + ((n2 - 1) : n1 : mult2)")))
(check-equal? (interp (parse-expr (tokenize-string "3 : 4 : mult2")))
12)
(check-equal? (interp (parse-expr (tokenize-string "4 : 3 : mult2")))
12)
(check-equal? (interp (parse-expr (tokenize-string "1 : 3 : mult2")))
3)
(check-equal? (interp (parse-expr (tokenize-string "3 : -2 : mult2")))
-6)
)
