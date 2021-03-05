#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test
;; REMEMBER: these tests create a global mapping of variables, order matters

(check-equal? (interp (parse-expr (tokenize-string "def int x = 1 * ( 2 + 3 )")))
5)

(check-equal? (interp (parse-expr (tokenize-string "x")))
5)

(check-equal? (interp (parse-expr (tokenize-string "x + 1")))
6)

(check-equal? (interp (parse-expr (tokenize-string "def int x = def y = 1 * ( 2 + 3 )")))
5)

(check-equal? (interp (parse-expr (tokenize-string "y")))
5)

(check-equal? (interp (parse-expr (tokenize-string "y + 1")))
6)

(check-equal? (interp (parse-expr (tokenize-string "def z1, int z2 = def int z3 = 1 * ( 2 + 3 )")))
5)

(check-equal? (interp (parse-expr (tokenize-string "z1")))
5)
(check-equal? (interp (parse-expr (tokenize-string "z2")))
5)
(check-equal? (interp (parse-expr (tokenize-string "z3")))
5)

(check-equal? (interp (parse-expr (tokenize-string "def int x = 2 * ( 2 + 3 )")))
10)
(check-equal? (interp (parse-expr (tokenize-string "z1")))
5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
10)

(begin (interp (parse-expr (tokenize-string "def x = lam int y = y + 1"))) (void))
(check-equal? (interp (parse-expr (tokenize-string "3:x")))
4)

(begin (interp (parse-expr (tokenize-string "def x = lam x, y = x + y"))) (void))
(check-equal? (interp (parse-expr (tokenize-string "3:5:x")))
8)

(check-equal? (interp (parse-expr (tokenize-string "3:5:(lam x, y = x + y)")))
8)

(check-equal? (interp (parse-expr (tokenize-string "3:5:lam x, y = x + y")))
8)
)
