#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (interp (parse-expr (tokenize-string "def x = let x = 4 in x + 1")))
5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5)

(check-equal? (interp (parse-expr (tokenize-string "let x = 4 in (def x = x) + 1")))
5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
4)

;; basic recursive case
(interp (parse-expr (tokenize-string "def r = lam n = | n = 0 -> 0 else n + (n - 1 : r)")))

(check-equal? (interp (parse-expr (tokenize-string "4 : r")))
10)
(check-equal? (interp (parse-expr (tokenize-string "1 : r")))
1)
(check-equal? (interp (parse-expr (tokenize-string "0 : r")))
0)

;; concerning closures
(interp (parse-expr (tokenize-string "def c = let x = 3 in lam v = v + x")))
(check-equal? (interp (parse-expr (tokenize-string "4 : c")))
7)

;; concerning multiple args
(interp (parse-expr (tokenize-string "def sub_them = lam v1,v2 = v1 - v2")))
(check-equal? (interp (parse-expr (tokenize-string "5 : 3 : sub_them")))
-2)

;; conerning both
(interp (parse-expr (tokenize-string "def sub_them_c = let v3 = 3 in lam v1,v2 = v1 - v2 - v3")))
(check-equal? (interp (parse-expr (tokenize-string "5 : 3 : sub_them_c")))
1)

(interp (parse-expr (tokenize-string "def sub_them_c2 = lam v1 = let v3 = 3 in lam v2 = v1 - v2 - v3")))
(check-equal? (interp (parse-expr (tokenize-string "5 : 3 : sub_them_c2")))
1)
)
