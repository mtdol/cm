#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(interp (parse-expr (tokenize-string "def add1 := lam n := n + 1")))
(check-equal? (interp (parse-expr (tokenize-string "appl add1 5;")))
6)

(interp (parse-expr (tokenize-string "def sub2 := lam n1, n2 := n1 - n2")))
(check-equal? (interp (parse-expr (tokenize-string "appl sub2 5,7;")))
-2)
(check-equal? (interp (parse-expr (tokenize-string "appl sub2 6,-2;")))
8)

(check-equal? (interp (parse-expr (tokenize-string "appl (lam list x, list y := `x + `y) (1,3;),(6,5;);")))
7)

(interp (parse-expr (tokenize-string "def add_heads := lam list x, list y := `x + `y")))
(check-equal? (interp (parse-expr (tokenize-string "appl add_heads (1,3;),(6,5;);")))
7)
(check-equal? (interp (parse-expr (tokenize-string "appl add_heads (7,9;),(-2,4;);")))
5)

;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "appl add1 3,4;")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "appl add1 4")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "appl 4;")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "appl sub2 4,5,6;")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "appl sub2 4,5")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "appl add_heads (4,5;), (6,7;)")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "appl add_heads (4,5;), (6,7;), (8,9;);")))))
)
