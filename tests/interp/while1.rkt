#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/ast cm/core/lex)

(module+ test
           (require rackunit))

(module+ test


;; should fail since x not yet defined
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "while x != 10 do def x := x + 1")))))

(interp (parse-expr (tokenize-string "def x := 3")))

(check-equal? (interp (parse-expr (tokenize-string "while x != 10 do def x := x + 1")))
(Prim0 'void))

(check-equal? (interp (parse-expr (tokenize-string "x")))
10)

(interp (parse-expr (tokenize-string "def y := 3")))

(check-equal? (interp (parse-expr (tokenize-string
        "(while y != 10 do def y := y + 1) comma y")))
10)



)
