#lang racket
(require cm/core/ast cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

;; will fail since no statement termination
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "eval \"4\"")))))

(check-equal? (interp (parse-expr (tokenize-string "eval \"4.\"")))
4)

(check-equal? (interp (parse-expr (tokenize-string "eval \"4 + 1.\"")))
5)

(check-equal? (interp (parse-expr (tokenize-string "eval \"def x := 7.\"")))
7)

(check-equal? (interp (parse-expr (tokenize-string "x")))
7)

(check-equal? (interp (parse-expr (tokenize-string "string eval \"4. 5.\"")))
"(4, 5;)")

(check-equal? (interp (parse-expr (tokenize-string "eval \"typedef S := a,b;.\"")))
(Prim0 'void))

(check-equal? (interp (parse-expr (tokenize-string "string eval \"struct S (4,5;).\"")))
"(struct S (4, 5;))")

(check-equal? (interp (parse-expr (tokenize-string "eval \"string struct S (4,5;).\"")))
"(struct S (4, 5;))")

(check-equal? (interp (parse-expr (tokenize-string "string struct S (4,5;)")))
"(struct S (4, 5;))")

(check-equal? (interp (parse-expr (tokenize-string 
        "eval \"match struct S (4,5;) | struct S (a,b;) -> a end.\"")))
4)

)
