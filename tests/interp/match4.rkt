#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (interp (parse-expr (tokenize-string "match 3,3 | a,a -> a end")))
3)

(check-equal? (interp (parse-expr (tokenize-string "match 3,3 | a, int a -> a end")))
3)

(check-equal? (interp (parse-expr (tokenize-string "match 3,3 | int a, a -> a end")))
3)

(check-equal? (interp (parse-expr (tokenize-string "match 3,4 | a, a -> a | a,b -> a + b end")))
7)

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "match 3,4 | a,a -> a end")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "match 3,4 | a, int a -> a end")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "match 3,4 | int a, a -> a end")))))


(check-equal? (interp (parse-expr (tokenize-string "match 3,3 | a,b -> a + b end")))
6)


(check-equal? (interp (parse-expr (tokenize-string "match null | a, a -> a | null -> 5 end")))
5)

(check-equal? (interp (parse-expr (tokenize-string "match void | a, a -> a | void -> 5 end")))
5)
)
