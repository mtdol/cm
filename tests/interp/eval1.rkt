#lang racket
(require cm/tests/test-utils rackunit cm/core/ast)

(run-silent "#:lang cm")

;; will fail since no statement termination
(check-exn exn:fail? (lambda ()
  (run "eval : \"4\"")))

(check-equal? (run "eval : \"4//\"")
(list 4))

(check-equal? (run "eval : \"4 + 1//\"")
(list 5))

(check-equal? (run "eval : \"def x := 7//\"")
(list 7))

(check-equal? (run "x")
7)

(check-equal? (run "eval : \"4// 5//\"")
(list 4 5))

(check-equal? (run "string (eval : \"4// 5//\")")
"(4, 5;)")

(check-equal? (run "eval : \"typedef S := a,b;//\"")
(list (Prim0 'void)))

(check-equal? (run "string (eval : \"struct S (4,5;)//\")")
"((struct S (4, 5;));)")

(check-equal? (run "eval : \"string struct S (4,5;)//\"")
(list "(struct S (4, 5;))"))

(check-equal? (run "string struct S (4,5;)")
"(struct S (4, 5;))")

(check-equal? (run "eval : \"match struct S (4,5;) | struct S (a,b;) -> a end//\"")
(list 4))


;; test evalxp
(check-exn exn:fail? (lambda ()
  (run "evalxp : \"4//\"")))

(check-equal? (run "evalxp : \"4\"")
4)

(check-equal? (run "evalxp : \"4 + 1\"")
5)

(check-equal? (run "evalxp : \"def x := 7\"")
7)

(check-equal? (run "x")
7)
