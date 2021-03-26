#lang racket
(require cm/tests/test-utils rackunit cm/core/ast)

(check-exn exn:fail? (lambda ()
  (run "foreach x 5 7")))

(check-exn exn:fail? (lambda ()
  (run "foreach x 5 do 7")))

(check-exn exn:fail? (lambda ()
  (run "foreach x in 5 do 7")))

(check-exn exn:fail? (lambda ()
  (run "foreach float x in 5; do 7")))

(check-exn exn:fail? (lambda ()
  (run "foreach x with 5; do 7")))

(check-exn exn:fail? (lambda ()
  (run "foreach x in 5; do y")))


(run-silent "def res := \"\"")
(check-equal? (run "foreach x in 1,2,\"c\"; do def res := res $ x")
(Prim0 'void))

(check-equal? (run "res")
"12c")

(run-silent "def res := \"\"")
(check-equal? (run "foreach x, y; in (1,true;), (\"bear\",3;); do def res := res $ y $ x")
(Prim0 'void))

(check-equal? (run "res")
"true13bear")

(run-silent "def res := \"\"")
(run-silent "typedef S := a,b;")
(check-equal? (run "foreach struct S (a,b;) in (struct S (5,3;)), (struct S (7,1;)); do def res := res $ b $ a")
(Prim0 'void))

(check-equal? (run "res")
"3517")