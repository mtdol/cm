#lang racket
(require cm/tests/test-utils rackunit cm/core/ast)

;; should fail since x not yet defined
(check-exn exn:fail? (lambda ()
  (run "while x != 10 do def x := x + 1")))

(run-silent "def x := 3")

(check-equal? (run "while x != 10 do def x := x + 1")
(Prim0 'void))

(check-equal? (run "x")
10)

(run-silent "def y := 3")

(check-equal? (run "(while y != 10 do def y := y + 1) comma y")
10)
