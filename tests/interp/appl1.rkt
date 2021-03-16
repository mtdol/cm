#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "def add1 := lam n := n + 1")
(check-equal? (run "appl add1 (5;)")
6)

(run-silent "def sub2 := lam n1, n2 := n1 - n2")
(check-equal? (run "appl sub2 (5,7;)")
-2)
(check-equal? (run "appl sub2 (6,-2;)")
8)

(check-equal? (run "appl (lam list x, list y := `x + `y) ((1,3;),(6,5;);)")
7)

(run-silent "def add_heads := lam list x, list y := `x + `y")
(check-equal? (run "appl add_heads ((1,3;),(6,5;);)")
7)
(check-equal? (run "appl add_heads ((7,9;),(-2,4;);)")
5)

;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (run "appl add1 (3,4;)")))
(check-exn exn:fail? (lambda ()
  (run "appl add1 4")))
(check-exn exn:fail? (lambda ()
  (run "appl (4;)")))
(check-exn exn:fail? (lambda ()
  (run "appl sub2 (4,5,6;)")))
(check-exn exn:fail? (lambda ()
  (run "appl sub2 (4,5)")))
(check-exn exn:fail? (lambda ()
  (run "appl add_heads ((4,5;), (6,7;))")))
(check-exn exn:fail? (lambda ()
  (run "appl add_heads ((4,5;), (6,7;), (8,9;);)")))
