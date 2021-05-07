#lang racket
(require cm/tests/test-utils rackunit)

;; REMEMBER: these tests create a global mapping of variables, order matters

(check-equal? (run "def int x := 1 * ( 2 + 3 )")
5)

(check-equal? (run "x")
5)

(check-equal? (run "x + 1")
6)

(check-equal? (run "def int x := def y := 1 * ( 2 + 3 )")
5)

(check-equal? (run "y")
5)

(check-equal? (run "y + 1")
6)

(check-equal? (run "def z1, int z2 := def int z3 := 1 * ( 2 + 3 )")
5)

(check-equal? (run "z1")
5)
(check-equal? (run "z2")
5)
(check-equal? (run "z3")
5)

(check-equal? (run "def int x := 2 * ( 2 + 3 )")
10)
(check-equal? (run "z1")
5)
(check-equal? (run "x")
10)

(run-silent "def x := lam int y := y + 1")
(check-equal? (run "x:3")
4)

(run-silent "def x := lam x, y := x + y")
(check-equal? (run "x:5:3")
8)

(check-equal? (run "(lam x, y := x + y):5:3")
8)


(run-silent "def v := lam () := 2 + 1")
(check-equal? (run ":>v")
3)

;; can't apply arg onto null-arg lambda
(check-failure run "v:()")
