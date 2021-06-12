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

(check-equal? (run "def z := def x,y; := 1,2;")
(list 1 2))

(check-equal? (run "z")
(list 1 2))
(check-equal? (run "x")
1)
(check-equal? (run "y")
2)

(check-equal? (run "def z := def int x, int y; := 1,2;")
(list 1 2))

(check-equal? (run "z")
(list 1 2))
(check-equal? (run "x")
1)
(check-equal? (run "y")
2)

(check-failure run "def z := def int x, float y; := 1,2;")


(check-equal? (run "def int x := 2 * ( 2 + 3 )")
10)
(check-equal? (run "x")
10)

(run-silent "def x := \\int y -> y + 1")
(check-equal? (run "x:3")
4)

(run-silent "def x := \\x, y -> x + y")
(check-equal? (run "x:5:3")
8)

(check-equal? (run "( \\x, y -> x + y):5:3")
8)


(run-silent "def v := \\() -> 2 + 1")
(check-equal? (run ":>v")
3)

;; can't apply arg onto null-arg lambda
(check-failure run "v:()")
