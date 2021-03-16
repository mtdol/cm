#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "def x := let x := 4 in x + 1")
5)
(check-equal? (run "x")
5)

(check-equal? (run "let x := 4 in (def x := x) + 1")
5)
(check-equal? (run "x")
4)

;; basic recursive case
(run-silent "def r := lam n := | n = 0 -> 0 else n + (n - 1 : r)")

(check-equal? (run "4 : r")
10)
(check-equal? (run "1 : r")
1)
(check-equal? (run "0 : r")
0)

;; concerning closures
(run-silent "def c := let x := 3 in lam v := v + x")
(check-equal? (run "4 : c")
7)

;; concerning multiple args
(run-silent "def sub_them := lam v1,v2 := v1 - v2")
(check-equal? (run "5 : 3 : sub_them")
-2)

;; conerning both
(run-silent "def sub_them_c := let v3 := 3 in lam v1,v2 := v1 - v2 - v3")
(check-equal? (run "5 : 3 : sub_them_c")
1)

(run-silent "def sub_them_c2 := lam v1 := let v3 := 3 in lam v2 := v1 - v2 - v3")
(check-equal? (run "5 : 3 : sub_them_c2")
1)
