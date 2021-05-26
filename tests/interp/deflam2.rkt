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
(run-silent "def r := \\n -> | n = 0 -> 0 else n + (r:(n - 1))")

(check-equal? (run "r:4")
10)
(check-equal? (run "r:1")
1)
(check-equal? (run "r:0")
0)

;; concerning closures
(run-silent "def c := let x := 3 in  \\v -> v + x")
(check-equal? (run "c:4")
7)

;; concerning multiple args
(run-silent "def sub_them := \\v1,v2 -> v1 - v2")
(check-equal? (run "sub_them:3:5")
-2)

;; conerning both
(run-silent "def sub_them_c := let v3 := 3 in \\v1,v2 -> v1 - v2 - v3")
(check-equal? (run "sub_them_c:3:5")
-5)

(run-silent "def sub_them_c2 :=  \\v1 -> let v3 := 3 in  \\v2 -> v1 - v2 - v3")
(check-equal? (run "sub_them_c2:3:5")
-5)
