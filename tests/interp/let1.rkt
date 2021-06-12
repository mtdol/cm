#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "let x := 3 in 5")
5)

(check-equal? (run "let x := 3 in x")
3)

(check-equal? (run "let x := 3 in x + 5")
8)

(check-equal? (run "let x := 3 + 7 in -7 + x")
3)

(check-equal? (run "let x := 3 + 7 in -2 + x")
8)

(check-equal? (run "let x := 1 + let y := -4 in y + 2 in x - 5")
-6)

(check-equal? (run "let x := 1 in let x := 2 in x")
2)

(check-equal? (run "let x,y := 1,2 in x - y")
-1)

(check-equal? (run "let int x, int y := 1,2 in x - y")
-1)

(check-failure run "let int x, float y := 1,2 in y")
