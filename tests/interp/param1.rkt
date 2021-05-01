#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "param x := 3 in x + 1")
4)

(check-equal? (run "let x := 2 in param x := 3 in x + 1")
3)

(check-equal? (run "let x := 2 in param x := x + 1 in x + 1")
3)

(check-equal? (run "param x := 2 in let x := x + 1 in x + 1")
4)

(run-silent "def x := -2")

(check-equal? (run "param x := 2 in let x := x + 1 in x + 1")
4)

(check-equal? (run "param x := 2 in x + 1")
3)

(check-equal? (run "param x := x in x + 1")
-1)

(run-silent "defun f () := y + 1")

(check-failure run ":>f")

(check-failure run "let y := 3 in :>f")

(check-equal? (run "param y := 3 in :>f")
4)


;; check that guards work

(check-equal? (run "param int x := 2 in x")
2)

(check-failure run "param float x := 2 in x")


;; check if param works with guard functions

(run-silent "defun y_is_1? () := y = 1")

(check-failure run "let ? x y_is_1? := 3 in x")

(check-failure run "param y := 2 in let ? x y_is_1? := 3 in x")

(check-equal? (run "param y := 1 in let ? x y_is_1? := 3 in x")
3)

;; check that new params overide old ones

(check-equal? (run "param x := 2 in param x := 3 in x")
3)

(run-silent "defun f2 () := param y := 3 in y - 1")

(check-equal? (run "param y := 2 in :>f2")
2)
