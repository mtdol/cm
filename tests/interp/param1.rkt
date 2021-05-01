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
0)

(check-equal? (run "param x := 2 in x + 1")
-1)

(check-equal? (run "param x := x in x + 1")
-1)

(run-silent "defun f () := y + 1")

(check-failure run ":>f")

(check-failure run "let y := 3 in :>f")

(check-equal? (run "param y := 3 in :>f")
4)


;; check that guards work

(check-equal? (run "param int x1 := 2 in x1")
2)

(check-failure run "param float x1 := 2 in x1")


;; check if param works with guard functions

(run-silent "defun y_is_1? () := y = 1")

(check-failure run "let ? x1 y_is_1? := 3 in x1")

(check-failure run "param y := 2 in let ? x1 y_is_1? := 3 in x1")

(check-equal? (run "param y := 1 in let ? x1 y_is_1? := 3 in x1")
3)

;; check that new params overide old ones

(check-equal? (run "param y := 2 in param y := 3 in y")
3)

(run-silent "defun f2 () := param y := 3 in y - 1")

(check-equal? (run "param y := 2 in :>f2")
2)

;;
;; get_param
;;

(run-silent "defun f3 () := get_param x")
(run-silent "def x := 3")

(check-equal? (run "param x := 2 in get_param x")
2)

(check-equal? (run "param x := 2 in :>f3")
2)

(check-failure run "get_param z")
