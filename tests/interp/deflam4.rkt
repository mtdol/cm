#lang racket
(require cm/tests/test-utils rackunit)

;; test the `?` operator in left hand exprs

(run-silent "defun odd? x := x % 2 = 1")

(check-equal? (run "def ? x odd? := 3")
3)

(check-equal? (run "x")
3)

(check-failure run "def ? x odd? := 2")

;; function must return a bool
(check-failure run "def ? x (lambda _ := 3) := 3")

(check-equal? (run "def ? x (lambda _ := true) := 3")
3)


(run-silent "defun f ? x odd? := x + 1")
(run-silent "defun g ? x (lambda _ := 3) := x - 1")

(check-equal? (run "f:3")
4)

(check-failure run "f:4")

(check-failure run "g:1")
