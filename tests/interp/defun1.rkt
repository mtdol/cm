#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "defun a x := x + 1")
        
(check-equal? (run "a:4")
5)

(run-silent "defun a float x := x + 1.0")
(check-equal? (run "a:4.0")
5.0)

(run-silent "defun a (types (\"int\", \"float\";) x, float y) := x + y")
(check-equal? (run "a:1.5:4.0")
5.5)

(run-silent "defun v () := 2 + 1")
(check-equal? (run ":>v")
3)

(check-failure run "v:()")

(run-silent "defun a (types (\"int\", \"float\";) x, float y) := float x + float y")
(check-exn exn:fail? (lambda ()
  (run "a:3.2:2")))
(check-exn exn:fail? (lambda ()
  (run "a:true:2.5")))
