#lang racket
(require cm/tests/test-utils rackunit)

(run "defun a x := x + 1")
        
(check-equal? (run "4:a")
5)

(run "defun a float x := x + 1.0")
(check-equal? (run "4.0:a")
5.0)

(run "defun a (types (\"int\", \"float\";) x, float y) := x + y")
(check-equal? (run "4.0:1.5:a")
5.5)

(run "defun a (types (\"int\", \"float\";) x, float y) := float x + float y")
(check-exn exn:fail? (lambda ()
  (run "2:3.2:a")))
(check-exn exn:fail? (lambda ()
  (run "2.5:true:a")))
