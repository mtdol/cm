#lang racket
(require cm/tests/test-utils rackunit)

;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (run "let x := 3")))

(check-exn exn:fail? (lambda ()
  (run "let x := 3 5")))

(check-exn exn:fail? (lambda ()
  (run "let x := 3 in")))

(check-exn exn:fail? (lambda ()
  (run "let x := 3 in in")))

(check-exn exn:fail? (lambda ()
  (run "let x 3 in")))

(check-exn exn:fail? (lambda ()
  (run "let x 3 in 5")))

(check-exn exn:fail? (lambda ()
  (run "let int x 3 in 5")))

(check-exn exn:fail? (lambda ()
  (run "let int x := 3.0 in 5")))

(check-exn exn:fail? (lambda ()
  (run "let bool x := 3.0 in 5")))

(check-exn exn:fail? (lambda ()
  (run "let string x := 3.0 in 5")))

(check-exn exn:fail? (lambda ()
  (run "let struct x := 3.0 in 5")))

(check-exn exn:fail? (lambda ()
  (run "let struct St x := 3.0 in 5")))

(check-exn exn:fail? (lambda ()
  (run "let lala x := 3.0 in 5")))

(check-exn exn:fail? (lambda ()
  (run "x := 3.0 in 5")))

(check-exn exn:fail? (lambda ()
  (run "let x := 3.0 in let float z := 5 in x")))
