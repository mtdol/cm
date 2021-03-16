#lang racket
(require cm/tests/test-utils rackunit)

;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (run "cond")))

(check-exn exn:fail? (lambda ()
  (run "cond | ")))

(check-exn exn:fail? (lambda ()
  (run "cond | 1")))

(check-exn exn:fail? (lambda ()
  (run "cond | 1 -> 2")))

(check-exn exn:fail? (lambda ()
  (run "cond | 1 -> 2 else")))

(check-exn exn:fail? (lambda ()
  (run "cond | 1 -> 2 else else")))

(check-exn exn:fail? (lambda ()
  (run "cond | 1 2 else 5")))

(check-exn exn:fail? (lambda ()
  (run "cond | -> 2 else 5")))

(check-exn exn:fail? (lambda ()
  (run "cond | bool 0 -> 2 end")))

(check-exn exn:fail? (lambda ()
  (run "cond | 1 else 2 else 5")))

(check-exn exn:fail? (lambda ()
  (run "else 2")))

(check-exn exn:fail? (lambda ()
  (run "-> 2")))

(check-exn exn:fail? (lambda ()
  (run "| -> 2")))

(check-exn exn:fail? (lambda ()
  (run "cond | -> 2")))

(check-exn exn:fail? (lambda ()
  (run "|")))

(check-exn exn:fail? (lambda ()
  (run "| 1")))

(check-exn exn:fail? (lambda ()
  (run "| 1 -> 2")))

(check-exn exn:fail? (lambda ()
  (run "| 1 -> 2 else")))

(check-exn exn:fail? (lambda ()
  (run "| 1 -> 2 else else")))

(check-exn exn:fail? (lambda ()
  (run "| 1 2 else 5")))

(check-exn exn:fail? (lambda ()
  (run "| -> 2 else 5")))

(check-exn exn:fail? (lambda ()
  (run "| bool 0 -> 2 end")))

(check-exn exn:fail? (lambda ()
  (run "| 1 else 2 else 5")))


