#lang racket
(require cm/tests/test-utils rackunit)

;; check if import with prefix works
(run-file-silent "files/basic3/a.cm")

(check-equal? (run-stat "pref_b.")
'(7))

(check-equal? (run-stat "a.")
'(4))

(check-exn exn:fail? (lambda ()
  (run-stat "b.")))

(check-exn exn:fail? (lambda ()
  (run-stat "pref_a.")))


;; since c.cm and d.cm have a cycle, we should get a failure
(check-exn exn:fail? (lambda ()
  (run-file-silent "files/basic3/c.cm")))
