#lang racket
(require cm/tests/test-utils rackunit)

;; check if lambda remembers the module space it was imported from
(run-file-silent "files/private1/a.cm")

(check-equal? (run-stat "a1.")
'(3))

(check-equal? (run-stat "a2.")
'(3))
