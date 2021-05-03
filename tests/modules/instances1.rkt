#lang racket
(require cm/tests/test-utils rackunit)

;; checks that `def` creates instance variables that
;; are not shared across modules, but persist within the current module

(run-file-silent "files/instances1/a.cm")

(check-equal? (run "b")
3)

;; change `b`'s value in `a.cm` context
(check-equal? (run "set b := 1")
1)

(check-equal? (run "b")
1)

(run-file-silent "files/instances1/c.cm")

;; the modified value of `b` didn't persist
(check-equal? (run "b")
3)

(check-equal? (run "set b := 2")
2)

(check-equal? (run "b")
2)
