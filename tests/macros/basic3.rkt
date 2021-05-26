#lang racket
(require cm/tests/test-utils rackunit)

;; import macros
(run-file-silent "files/basic3/a.cm")

(check-equal? (run "a1")
3)

(check-equal? (run "a2")
2)

;; _b is not in a.cm's scope, so eager evaluation of arguments
;; should prevent _b from being meaningful
(check-failure run "{gimme {_b}}")
