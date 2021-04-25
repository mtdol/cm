#lang racket
(require cm/tests/test-utils rackunit)

(run-file-silent "files/load1/a.cm")

(run-silent ":>a_load")

(check-equal? (run ":>a")
2)

(check-equal? (run "b_b")
3)

(check-failure run "b")
