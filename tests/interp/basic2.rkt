#lang racket
(require cm/tests/test-utils rackunit)

;; check for things that should fail

(check-failure run "x")

(check-failure run "def 9x := 3")

(check-failure run "-x")

(check-failure run "int x")

(check-failure run "int")

(check-failure run "-")

(check-failure run "2 -")

(check-failure run "2 + ")

(check-failure run "* ")

(check-failure run "* 2 ")

(check-failure run "!4")

(check-failure run "1 + 1.0")

(check-failure run "1.0 + 1")

(check-failure run "1 / 7")

(check-failure run "3.0 / 0")

(check-failure run "3.0 / 0.0")

(check-failure run "3.0 and 0.0")

(check-failure run "3 = 5.8")

(check-failure run "true and 0.0")

(check-failure run "true 0.0")

(check-failure run "`5")

(check-failure run "~5")

(check-failure run "~ 3,2")

(check-failure run "` 3,2")

(check-failure run "``(3,2)")

(check-failure run "`~(3,2)")

(check-failure run "``~(3,2;)")

(check-failure run ";;")

(check-failure run "1,")

(check-failure run ",")

(check-failure run ";")

(check-failure run ",7")

(check-failure run "int \"not a number\"")

(check-failure run "let x := 3 in y")
