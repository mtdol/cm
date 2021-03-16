#lang racket
(require cm/tests/test-utils rackunit)

;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (run "x")))

(check-exn exn:fail? (lambda ()
  (run "def 9x := 3")))

(check-exn exn:fail? (lambda ()
  (run "-x")))

(check-exn exn:fail? (lambda ()
  (run "int x")))

(check-exn exn:fail? (lambda ()
  (run "int")))

(check-exn exn:fail? (lambda ()
  (run "-")))

(check-exn exn:fail? (lambda ()
  (run "2 -")))

(check-exn exn:fail? (lambda ()
  (run "2 + ")))

(check-exn exn:fail? (lambda ()
  (run "* ")))

(check-exn exn:fail? (lambda ()
  (run "* 2 ")))

(check-exn exn:fail? (lambda ()
  (run "!4")))

(check-exn exn:fail? (lambda ()
  (run "1 + 1.0")))

(check-exn exn:fail? (lambda ()
  (run "1.0 + 1")))

(check-exn exn:fail? (lambda ()
  (run "1 / 7")))

(check-exn exn:fail? (lambda ()
  (run "3.0 / 0")))

(check-exn exn:fail? (lambda ()
  (run "3.0 / 0.0")))

(check-exn exn:fail? (lambda ()
  (run "3.0 and 0.0")))

(check-exn exn:fail? (lambda ()
  (run "3 = 5.8")))

(check-exn exn:fail? (lambda ()
  (run "true and 0.0")))

(check-exn exn:fail? (lambda ()
  (run "true 0.0")))

(check-exn exn:fail? (lambda ()
  (run "`5")))

(check-exn exn:fail? (lambda ()
  (run "~5")))

(check-exn exn:fail? (lambda ()
  (run "~ 3,2")))

(check-exn exn:fail? (lambda ()
  (run "` 3,2")))

(check-exn exn:fail? (lambda ()
  (run "``(3,2)")))

(check-exn exn:fail? (lambda ()
  (run "`~(3,2)")))

(check-exn exn:fail? (lambda ()
  (run "``~(3,2;)")))

(check-exn exn:fail? (lambda ()
  (run ";;")))

(check-exn exn:fail? (lambda ()
  (run "1,")))

(check-exn exn:fail? (lambda ()
  (run ",")))

(check-exn exn:fail? (lambda ()
  (run ";")))

(check-exn exn:fail? (lambda ()
  (run ",7")))

(check-exn exn:fail? (lambda ()
  (run "int \"not a number\"")))

(check-exn exn:fail? (lambda ()
  (run "let x := 3 in y")))
