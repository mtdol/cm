#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "def x := 3")

(check-equal? (run "var \"x\"")
3)

(check-equal? (run "def var \"y\" := 4")
4)

(check-equal? (run "var \"y\"")
4)

(check-equal? (run "y")
4)

;; cannot be empty
(check-failure run "def var \"\" := 5")

;; this var name would usually be illegal
(check-equal? (run "def var \"~y 4\" := 5")
5)

(check-equal? (run "var \"~y 4\"")
5)

(check-equal? (run "def int var \"~z 4\" := 5")
5)

(check-equal? (run "var \"~z 4\"")
5)

(check-equal? (run "def types (\"int\";) var \"~z1 4\" := 5")
5)

(check-equal? (run "var \"~z1 4\"")
5)

(check-failure run "def types (\"float\", \"string\";) var \"~z2 4\" := 5")

(check-equal? (run "def types (\"int\";) var \"~z2 4\" := def dynamic var \"2\" := 5")
5)

(check-equal? (run "var \"~z2 4\"")
5)

(check-equal? (run "var \"2\"")
5)

(check-equal? (run "let types (\"int\";) var \"3\" := 5 in var \"3\" + 1")
6)

(check-equal? (run "def var \" \" := 2")
2)

(check-equal? (run "var \" \"")
2)
