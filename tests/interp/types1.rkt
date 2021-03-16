#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "def types (\"int\";) x := 5")
5)
(check-equal? (run "x")
5)

(check-equal? (run "def types (\"dynamic\";) x := 5.5")
5.5)
(check-equal? (run "x")
5.5)

(check-equal? (run "def types (\"dynamic\", \"int\";) x := 5.5")
5.5)
(check-equal? (run "x")
5.5)

(check-equal? (run "def types (\"float\", \"int\";) x := 5.5")
5.5)
(check-equal? (run "x")
5.5)

(check-equal? (run "def types (\"int\", \"float\";) x := 5.5")
5.5)
(check-equal? (run "x")
5.5)

(check-exn exn:fail? (lambda ()
  (run "def types (\"float\", \"int\";) x := true")))

(check-equal? (run "def types (\"int\", \"float\";) x, y := 5.5")
5.5)
(check-equal? (run "x")
5.5)
(check-equal? (run "y")
5.5)

(check-equal? (run "def types (\"int\", \"float\";) x, float y := 5.5")
5.5)
(check-equal? (run "x")
5.5)
(check-equal? (run "y")
5.5)

(check-exn exn:fail? (lambda ()
  (run "def types (\"float\", \"int\";) x, int y := 5.5")))

(check-equal? (run "def types (\"int\", \"fl\"$\"oat\";) x := 5.5")
5.5)
(check-equal? (run "x")
5.5)

(check-exn exn:fail? (lambda ()
  (run "def types (\"int\", \"fl\"$\"at\";) x := 5.5")))

(run "def str := \"float\"")
(run "def lst := (\"int\", \"float\";)")

(check-equal? (run "def types (\"int\", str;) x := 5.5")
5.5)
(check-equal? (run "x")
5.5)

(check-equal? (run "def types lst x := 5.5")
5.5)
(check-equal? (run "x")
5.5)

(check-exn exn:fail? (lambda ()
  (run "def types null x := 5.5")))
(check-exn exn:fail? (lambda ()
  (run "def types 3 x := 5.5")))
(check-exn exn:fail? (lambda ()
  (run "def types (3;) x := 5.5")))


(check-equal? (run "let types (\"int\", \"float\";) x := 5.5 in x + 1.0")
6.5)
(check-equal? (run "let types (\"int\", str;) x := 5.5 in x + 1.0")
6.5)
(check-equal? (run "let types lst x := 5.5 in x + 1.0")
6.5)

(check-equal? (run "5.0 : lam types (\"int\", \"float\";) x := x + 2.0")
7.0)
(check-equal? (run "5.0 : lam types (\"int\", str;) x := x + 2.0")
7.0)
(check-equal? (run "5.0 : lam types lst x := x + 2.0")
7.0)
(check-equal? (run "8 : 5.0 : lam types lst x, y := + x + float y + 2.0")
15.0)
(check-equal? (run "8 : 5.0 : lam types lst x, types (\"int\", \"string\";) y := + x + float y + 2.0")
15.0)
(check-exn exn:fail? (lambda ()
  (run "8.0 : 5.0 : lam types lst x, types (\"int\", \"string\";) y := + x + float y + 2.0")))
