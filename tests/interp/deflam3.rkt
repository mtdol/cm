#lang racket
(require cm/tests/test-utils rackunit)

;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (run "def x 4")))

(check-exn exn:fail? (lambda ()
  (run "def float x := 4")))

(check-exn exn:fail? (lambda ()
  (run "def int x := 4.0")))

(check-exn exn:fail? (lambda ()
  (run "def bool x := 4.0")))

(check-exn exn:fail? (lambda ()
  (run "def struct x := 4.0")))

(check-exn exn:fail? (lambda ()
  (run "def struct St x := 4.0")))

(check-exn exn:fail? (lambda ()
  (run "def lalala x := 4.0")))

(check-exn exn:fail? (lambda ()
  (run "def lalala x 4.0")))

(check-exn exn:fail? (lambda ()
  (run "def 2 x := 4.0")))

(check-exn exn:fail? (lambda ()
  (run "int x := 4.0")))

(check-exn exn:fail? (lambda ()
  (run "def list x := 4.0")))

(check-exn exn:fail? (lambda ()
  (run "def list x := 4.0,5")))

(check-exn exn:fail? (lambda ()
  (run "def pair x := 4.0")))

(check-exn exn:fail? (lambda ()
  (run "1 + def int x := 4.0")))

(check-exn exn:fail? (lambda ()
  (run "def int x ::= 4")))

(check-exn exn:fail? (lambda ()
  (run "def x := int")))

(check-exn exn:fail? (lambda ()
  (run "let x := 5 in def float x := x")))

(check-exn exn:fail? (lambda ()
  (run "let x := 5 in def float z := x")))


(check-exn exn:fail? (lambda ()
  (run "lam x 4")))

(check-exn exn:fail? (lambda ()
  (run "3 : lam float x := x")))

(check-exn exn:fail? (lambda ()
  (run "3 : lam string x := x")))

(check-exn exn:fail? (lambda ()
  (run "3 : lam bool x := x")))

(check-exn exn:fail? (lambda ()
  (run "\"3\" : lam int x := x")))

(check-exn exn:fail? (lambda ()
  (run "\"3\" : lam struct x := x")))

(check-exn exn:fail? (lambda ()
  (run "\"3\" : lam struct St x := x")))

(check-exn exn:fail? (lambda ()
  (run "\"3\" : lam lala x := x")))

(check-exn exn:fail? (lambda ()
  (run "\"3\" : lam int x, float y := x")))

(check-exn exn:fail? (lambda ()
  (run "4: \"3\" : lam x, float y := x")))

(check-exn exn:fail? (lambda ()
  (run "lam float y x")))

(check-exn exn:fail? (lambda ()
  (run "\"3\" : lam int x := lam float y := x")))

(check-exn exn:fail? (lambda ()
  (run "4: \"3\" : lam x := lam float y := x")))

(check-exn exn:fail? (lambda ()
  (run "let x := lam int y in 3.0 : x")))

(check-exn exn:fail? (lambda ()
  (run "let x := lam int y := y in 3.0 : x")))
