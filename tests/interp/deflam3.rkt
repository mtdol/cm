#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def x 4")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def float x := 4")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def int x := 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def bool x := 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def struct x := 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def struct St x := 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def lalala x := 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def lalala x 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def 2 x := 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "int x := 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def list x := 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def list x := 4.0,5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def pair x := 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "1 + def int x := 4.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def int x ::= 4")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def x := int")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x := 5 in def float x := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x := 5 in def float z := x")))))


(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "lam x 4")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "3 : lam float x := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "3 : lam string x := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "3 : lam bool x := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "\"3\" : lam int x := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "\"3\" : lam struct x := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "\"3\" : lam struct St x := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "\"3\" : lam lala x := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "\"3\" : lam int x, float y := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "4: \"3\" : lam x, float y := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "lam float y x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "\"3\" : lam int x := lam float y := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "4: \"3\" : lam x := lam float y := x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x := lam int y in 3.0 : x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x := lam int y := y in 3.0 : x")))))
)
