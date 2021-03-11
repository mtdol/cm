#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test
;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x := 3")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x := 3 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x := 3 in")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x := 3 in in")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x 3 in")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x 3 in 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let int x 3 in 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let int x := 3.0 in 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let bool x := 3.0 in 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let string x := 3.0 in 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let struct x := 3.0 in 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let struct St x := 3.0 in 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let lala x := 3.0 in 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "x := 3.0 in 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x := 3.0 in let float z := 5 in x")))))
)
