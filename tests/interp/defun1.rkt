#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(interp (parse-expr (tokenize-string "defun a x := x + 1")))
        
(check-equal? (interp (parse-expr (tokenize-string "4:a")))
5)

(interp (parse-expr (tokenize-string "defun a float x := x + 1.0")))
(check-equal? (interp (parse-expr (tokenize-string "4.0:a")))
5.0)

(interp (parse-expr (tokenize-string 
    "defun a (types (\"int\", \"float\";) x, float y) := x + y")))
(check-equal? (interp (parse-expr (tokenize-string "4.0:1.5:a")))
5.5)

(interp (parse-expr (tokenize-string 
    "defun a (types (\"int\", \"float\";) x, float y) := float x + float y")))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "2:3.2:a")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "2.5:true:a")))))
)
