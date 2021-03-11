#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test
;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def 9x := 3")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "-x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "int x")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "int")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "-")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "2 -")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "2 + ")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "* ")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "* 2 ")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "!4")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "1 + 1.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "1.0 + 1")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "1 / 7")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "3.0 / 0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "3.0 / 0.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "3.0 and 0.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "3 = 5.8")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "true and 0.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "true 0.0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "`5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "~5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "~ 3,2")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "` 3,2")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "``(3,2)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "`~(3,2)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "``~(3,2;)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string ";;")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "1,")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string ",")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string ";")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string ",7")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "int \"not a number\"")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "let x := 3 in y")))))
)
