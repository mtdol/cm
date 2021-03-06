#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test
;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond | ")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond | 1")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond | 1 -> 2")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond | 1 -> 2 else")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond | 1 -> 2 else else")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond | 1 2 else 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond | -> 2 else 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond | bool 0 -> 2 end")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond | 1 else 2 else 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "else 2")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "-> 2")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "| -> 2")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "cond | -> 2")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "|")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "| 1")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "| 1 -> 2")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "| 1 -> 2 else")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "| 1 -> 2 else else")))))

;(check-exn exn:fail? (lambda ()
  ;(interp (parse-expr (tokenize-string "| 1 2 else 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "| -> 2 else 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "| bool 0 -> 2 end")))))

;(check-exn exn:fail? (lambda ()
  ;(interp (parse-expr (tokenize-string "| 1 else 2 else 5")))))


)
