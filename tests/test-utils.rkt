#lang racket
(require (prefix-in main: cm/core/main) (prefix-in tokens: cm/core/tokens)
         rackunit
         cm/core/ast)
(provide (all-defined-out))

;; for use in io
(define o (open-output-string))
(define (refresh) (close-output-port o) (set! o (open-output-string)))

(define (run-racket-silent expr)
  expr (void))

(define (run str) 
  (main:run-expr str))
(define (run-silent str)
  (main:silent (main:run-expr str)))
(define (run-stat str)
  (main:run str))
(define (run-stat-silent str)
  (main:silent (main:run str)))
(define (run-file-silent str)
  (main:silent (main:run-file str)))
(define (parse str)
  (main:run-parse-expr str))
(define (tokenize str) 
  (main:run-tokenize-string str))

(define-syntax-rule (check-failure f v) 
  (check-exn exn:fail? (lambda ()
    (f v))))

(define-syntax-rule (check-failure-args f vs) 
  (check-exn exn:fail? (lambda ()
    (apply f vs))))


;; runs `f` while gathering the output.
(define (output f str)
  (parameterize ([current-output-port o])
    (f str) 
    (let ([res (get-output-string o)])
      (refresh)
      res)))

(define (input f str input/str)
  (parameterize ([current-input-port (open-input-string input/str)])
    (f str)))

(define val-true (Bool 1))
(define val-false (Bool 0))
(define val-void (Prim0 'void))
