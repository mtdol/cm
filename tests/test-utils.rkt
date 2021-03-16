#lang racket
(require cm/core/parse-expr cm/core/lex cm/core/interp)
(provide run run-silent parse)

(define (run str) (interp (parse-expr (tokenize-string str))))
(define (run-silent str) (interp (parse-expr (tokenize-string str))) (void))
(define (parse str) (parse-expr (tokenize-string str)))
