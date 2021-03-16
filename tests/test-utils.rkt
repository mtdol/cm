#lang racket
(require cm/core/parse-expr cm/core/lex cm/core/interp)
(provide run parse)

(define (run str) (interp (parse-expr (tokenize-string str))))
(define (parse str) (parse-expr (tokenize-string str)))
