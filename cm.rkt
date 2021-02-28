#lang racket
(require "lex.rkt" "ast.rkt" "parse-expr.rkt" "interp.rkt")
(provide cm-run cm-tokenize cm-parse)

;; takes in a string, then tokenizes, parses, and interprets
(define (cm-run input) (interp (parse-expr (tokenize input))))

(define (cm-tokenize input) (tokenize input))

(define (cm-parse input) (parse-expr (tokenize input)))
