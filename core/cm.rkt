#lang racket
;(require "lex.rkt" "ast.rkt" "parse-expr.rkt" "interp.rkt")
(require cm/core/lex cm/core/ast cm/core/parse-expr cm/core/interp)
(provide cm-run cm-tokenize cm-parse)

;; takes in a string, then tokenizes, parses, and interprets
(define (cm-run input) (interp (parse-expr (tokenize input))))

(define (cm-tokenize input) (tokenize input))

(define (cm-parse input) (parse-expr (tokenize input)))
