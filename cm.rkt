#lang racket
(require "lex.rkt" "ast.rkt" "parse.rkt" "interp.rkt")
(provide run)

;; takes in a string, then tokenizes, parses, and interprets
(define (run input) (interp (parse-expr (tokenize input))))
