#lang racket
(require cm/core/parse-expr cm/core/parse-stat cm/core/lex cm/core/interp)
(provide run run-silent run-stat run-stat-silent run-file-silent parse)

(define (run str) (interp (parse-expr (tokenize-string str))))
(define (run-silent str) (interp (parse-expr (tokenize-string str))) (void))
(define (run-stat str) (interp (parse-stat (tokenize-string str))))
(define (run-stat-silent str) (interp (parse-stat (tokenize-string str))) (void))
(define (run-file-silent str) (interp (parse-stat (tokenize-file str))) (void))
(define (parse str) (parse-expr (tokenize-string str)))
