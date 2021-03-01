#lang racket
(require cm/core/lex cm/core/ast cm/core/parse-expr 
         cm/core/parse-stat cm/core/interp)
(provide cm-run cm-run-expr cm-run-file cm-tokenize cm-tokenize-file
         cm-parse cm-parse-expr cm-parse-file)

;; takes in a string, then tokenizes, parses, and interprets
(define (cm-run input) (interp (parse-stat (tokenize-string input))))
(define (cm-run-expr input) (interp (parse-expr (tokenize-string input))))
(define (cm-run-file file) (interp (parse-stat (tokenize-file file))))

(define (cm-tokenize input) (tokenize-string input))
(define (cm-tokenize-file file) (tokenize-file file))

(define (cm-parse-expr input) (parse-expr (tokenize-string input)))
(define (cm-parse input) (parse-stat (tokenize-string input)))
(define (cm-parse-file file) (parse-stat (tokenize-file file)))
