#lang racket
(require cm/core/lex cm/core/ast cm/core/parse-expr 
         cm/core/parse-stat cm/core/interp)
(provide cm-run cm-run-silent cm-run-list cm-run-expr cm-run-file cm-run-file-list
         cm-run-file-silent cm-tokenize cm-tokenize-file
         cm-parse cm-parse-expr cm-parse-file cm-prefix-form)

;; converts the resulting list from interp into values
(define (cm-values lst) (apply values (flatten lst)))

;; takes in a string, then tokenizes, parses, and interprets
;; returns values
(define (cm-run input) (cm-values (interp (parse-stat (tokenize-string input)))))
;; runs and returns void
(define (cm-run-silent input) (begin (interp (parse-stat (tokenize-string input))) (void)))
;; returns a list of strings rather than string values.
(define (cm-run-list input) (interp (parse-stat (tokenize-string input))))
;; returns a string
(define (cm-run-expr input) (interp (parse-expr (tokenize-string input))))
;; interps file, returns values
(define (cm-run-file file) (cm-values (interp (parse-stat (tokenize-file file)))))
;; runs and returns void
(define (cm-run-file-silent file) (begin (interp (parse-stat (tokenize-file file))) (void)))
;; interps file, returns list
(define (cm-run-file-list file) (interp (parse-stat (tokenize-file file))))

(define (cm-tokenize input) (tokenize-string input))
(define (cm-tokenize-file file) (tokenize-file file))

(define (cm-parse-expr input) (parse-expr (tokenize-string input)))
(define (cm-parse input) (parse-stat (tokenize-string input)))
(define (cm-parse-file file) (parse-stat (tokenize-file file)))

(define (cm-prefix-form input) (half-parse-expr (tokenize-string input)))
