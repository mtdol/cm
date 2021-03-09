#lang racket
(require cm/core/lex cm/core/ast cm/core/parse-expr 
         cm/core/parse-stat cm/core/interp cm/core/ast-to-string
         cm/core/types)
(provide cm-run cm-run-silent cm-run-list cm-run-expr cm-run-file cm-run-file-list
         cm-run-file-silent cm-run-file-silent-abs cm-tokenize cm-tokenize-file
         cm-parse cm-parse-expr cm-parse-file cm-prefix-form
         cm-ast-to-string)

;; converts the resulting list from interp into values
(define (cm-values lst) (apply values (flatten lst)))

;; takes the output from interp and makes it more presentable
(define (filter-values lst)
  (match lst
         ['() '()]
         [(cons h t) (cons (string-coerce h) (filter-values t))]
         [h (string-coerce h)]))

;; takes in a string, then tokenizes, parses, and interprets
;; returns values
(define (cm-run input) (cm-values (filter-values (interp (parse-stat (tokenize-string input))))))
;; runs and returns void
(define (cm-run-silent input) (begin (interp (parse-stat (tokenize-string input))) (void)))
;; returns a list of strings rather than string values.
(define (cm-run-list input) (filter-values (interp (parse-stat (tokenize-string input)))))
;; interps file, returns values
(define (cm-run-file file) (cm-values (filter-values (interp (parse-stat (tokenize-file file))))))
;; runs and returns void
(define (cm-run-file-silent file) (begin (interp (parse-stat (tokenize-file file))) (void)))
;; runs and returns void
(define (cm-run-file-silent-abs file) (begin (interp (parse-stat (tokenize-file-abs file))) (void)))
;; interps file, returns list
(define (cm-run-file-list file) (filter-values (interp (parse-stat (tokenize-file file)))))
;; interps file, returns list
(define (cm-run-file-list-abs file) (filter-values (interp (parse-stat (tokenize-file-abs file)))))

;; returns a string
(define (cm-run-expr input) (interp (parse-expr (tokenize-string input))))

(define (cm-tokenize input) (tokenize-string input))
(define (cm-tokenize-file file) (tokenize-file file))

(define (cm-parse-expr input) (parse-expr (tokenize-string input)))
(define (cm-parse input) (parse-stat (tokenize-string input)))
(define (cm-parse-file file) (parse-stat (tokenize-file file)))

(define (cm-prefix-form input) (half-parse-expr (tokenize-string input)))

(define (cm-ast-to-string input) (ast-to-string (parse-stat (tokenize-string input))))
