#lang racket
(require cm/core/lex cm/core/ast cm/core/parse-expr 
         cm/core/parse-stat cm/core/interp cm/core/ast-to-string
         cm/core/types)
(provide run run-raw run-silent run-expr run-file
         run-file-silent run-file-silent-abs run-tokenize-string run-tokenize-file
         run-parse run-parse-expr run-parse-file run-prefix-form
         run-ast-to-string)

;; takes the output from interp and makes it more presentable
(define (filter-values lst)
  (match lst
         ['() '()]
         [(cons h t) (cons (string-coerce h) (filter-values t))]
         [h (string-coerce h)]))

;; runs and returns void
(define (run-silent input) (begin (interp (parse-stat (tokenize-string input))) (void)))
;; returns a list of strings rather than string values.
(define (run input) (filter-values (interp (parse-stat (tokenize-string input)))))
;; does not do any to string conversions
(define (run-raw input) (interp (parse-stat (tokenize-string input))))
;; interps file, returns list
(define (run-file file) (filter-values (interp (parse-stat (tokenize-file file)))))
;; runs and returns void
(define (run-file-silent file) (begin (interp (parse-stat (tokenize-file file))) (void)))
;; runs and returns void
(define (run-file-silent-abs file) (begin (interp (parse-stat (tokenize-file-abs file))) (void)))
;; interps file, returns list
(define (run-file-abs file) (filter-values (interp (parse-stat (tokenize-file-abs file)))))

;; returns a string
(define (run-expr input) (interp (parse-expr (tokenize-string input))))

(define (run-tokenize-string input) (tokenize-string input))
(define (run-tokenize-file file) (tokenize-file file))

(define (run-parse-expr input) (parse-expr (tokenize-string input)))
(define (run-parse input) (parse-stat (tokenize-string input)))
(define (run-parse-file file) (parse-stat (tokenize-file file)))

(define (run-prefix-form input) (half-parse-expr (tokenize-string input)))

(define (run-ast-to-string input) (ast-to-string (parse-stat (tokenize-string input))))
