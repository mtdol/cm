#lang racket
(require cm/core/lex cm/core/ast cm/core/parse-expr 
         cm/core/parse-stat cm/core/interp cm/core/ast-to-string
         cm/core/types cm/core/context cm/core/modules)
(provide run run-file run-expr run-tokenize-string run-tokenize-file
         run-parse run-parse-expr run-parse-file run-parse-file run-prefix-form
         run-ast-to-string 
         display-output display-expr-output silent)

;; reads over each list element and prints its tostring
;; value list | value -> void
(define (display-output input)
    (match input
           ['() (void)]
           [(cons h t) (displayln (string-coerce h)) (display-output t)]
           ;; if output is void
           [_ (void)]
           ))

;; coerces an expr to a string and prints
(define (display-expr-output input)
    (displayln (string-coerce input)))

;; list -> void
(define (silent input) input (void))

;; runs a statement
(define (run input) (interp (parse-stat (tokenize-string input))))
;(define (run input) (interp (parse-stat (tokenize-string input))) (print-macro-context))
;; string -> value list
(define (run-file file) 
  (set-current-module-id! (file-name->module-id file))
  (interp (parse-stat (tokenize-file file))))

;; runs an expr (no dot)
(define (run-expr input) (interp (parse-expr (tokenize-string input))))

(define (run-tokenize-string input) (tokenize-string input))
(define (run-tokenize-file file) (tokenize-file file))

(define (run-parse input) (parse-stat (tokenize-string input)))
(define (run-parse-expr input) (parse-expr (tokenize-string input)))
(define (run-parse-file file) (parse-stat (tokenize-file file)))

(define (run-prefix-form input) (half-parse-expr (tokenize-string input)))

(define (run-ast-to-string input) (ast-to-string (parse-stat (tokenize-string input))))
