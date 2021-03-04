#lang racket
(require cm/core/ast cm/core/error cm/core/parse-expr)
(provide parse-stat)
(define error-id 2)

;; takes a master token list and parses its sub-expressions

(define (parse-stat tokens)
  (let aux ([tokens tokens] [acc '()] [linenum 1] [curr-linenum 1])
    (match tokens 
           [(cons "dot" t) 
            ;; use error handler to display line numbers during errors
                (Stat linenum (cm-error-with-line-handler
                    curr-linenum parse-expr (list (reverse acc)))
                      (aux t '() curr-linenum curr-linenum))]
           [(cons ":newline" t) (aux t acc linenum (add1 curr-linenum))]
           [(cons h t) (aux t (cons h acc) linenum curr-linenum)]
           ['() #:when (not (null? acc)) 
            (cm-error-linenum linenum error-id "No termination of statement.")]
           ['() (EOP)])))
