#lang racket
(require cm/core/ast cm/core/error cm/core/parse-expr cm/core/pre-parse
         cm/core/context cm/core/tokens)
(provide parse-stat)
(define error-id "SYNTAX")

;; takes a master token list and parses its sub-expressions
;;
;; token list, string -> expr
(define (parse-stat tokens module-id)
  (let aux ([tokens (pre-parse tokens module-id)] [acc '()] [linenum 1] [curr-linenum 1])
    (match tokens 
           [(cons (? (tok=? "dot")) t) 
            ;; set linenum for error messages
            (set-current-linenum! 
              (if (null? acc) curr-linenum (line (last acc))))
            (Stat (get-current-linenum) 
                  (parse-expr (reverse acc) module-id)
                  (aux t '() curr-linenum curr-linenum))]
           [(cons h t) 
            (aux t (cons h acc) linenum 
                 (if (= (line h) -1) curr-linenum (line h)))]
           ['() #:when (not (null? acc)) 
            (cm-error-linenum 
              module-id linenum error-id "No termination of statement.")]
           ['() (EOP)])))
