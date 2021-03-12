#lang racket
;(require "error.rkt" "operators.rkt" "parse-auxiliaries.rkt")
(require cm/core/error cm/core/operators cm/core/parse-auxiliaries)
(provide pre-parse-expr)
(define error-id "PARSE")

;; this file is for performing adjustments to the token list, so it
;; will be parsable by the main parser

;; prepares tokens for parsing
(define (pre-parse-expr tokens)
  (replace-unary-plus-minus
  (check-balanced-parens 
        tokens)))

;; replaces unary minus with :uni_minus and unary plus with :uni_plus
;; additionally, throws an exception when a infix operator is treated as
;; a prefix operator
(define (replace-unary-plus-minus tokens)
  (let aux ([tokens tokens] [first? #t])
    (match tokens
           [(cons h t) #:when 
                    (and first? (or (string=? "plus" h) (string=? "minus" h)))
                (aux (cons (string-append ":uni_" h) t) #f)]
           ;; infix op being treated as prefix is error
           [(cons h t) #:when 
                    (and first? (and (is-operator? h)
                                     (string=? (op-to-position h) "infix")))
                (cm-error error-id (format "Missing operand(s) for ~a." h))]
           [(cons h1 (cons h2 t)) #:when 
                      (and (or (string=? "(" h1) 
                               (and (is-operator? h1) (not (zero? (op-to-arity h1))))) 
                           (and (is-operator? h2) (string=? (op-to-position h2) "infix")))
                    (cond [(or (string=? "plus" h2) (string=? "minus" h2))
                        ;; call aux on uni since it is also an operator
                        (cons h1 (aux (cons (string-append ":uni_" h2) t) #f))]
                          [else (cm-error error-id (format "Missing operand(s) for ~a." h1))])]
           [(cons h t) (cons h (aux t #f))]
           ['() '()])))
