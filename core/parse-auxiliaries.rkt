#lang racket
(require cm/core/error cm/core/operators)
(provide
  get-matching-paren
  unwrap-expr
  check-balanced-parens
  balanced-parens?
  check-missing-operator
  missing-operator?
  )
(define error-id "PARSE")



(define (is-paren? v) (or (string=? "(" v) (string=? ")" v)))

(define (is-value? v) (and (not (is-paren? v))
                     (not (is-operator? v))))

;; given the character after a left paren, returns the tail of the list featuring
;; the matching paren
(define (get-matching-paren lst) (get-matching-paren-aux lst 0))
(define (get-matching-paren-aux lst pcount)
    (match lst
            ['() #f] ;; no matching paren
            [(cons "(" t) (get-matching-paren-aux t (add1 pcount))]
            [(cons ")" t) #:when (zero? pcount) t]
            [(cons ")" t) (get-matching-paren-aux t (sub1 pcount))]
            [(cons h t) (get-matching-paren-aux t pcount)]))

;; removes extra parens from the given expression
;; ie: ((3 + 1)) -> 3 + 1
(define (unwrap-expr tokens)
  (match tokens
         [(cons "(" t) #:when 
                  (and (string=? (last tokens) ")")
                      (null? (get-matching-paren t)))
            (unwrap-expr (drop-right t 1))]
         [_ tokens]))

;; returns list if parens are balanced, else throws exception
(define (check-balanced-parens lst) 
  (if (balanced-parens? lst) lst (cm-error error-id "Missing Paren(s).")))
;; determines that every paren (0 or more) in an expr has a matching friend
(define (balanced-parens? lst) (balanced-parens-aux? lst 0))
(define (balanced-parens-aux? lst pcount)
    (match lst
            ['() (zero? pcount)] ;; if pcount is zero, then parens are balanced
            [(cons "(" t) (balanced-parens-aux? t (add1 pcount))]
            [(cons ")" t) (balanced-parens-aux? t (sub1 pcount))]
            [(cons h t) (balanced-parens-aux? t pcount)]))

;; throws exception if expression is missing operator, ie.
;; 1 2 + 3 -> error
(define (check-missing-operator tokens)
  (if (not (missing-operator? tokens)) tokens
    (cm-error error-id "Invalid Expression. Probably missing an operator.")))
(define (missing-operator? tokens) 
    (match tokens
           ['() #f]
           [(cons h1 (cons h2 t)) #:when (is-value? h1)
                (match (is-operator? h2)
                       [#t (missing-operator? (cons h2 t))]
                       [#f #t])]
           [(cons h t) (missing-operator? t)]))
