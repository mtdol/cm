#lang racket
(require cm/core/error cm/core/operators cm/core/tokens)
(provide
  get-matching-paren
  unwrap-expr
  check-balanced-parens
  balanced-parens?
  )
(define error-id "PARSE")

;; given the character after a left paren, returns the tail of the list featuring
;; the matching paren
(define (get-matching-paren lst) (get-matching-paren-aux lst 0))
(define (get-matching-paren-aux lst pcount)
    (match lst
            ['() #f] ;; no matching paren
            [(cons (? (tok=? "(")) t) (get-matching-paren-aux t (add1 pcount))]
            [(cons (? (tok=? ")")) t) #:when (zero? pcount) t]
            [(cons (? (tok=? ")")) t) (get-matching-paren-aux t (sub1 pcount))]
            [(cons h t) (get-matching-paren-aux t pcount)]))

;; removes extra parens from the given expression
;; ie: ((3 + 1)) -> 3 + 1
(define (unwrap-expr tokens)
  (match tokens
         [(cons (? (tok=? "(")) t) #:when 
                  (and (string=? (tok (last tokens)) ")")
                      (null? (get-matching-paren t)))
            (unwrap-expr (drop-right t 1))]
         [_ tokens]))

;; returns list if parens are balanced, else throws exception
(define (check-balanced-parens lst module-id) 
  (if (balanced-parens? lst) lst 
    (cm-error-linenum module-id (line (car lst)) error-id "Missing Paren(s).")))
;; determines that every paren (0 or more) in an expr has a matching friend
(define (balanced-parens? lst) (balanced-parens-aux? lst 0))
(define (balanced-parens-aux? lst pcount)
    (match lst
            ['() (zero? pcount)] ;; if pcount is zero, then parens are balanced
            [(cons (? (tok=? "(")) t) (balanced-parens-aux? t (add1 pcount))]
            [(cons (? (tok=? ")")) t) (balanced-parens-aux? t (sub1 pcount))]
            [(cons h t) (balanced-parens-aux? t pcount)]))
