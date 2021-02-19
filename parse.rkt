#lang racket
(provide parse-expr ast-to-expr)
(require "lex.rkt" "ast.rkt")

;; Auxiliaries

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

;; determines that every paren (0 or more) in an expr has a matching friend
(define (balanced-parens? lst) (balanced-parens-aux? lst 0))
(define (balanced-parens-aux? lst pcount)
    (match lst
            ['() (zero? pcount)] ;; if pcount is zero, then parens are balanced
            [(cons "(" t) (balanced-parens-aux? t (add1 pcount))]
            [(cons ")" t) (balanced-parens-aux? t (sub1 pcount))]
            [(cons h t) (balanced-parens-aux? t pcount)]))


;; returns the ast back with proper parenthesis
(define (ast-to-expr ast) (apply string-append (ast-to-expr-aux ast)))
(define (ast-to-expr-aux ast) 
    (match ast
           [(Stat e st) (list (ast-to-expr e) ". " (ast-to-expr st))]
           [(EOP) (list)]
           [(Let v e1 e2) (list "let " (ast-to-expr v) " = "
                                (ast-to-expr e1) (ast-to-expr e2) )]
           [(Def v e) (list "def " (ast-to-expr v) " = "
                                (ast-to-expr e))]
           [(Lambda v e) (list "lam " (ast-to-expr v) " " (ast-to-expr e))]
           [(Prim1 op e1 e2) #:when (symbol=? op 'cons)
                              (flatten (list "(" (ast-to-expr-aux e1)
                                     ", " (ast-to-expr-aux e2) ")"))]
           [(Prim1 op e1 e2) (flatten
                               (list "(" (ast-to-expr-aux e1)
                                     " " (symbol->string (translate-op-name op))
                                     " " (ast-to-expr-aux e2) ")"))]
           [(Prim2 op e) (flatten
                               (list "(" (symbol->string (translate-op-name op))
                                     " " (ast-to-expr-aux e) ")"))]
           [(If e1 e2 e3) (flatten
                               (list "(" "if " (ast-to-expr-aux e1)
                                  " " (ast-to-expr-aux e2) 
                                  " " (ast-to-expr-aux e3) ")"))]
           [(Print e) (flatten (list "(" "print " (ast-to-expr-aux e) ")"))]
           [(Int i) (list (number->string i))]
           [(Float f) (list (number->string f))]
           [(String s) (list "\"" s "\"")]
           [(Var v) (list v)]))

;; translates op names if necessary to the form used in the language
(define (translate-op-name op) 
  (match op
    ['add 'plus]
    ['sub 'minus]
    ['apply ':]
    [op op]))


(define (is-keyword? v)
  (set-member? reserved-keywords v))
(define (is-operator? v)
  (set-member? operators v))
(define (is-string? v) 
         (and (string? v) (string=? (substring v 0 1) "\"")))
(define (is-int? v)
  (and (string->number v) (exact-integer? (string->number v))))
(define (is-float? v)
  (and (string->number v) (flonum? (string->number v))))
(define (is-variable? v) 
  (and (not (is-keyword? v)) (regexp-match? #rx"^[a-zA-Z_][a-zA-Z1-9_]*[?]*[']*$" v)))
;; is the given token a fundamental value?
(define (is-value? v)
      (or (is-variable? v)
          (is-int? v)
          (is-float? v)
          (is-string? v)))
;; returns true if the given token could precede a unary operator
(define (precedes-unary? v) 
  (and (not (string=? ")" v)) (not (is-value? v))))

;; removes unary plus and replaces unary minus with the token ":uni_minus"
(define (replace-unary-minus tokens first?) 
    (match tokens
           ['() '()]
           [(cons "minus" t) #:when first? (cons ":uni_minus" (replace-unary-minus t #f))]
           [(cons "plus" t) #:when first? (replace-unary-minus t #f)]
           [(cons h1 (cons "plus" t)) #:when (precedes-unary? h1)
                (replace-unary-minus (cons h1 t) #f)]
           [(cons h1 (cons "minus" t)) #:when (precedes-unary? h1)
               (cons h1 (replace-unary-minus (cons ":uni_minus" t) #f))]
           [(cons h t) (cons h (replace-unary-minus t #f))]))


;; begin parsing code

(define (parse-expr tokens)
  (parse-expr-aux (replace-unary-minus tokens #t)))
    ;(replace-unary-minus tokens #t))

(define (parse-expr-aux tokens) 
  (match tokens
         ;; this case removes the extra set of parens around the entire expression
         ;; if such parens are present, this is required for the later functions
        [(cons "(" t) #:when 
                    (and (string=? (last tokens) ")")
                        (null? (get-matching-paren t)))
            (parse-print 
              (drop-right t 1)
              '() 0)] ;; trim off the left and right parens if necessary
        [_ (parse-print tokens '() 0)]))

(define (parse-print tokens acc pcount)
    (match tokens
           [(cons "print" t) #:when (zero? pcount) (Print (parse-expr-aux t))]
           [(cons "(" t) (parse-print t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-print t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-apply (reverse acc) '() 0)]
           [(cons h t) (parse-print t (cons h acc) pcount)]
           [_ (error "print parse error")]))

(define (parse-apply tokens acc pcount)
    (match tokens
           ;; when we have found the operator, everything to our left is the left
           ;; operand, everything to our right is the right one

           ;; parencount tells us whether we are currently in a subexpression
           ;; we can only form ast when not in a subexpression
           [(cons "apply" t) #:when (zero? pcount) (Prim1 'apply (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-apply t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-apply t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-cons (reverse acc) '() 0)]
           [(cons h t) (parse-apply t (cons h acc) pcount)]
           [_ (error "apply parse error")]))

(define (parse-cons tokens acc pcount)
    (match tokens
           [(cons "cons" t) #:when (zero? pcount) (Prim1 'cons (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-cons t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-cons t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-andor (reverse acc) '() 0)]
           [(cons h t) (parse-cons t (cons h acc) pcount)]
           [_ (error "cons parse error")]))

(define (parse-andor tokens acc pcount)
    (match tokens
           [(cons "and" t) #:when (zero? pcount) (Prim1 'and (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "or" t) #:when (zero? pcount) (Prim1 'or (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-andor t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-andor t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-comp (reverse acc) '() 0)]
           [(cons h t) (parse-andor t (cons h acc) pcount)]
           [_ (error "andor parse error")]))

(define (parse-comp tokens acc pcount)
    (match tokens
           [(cons "gt" t) #:when (zero? pcount) (Prim1 'gt (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "lt" t) #:when (zero? pcount) (Prim1 'lt (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "equal" t) #:when (zero? pcount) (Prim1 'equal (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "ge" t) #:when (zero? pcount) (Prim1 'ge (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "le" t) #:when (zero? pcount) (Prim1 'le (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-comp t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-comp t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-addsub (reverse acc) '() 0)]
           [(cons h t) (parse-comp t (cons h acc) pcount)]
           [_ (error "comp parse error")]))

(define (parse-addsub tokens acc pcount)
    (match tokens
           [(cons "plus" t) #:when (zero? pcount) (Prim1 'add (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "minus" t) #:when (zero? pcount) (Prim1 'sub (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-addsub t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-addsub t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-multdiv (reverse acc) '() 0)]
           [(cons h t) (parse-addsub t (cons h acc) pcount)]
           [_ (error "addsub parse error")]))

(define (parse-multdiv tokens acc pcount)
    (match tokens
           [(cons "mult" t) #:when (zero? pcount) (Prim1 'mult (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "div" t) #:when (zero? pcount) (Prim1 'div (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "mod" t) #:when (zero? pcount) (Prim1 'mod (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-multdiv t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-multdiv t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-exp (reverse acc) '() 0)]
           [(cons h t) (parse-multdiv t (cons h acc) pcount)]
           [_ (error "multdiv parse error")]))

(define (parse-exp tokens acc pcount)
    (match tokens
           [(cons "exp" t) #:when (zero? pcount) (Prim1 'exp (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-exp t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-exp t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-unary (reverse acc) '() 0)]
           [(cons h t) (parse-exp t (cons h acc) pcount)]
           [_ (error "exp parse error")]))

(define (parse-unary tokens acc pcount)
    (match tokens
           [(cons ":uni_minus" t) #:when (zero? pcount)
                    (Prim1 'sub (Int 0) (parse-expr-aux t))]
           [(cons "head" t) #:when (zero? pcount) (Prim2 'head (parse-expr-aux t))]
           [(cons "tail" t) #:when (zero? pcount) (Prim2 'tail (parse-expr-aux t))]
           [(cons "not" t) #:when (zero? pcount) (Prim2 'not (parse-expr-aux t))]
           [(cons "(" t) (parse-unary t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-unary t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-fundamental (reverse acc))]
           [(cons h t) (parse-unary t (cons h acc) pcount)]
           [_ (error "unary parse error")]))

(define (parse-fundamental tokens)
    (match tokens
           [(cons h t) #:when (is-int? h) (Int (string->number h))]
           [(cons h t) #:when (is-float? h) (Float (string->number h))]
           [(cons h t) #:when (is-string? h) (String (substring h 1 (sub1 (string-length h))))]
           [(cons h t) #:when (is-variable? h) (Var h)]
           [_ (error "fundamental parse error")]))
