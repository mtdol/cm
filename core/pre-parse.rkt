#lang racket
;(require "error.rkt" "operators.rkt" "parse-auxiliaries.rkt")
(require cm/core/error cm/core/operators cm/core/parse-auxiliaries)
(provide pre-parse-expr)
(define error-id 2)

;; this file is for performing adjustments to the token list, so it
;; will be parsable by the main parser

;; prepares tokens for parsing
(define (pre-parse-expr tokens)
  (replace-assign-commas
  (replace-assign-equal
  (replace-unary-plus-minus
  ;(check-missing-operator
  (check-balanced-parens 
        tokens)))));)

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


;; lam x,y = x + y -> lam x = lam y = x + y
;; def x,y = 1 + 5 -> def x = def y = 1 + 5
(define (replace-assign-commas lst)
  (match lst
        ['() '()]
        [(cons h1 (cons h2 (cons "cons" t))) 
            #:when (or (string=? h1 "def") (string=? h1 "lambda"))
            (cons h1 (cons h2 (cons ":assign1"
                (replace-assign-commas (cons h1 t)))))]
        ;; def int x, y case
        [(cons h1 (cons h2 (cons h3 (cons "cons" t))))
            #:when (or (string=? h1 "def") (string=? h1 "lambda"))
            (cons h1 (cons h2 (cons h3 (cons ":assign1"
                (replace-assign-commas (cons h1 t))))))]
        [(cons h t) (cons h (replace-assign-commas t))]))

;; walks down the list, conses, and returns the list back with the first
;; instance of target changed to replacement
(define (replace-first lst target replacement)
  (match lst
         ['() (cm-error error-id
                        "Improperly formed assignment.")]
         [(cons h t) #:when (string=? h target)
                (cons replacement t)]
         [(cons h t) (cons h (replace-first t target replacement))]))

;; def x equal 2 -> def x :assign1 2
;; let x equal 2 in x + y -> let x :assign2 2 in x + y 
(define (replace-assign-equal lst)
  (match lst
        ['() '()]
        [(cons h t) #:when (or (string=? "values" h) (string=? "let" h)) 
            (cons h
                (replace-assign-equal (replace-first t "equal" ":assign2")))]
        [(cons h t) #:when (or (string=? "def" h) (string=? "lambda" h)) 
            (cons h
                (replace-assign-equal (replace-first t "equal" ":assign1")))]
        [(cons h t) (cons h (replace-assign-equal t))]))
