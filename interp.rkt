#lang racket
(require "ast.rkt")
(provide interp ast-to-expr)

;; Matthew Dolinka

;; cm interpreter

(define (eval-string s) (eval (read (open-input-string s))))
(define (get-type v)
  (cond 
    [(string? v) "string"]
    [(flonum? v) "float"]
    [(integer? v) "int"]
    [(null? v) "null"]
    [(list? v) "list"]
    [(cons? v) "pair"]
    [else "unknown"]))

(define (string-to-number v) (string->number v))


;; turns a list or pair into a string
(define (list-to-string lst) 
    (match lst
           ['() "null"]
           [(cons h '()) (string-append (string-coerce h) ", " (list-to-string '()))]
           [(cons h t) (string-append (string-coerce h) ", " (list-to-string t))]
           [h (string-coerce h)]))

(define (string-coerce v) 
  (match (get-type v)
         ["string" v]
         ["null" ""]
         ["int" (number->string v)]
         ["float" (number->string v)]
         ["list" (list-to-string v)]
         ["pair" (list-to-string v)]
         [_ (error "String coersion error.")]))
(define (int-coerce v) 
  (match (get-type v) 
         ["string" (exact-floor (string-to-number v))]
         ["null" 0]
         ["int" v]
         ["float" (exact-floor v)]
         ["list" (error "Cannot coerce a List to an int.")]
         ["pair" (error "Cannot coerce a Pair to an int.")]
         [_ (error "Int coersion error.")]))
(define (float-coerce v) 
  (match (get-type v) ["string" (+ (string-to-number v) 0.0)]
         ["null" 0.0]
         ["int" (+ v 0.0)]
         ["float" v]
         ["list" (error "Cannot coerce a List to a float.")]
         ["pair" (error "Cannot coerce a Pair to a float.")]
         [_ (error "Float coersion error.")]))
;; turns a value into a cm bool
(define (bool-coerce v) 
  (match (get-type v) 
         ["string" 1]
         ["null" 0]
         ["int" (if (zero? v) 0 1)]
         ["float" (if (zero? v) 0 1)]
         ["list" 1]
         ["pair" 1]
         [#t 1]
         [#f 0]
         [_ (error "Bool coersion error.")]))

;; converts a cm bool to a racket bool
(define (bool-to-racket v) 
  (match v 
         [0 #f]
         [0.0 #f]
         [v #:when (null? v) #f]
         [_ #t]))

;; converts racket true and false values into their cm equivalent
(define (racket-to-bool v) 
  (match v 
         [#f 0]
         [_ 1]))

(define (interp ast)
  (match ast
        [(Prim1 op e1 e2) (let ([e1-2 (interp e1)] [e2-2 (interp e2)])
           (match op 
                  ['cons (cons e1-2 e2-2)]
                  ['and (racket-to-bool (and (bool-to-racket e1-2) (bool-to-racket e2-2)))]
                  ['or (racket-to-bool (or (bool-to-racket e1-2) (bool-to-racket e2-2)))]
                  ['cat (string-append (string-coerce e1-2)
                                       (string-coerce e2-2))]
                  [op #:when (not (string=? (get-type e1-2) (get-type e2-2)))
                      (error (string-append "Error: Operands of " (symbol->string op)
                            " are not of the same type. Given " (get-type e1-2)
                            ", " (get-type e2-2) "."))]
                  ['gt (racket-to-bool (> e1-2 e2-2))]
                  ['lt (racket-to-bool (< e1-2 e2-2))]
                  ['ge (racket-to-bool (>= e1-2 e2-2))]
                  ['le (racket-to-bool (<= e1-2 e2-2))]
                  ['equal (racket-to-bool (= e1-2 e2-2))]
                  ['add (+ e1-2 e2-2)]
                  ['sub (- e1-2 e2-2)]
                  ['mult (* e1-2 e2-2)]
                  ['div (/ e1-2 e2-2)]
                  ['mod (modulo e1-2 e2-2)]
                  ['exp (expt e1-2 e2-2)]))]
        [(Prim2 op e) (let ([e-2 (interp e)])
           (match op 
                  ['pos  #:when (or (string=? (get-type e-2) "int" ) 
                                    (string=? (get-type e-2) "float"))
                    (+ e-2)]
                  ['pos (error (string-append 
                        "Error: Applied positive to non number. Given type "
                        (get-type e-2) "."))]
                  ['neg  #:when (or (string=? (get-type e-2) "int" ) 
                                    (string=? (get-type e-2) "float"))
                    (- e-2)]
                  ['neg (error (string-append 
                        "Error: Applied negative to non number. Given type "
                        (get-type e-2) "."))]
                  ['head  #:when (or (string=? (get-type e-2) "list" ) 
                                    (string=? (get-type e-2) "pair"))
                    (car e-2)]
                  ['head (error (string-append 
                        "Error: Applied head to non list or pair. Given type "
                        (get-type e-2) "."))]
                  ['tail  #:when (or (string=? (get-type e-2) "list" ) 
                                    (string=? (get-type e-2) "pair"))
                    (cdr e-2)]
                  ['tail (error (string-append 
                        "Error: Applied tail to non list or pair. Given type "
                        (get-type e-2) "."))]
                  ['length  #:when (or (string=? (get-type e-2) "list")
                                       (string=? (get-type e-2) "null")) 
                    (length e-2)]
                  ['length  #:when (string=? (get-type e-2) "string" ) 
                    (string-length e-2)]
                  ['length (error (string-append 
                        "Error: Applied tail to non list or string. Given type "
                        (get-type e-2) "."))]
                  ['type (get-type e-2)] 
                  ['string (string-coerce e-2)] 
                  ['int (int-coerce e-2)] 
                  ['float (float-coerce e-2)] 
                  ['bool (bool-coerce e-2)] 
                  ['not (racket-to-bool (not (bool-to-racket e-2)))]))]
        [(If e1 e2 e3) (if (bool-to-racket (interp e1)) (interp e2) (interp e3))] 
        [(Print e) (display (string-append (string-coerce (interp e)) "\n"))] 
        [(Error e) (error (string-coerce (interp e)))] 
        [(Eval s) (eval-string (string-coerce (interp s)))] 
        [(Int i) i]
        [(Float f) f]
        [(Null) null]
        [(String s) s]))


;; returns the ast back with proper parenthesis
(define (ast-to-expr ast) (apply string-append (ast-to-expr-aux ast)))
(define (ast-to-expr-aux ast) 
    (match ast
           [(Int i) (list (number->string i))]
           [(Float f) (list (number->string f))]
           [(String s) (list "\"" s "\"")]
           [(Var v) (list v)]
           [(Null) "null"]
           [(Prim1 op e1 e2) #:when (symbol=? op 'cons)
                              (flatten (list "(" (ast-to-expr-aux e1)
                                     ", " (ast-to-expr-aux e2) ")"))]
           [(Prim1 op e1 e2) (flatten
                               (list "(" (ast-to-expr-aux e1)
                                     " " (symbol->string (translate-op-name op))
                                     " " (ast-to-expr-aux e2) ")"))]
           [(Prim2 op e) (flatten
                               (list "(" (symbol->string (translate-op-name op))
                                     " (" (ast-to-expr-aux e) "))"))]
           [(Def v e) (list "(" "def " (ast-to-expr v) " = "
                                (ast-to-expr e) ")")]
           [(Let v e1 e2) (list "(" "let " (ast-to-expr v) " = "
                                (ast-to-expr e1) " in " (ast-to-expr e2) ")")]
           [(Values e1 e2 e3) (list "(" "values " (ast-to-expr e1) " = "
                                (ast-to-expr e2) " in " (ast-to-expr e3) ")")]
           [(Lambda v e) (flatten (list "(" "lambda "
                        (ast-to-expr-aux v) " = " (ast-to-expr-aux e) ")"))]
           [(If e1 e2 e3) (flatten
                               (list "(" "if " (ast-to-expr-aux e1)
                                  " then " (ast-to-expr-aux e2) 
                                  " else " (ast-to-expr-aux e3) ")"))]
           [(Print e) (flatten (list "(" "print " (ast-to-expr-aux e) ")"))]
           [(Format e1 e2) (flatten (list "(" "format " 
                            (ast-to-expr-aux e1) " with " (ast-to-expr-aux e2) ")"))]
           [(Match e1 e2) (flatten (list "(" "match " 
                            (ast-to-expr-aux e1) " with " (ast-to-expr-aux e2) ")"))]
           [(Index e1 e2) (flatten (list "(" "index " 
                            (ast-to-expr-aux e1) " with " (ast-to-expr-aux e2) ")"))]
           [(Error s) (flatten (list "(" "error " (ast-to-expr-aux s) ")"))]
           [(Stat e st) (list (ast-to-expr e) ". " (ast-to-expr st))]
           [(EOP) (list)]))

;; translates op names if necessary to the form used in the language
(define (translate-op-name op) 
  (match op
    ['add 'plus]
    ['pos 'plus]
    ['sub 'minus]
    ['neg 'minus]
    ['apply ':]
    [op op]))
