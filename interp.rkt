#lang racket
(require "ast.rkt" "error.rkt" "types.rkt")
(provide interp ast-to-expr)
(define error-id 3)

;; Matthew Dolinka
;; cm interpreter

(define (eval-string s) (eval (read (open-input-string s))))


;; converts a pair to a list if it wasn't already
;(define (pair-to-list p)
  ;(match p
        ;['() '()] ;; already a list
        ;[(cons h t) (cons h (pair-to-list t))]
        ;[h (cons h '())]))

;; Ensures that the subarguments given are all lists.
(define (check-list-arguments args op)
    (if (not (list? args))
        (cm-error error-id (string-append "Arguments to " op " were not a list. "
                              "Perhaps you forgot a semicolon."))
    (let aux ([lst args])
        {match lst
            ['() '()]
            [(cons h t) #:when (not (list? h)) 
                (cm-error error-id (string-append "All subarguments to " op 
                        " must be lists. "
                       "Perhaps you forgot a semicolon."))]
            [(cons h t) (cons h (aux t))]})))

(define (check-argument-dimensions args minargs maxargs op)
    (let aux ([lst args])
        {match lst
            ['() '()]
            [(cons h t) #:when 
              (or (> (length h) maxargs) (< (length h) minargs))
                (cm-error error-id
                  (string-append "Incorrect number of subarguments to " op ". "
                       "Min number of args: " (number->string minargs) ". "
                       "Max number of args: " (number->string maxargs) ". "))]
            [(cons h t) (cons h (aux t))]}))

(define (interp ast)
  (match ast
        [(Prim1 op e1 e2) (interp-prim1 op (interp e1) (interp e2))]
        [(Prim2 op e) (interp-prim2 op (interp e))]
        [(If e1 e2 e3) (if (bool-to-racket (interp e1)) (interp e2) (interp e3))] 
        [(Cond e1 e2) 
            (let ([v1 (interp e1)] [v2 (interp e2)])
                    { let aux ([args (check-argument-dimensions (check-list-arguments v1 "cond") 2 2 "cond")]) 
                           (match args
                                  ['() v2]
                                  [(cons h t) #:when (bool-to-racket (car h)) (cadr h)]
                                  [(cons h t) (aux t)])})] 
        [(Print e) 
          (let ([v (interp e)])
             (match (cons (displayln (string-append (string-coerce v))) v)
                    [(cons _ res) res]))] 
        [(Error e) (error (string-coerce (interp e)))] 
        [(Eval s) (eval-string (string-coerce (interp s)))] 
        [(Int i) i]
        [(Float f) f]
        [(Null) null]
        [(String s) s]))

(define (interp-prim1 op v1 v2)
  (match op
        ['cons (cons v1 v2)]
        ['and (racket-to-bool (and (bool-to-racket v1) (bool-to-racket v2)))]
        ['or (racket-to-bool (or (bool-to-racket v1) (bool-to-racket v2)))]
        ['xor (racket-to-bool (xor (bool-to-racket v1) (bool-to-racket v2)))]
        ['cat (string-append (string-coerce v1)
                           (string-coerce v2))]
        [op #:when (not (string=? (get-type v1) (get-type v2)))
          (cm-error error-id (string-append "Operands of " (symbol->string op)
                " are not of the same type. Given " (get-type v1)
                ", " (get-type v2) "."))]
        ['gt (racket-to-bool (> v1 v2))]
        ['lt (racket-to-bool (< v1 v2))]
        ['ge (racket-to-bool (>= v1 v2))]
        ['le (racket-to-bool (<= v1 v2))]
        ['equal (racket-to-bool (= v1 v2))]
        ['add (+ v1 v2)]
        ['sub (- v1 v2)]
        ['mult (* v1 v2)]
        ['div (/ v1 v2)]
        ['mod (modulo v1 v2)]
        ['exp (expt v1 v2)]))
 
(define (interp-prim2 op v)
  (match op
        ['pos  #:when (or (string=? (get-type v) "int" ) 
                        (string=? (get-type v) "float"))
        (+ v)]
        ['pos (cm-error error-id (string-append 
            "Applied positive to non number. Given type "
            (get-type v) "."))]
        ['neg  #:when (or (string=? (get-type v) "int" ) 
                        (string=? (get-type v) "float"))
        (- v)]
        ['neg (cm-error error-id (string-append 
            "Applied negative to non number. Given type "
            (get-type v) "."))]
        ['head  #:when (or (string=? (get-type v) "list" ) 
                        (string=? (get-type v) "pair"))
        (car v)]
        ['head (cm-error error-id (string-append 
            "Applied head to non list or pair. Given type "
            (get-type v) "."))]
        ['tail  #:when (or (string=? (get-type v) "list" ) 
                        (string=? (get-type v) "pair"))
        (cdr v)]
        ['tail (cm-error error-id (string-append 
            "Applied tail to non list or pair. Given type "
            (get-type v) "."))]
        ['length  #:when (or (string=? (get-type v) "list")
                           (string=? (get-type v) "null")) 
        (length v)]
        ['length  #:when (string=? (get-type v) "string" ) 
        (string-length v)]
        ['length (cm-error error-id (string-append 
            "Applied tail to non list or string. Given type "
            (get-type v) "."))]
        ['type (get-type v)] 
        ['string (string-coerce v)] 
        ['int (int-coerce v)] 
        ['float (float-coerce v)] 
        ['bool (bool-coerce v)] 
        ['string? (racket-to-bool (is-string? v))] 
        ['int? (racket-to-bool (is-int? v))] 
        ['float? (racket-to-bool (is-float? v))] 
        ['bool? (racket-to-bool (is-bool? v))] 
        ['list? (racket-to-bool (is-list? v))] 
        ['pair? (racket-to-bool (is-pair? v))] 
        ['null? (racket-to-bool (is-null? v))] 
        ['not (racket-to-bool (not (bool-to-racket v)))]))


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
           [(Cond e1 e2) (flatten
                               (list "(" "cond " (ast-to-expr-aux e1)
                                  " else " (ast-to-expr-aux e2) ")"))]
           [(Print e) (flatten (list "(" "print " (ast-to-expr-aux e) ")"))]
           [(Format e1 e2) (flatten (list "(" "format " 
                            (ast-to-expr-aux e1) " with " (ast-to-expr-aux e2) ")"))]
           [(Match e1 e2) (flatten (list "(" "match " 
                            (ast-to-expr-aux e1) " with " (ast-to-expr-aux e2) ")"))]
           [(Splice e1 e2) (flatten (list "(" "index " 
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
