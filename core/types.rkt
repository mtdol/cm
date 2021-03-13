#lang racket
(require cm/core/error cm/core/reserved-keywords cm/core/ast cm/core/context)
(provide (all-defined-out))

(struct CmStruct (label args))
;; all types used in guards minus structs
(define guard-types '(int float string bool list pair fun dynamic))

;; value -> bool
(define (is-string? v) (string? v))
(define (is-int? v) (and (not (flonum? v) ) (integer? v)))
(define (is-float? v) (flonum? v))
(define (is-bool? v) (match v [(Bool _) #t] [_ #f]))
(define (is-list? v) (list? v))
(define (is-pair? v) (pair? v))
(define (is-null? v) (null? v))
(define (is-fun? v) (match v [(Fun _ _ _ _) #t] [_ #f]))
(define (is-struct? v) (match v [(CmStruct _ _) #t] [_ #f]))
(define (is-void? v) (match v [(Prim0 'void) #t] [_ #f]))

;; yields true if v is a struct with the same label as label1
;; string, value -> bool
(define (is-struct-type? label1 v) 
  (match v [(CmStruct label2 _) (string=? label1 label2)] [_ #f]))

;; string, value -> bool
(define (is-type? type value)
  (let ([vtype (get-type value)])
    (or (string=? type "dynamic") (string=? vtype type))))

;; gives the type string for a label of a struct
;; string -> string
(define (get-struct-type-string label)
    (format "struct ~a" label))


;; true if the expr is a list, false otherwise
;; ast -> bool
(define (expr-is-list? e)
  (match e
         [(Prim2 'cons h t) (expr-is-list? t)]
         [(Null) #t]
         [_ #f]))

;; value -> string
(define (get-type v)
  (cond 
    [(is-string? v) "string"]
    [(is-float? v) "float"]
    [(is-int? v) "int"]
    [(is-bool? v) "bool"]
    [(is-null? v) "null"]
    [(is-list? v) "list"]
    [(is-pair? v) "pair"]
    [(is-fun? v) "fun"]
    [(is-struct? v) (match v [(CmStruct label _) (get-struct-type-string label)])]
    [(is-void? v) "void"]
    [else "unknown"]))

;; checks if the values and subvalues of the two args represent the same thing
(define (deep-equal? v1 v2)
  (match (cons v1 v2)
         [(cons (CmStruct lab lst1) (CmStruct lab lst2)) (deep-equal? lst1 lst2)]
         [(cons (Bool i) (Bool i)) #t]
         [(cons (cons h1 t1) (cons h2 t2)) (and (deep-equal? h1 h2) (deep-equal? t1 t2))]
         ;; otherwise punt to the more conservative equal? proc
         [_ (equal? v1 v2)]))

(define (string-to-number v) 
    (let ([v2 (string->number v)]) 
        (match v2
            [#f (cm-error "GENERIC" 
                    (string-append "String could not be coerced into a number.\n" 
                                      "The string was: " v))]
            [_ v2])))


;; turns a list or pair into a string
(define (list-to-string lst) 
    (match lst
           ['() "()"]
           [(cons h '()) (string-append (string-coerce h) ";")]
           [(cons h t) (string-append (string-coerce h) ", " (list-to-string t))]
           [h (string-coerce h)]))

(define (string-coerce v) 
  (cond 
        [(is-struct? v) 
         (match v
                [(CmStruct label vals) (format "(struct ~a ~a)" label (string-coerce vals))]
                )]
    [else
  (match (get-type v)
         ["string" v]
         ["int" (number->string v)]
         ["float" (number->string v)]
         ["bool" (match v [(Bool 1) "true"] [(Bool 0) "false"])]
         ["list" (string-append "(" (list-to-string v) ")")]
         ["pair" (string-append "(" (list-to-string v) ")")]
         ["null" "()"]
         ["void" ""]
         ["fun" (match v [(Fun var type context expr)
                          (format "Fun ~a ~a" type var)])]
         [_ (cm-error "GENERIC" "String coersion error.")])]))
(define (int-coerce v) 
  (cond [(is-struct? v) (cm-error "CONTRACT" "Cannot coerce a struct to an int.")]
    [else
  (match (get-type v) 
         ["string" (exact-floor (string-to-number v))]
         ["int" v]
         ["float" (exact-floor v)]
         ["bool" (match v [(Bool i) i])]
         ["list" (cm-error "CONTRACT" "Cannot coerce a list to an int.")]
         ["pair" (cm-error "CONTRACT" "Cannot coerce a pair to an int.")]
         ["null" (cm-error "CONTRACT" "Cannot coerce a null to an int.")]
         ["void" (cm-error "CONTRACT" "Cannot coerce a void to an int.")]
         ["fun" (cm-error "CONTRACT" "Cannot coerce a fun to an int.")]
         [_ (cm-error "GENERIC" "Int coersion error.")])]))
(define (float-coerce v) 
  (cond [(is-struct? v) (cm-error "CONTRACT" "Cannot coerce a struct to a float.")]
    [else
  (match (get-type v) 
         ["string" (+ (string-to-number v) 0.0)]
         ["int" (+ v 0.0)]
         ["float" v]
         ["bool" (match v [(Bool 1) 1.0] [(Bool 0) 0.0])]
         ["list" (cm-error "CONTRACT" "Cannot coerce a list to a float.")]
         ["pair" (cm-error "CONTRACT" "Cannot coerce a pair to a float.")]
         ["null" (cm-error "CONTRACT" "Cannot coerce a null to a float.")]
         ["void" (cm-error "CONTRACT" "Cannot coerce a void to a float.")]
         ["fun" (cm-error "CONTRACT" "Cannot coerce a fun to a float.")]
         [_ (cm-error "GENERIC" "Float coersion error.")])]))

;; turns a value into a cm bool
;; zero int is the only false value in the language
(define (bool-coerce v) 
  (cond [(is-struct? v) (cm-error "CONTRACT" "Cannot coerce a struct to a bool.")]
    [else
  (match (get-type v) 
         ["string" (Bool 1)]
         ["int" (if (zero? v) (Bool 0) (Bool 1))]
         ["float" (if (zero? v) (Bool 0) (Bool 1))]
         ["bool" v]
         ["list" (Bool 1)]
         ["pair" (Bool 1)]
         ["null" (Bool 1)]
         ["void" (cm-error "CONTRACT" "Cannot coerce a void to a bool")]
         ["fun" (cm-error "CONTRACT" "Cannot coerce a fun to a bool.")]
         [#t (Bool 1)]
         [#f (Bool 0)]
         [_ (cm-error "GENERIC" "Bool coersion error.")])]))

;; converts a cm bool to a racket bool
(define (bool-to-racket v) 
  (match v 
         [(Bool 0) #f]
         [_ #t]))

;; converts racket true and false values into their cm equivalent
(define (racket-to-bool v) 
  (match v 
         [#f (Bool 0)]
         [_ (Bool 1)]))

;; token checkers
(define (is-int-token? v)
  (and (string->number v) (exact-integer? (string->number v))))
(define (is-float-token? v)
  (and (string->number v) (flonum? (string->number v))))
(define (is-string-token? v)
         (and (string? v) (string=? (substring v 0 1) "\"")))
(define (is-bool-token? v)
         (or (string=? "true") (string=? "false")))
(define (is-var-token? v) 
  (and (not (is-keyword? v)) (regexp-match? #rx"^[a-zA-Z_][a-zA-Z1-9_]*[?!]?$" v)))
