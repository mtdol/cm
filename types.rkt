#lang racket
(require "error.rkt" "reserved-keywords.rkt")
(provide (all-defined-out))
(define error-id 4)

(define (is-string? v) (string? v))
(define (is-int? v) (and (not (flonum? v) ) (integer? v)))
(define (is-float? v) (flonum? v))
(define (is-bool? v) (and (is-int? v) (or (= v 1) (= v 0))))
(define (is-list? v) (list? v))
(define (is-pair? v) (pair? v))
(define (is-null? v) (null? v))

(define (get-type v)
  (cond 
    [(is-string? v) "string"]
    [(is-float? v) "float"]
    [(is-int? v) "int"]
    [(is-null? v) "null"]
    [(is-list? v) "list"]
    [(is-pair? v) "pair"]
    [else "unknown"]))

(define (string-to-number v) 
    (let ([v2 (string->number v)]) 
        (match v2
            [#f (cm-error error-id 
                    (string-append "String could not be coerced into a number.\n" 
                                      "The string was: " v))]
            [_ v2])))


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
         ["int" (number->string v)]
         ["float" (number->string v)]
         ["list" (list-to-string v)]
         ["pair" (list-to-string v)]
         ["null" "null"]
         [_ (cm-error error-id "String coersion error.")]))
(define (int-coerce v) 
  (match (get-type v) 
         ["string" (exact-floor (string-to-number v))]
         ["int" v]
         ["float" (exact-floor v)]
         ["list" (cm-error error-id "Cannot coerce a List to an int.")]
         ["pair" (cm-error error-id "Cannot coerce a Pair to an int.")]
         ["null" (cm-error error-id "Cannot coerce a Null to an int.")]
         [_ (cm-error error-id "Int coersion error.")]))
(define (float-coerce v) 
  (match (get-type v) 
         ["string" (+ (string-to-number v) 0.0)]
         ["int" (+ v 0.0)]
         ["float" v]
         ["list" (cm-error error-id "Cannot coerce a List to a float.")]
         ["pair" (cm-error error-id "Cannot coerce a Pair to a float.")]
         ["null" (cm-error error-id "Cannot coerce a Null to a float.")]
         [_ (cm-error error-id "Float coersion error.")]))

;; turns a value into a cm bool
;; zero int is the only false value in the language
(define (bool-coerce v) 
  (match (get-type v) 
         ["string" 1]
         ["int" (if (zero? v) 0 1)]
         ["float" 1]
         ["list" 1]
         ["pair" 1]
         ["null" 1]
         [#t 1]
         [#f 0]
         [_ (cm-error error-id "Bool coersion error.")]))

;; converts a cm bool to a racket bool
(define (bool-to-racket v) 
  (match v 
         [0 #f]
         [_ #t]))

;; converts racket true and false values into their cm equivalent
(define (racket-to-bool v) 
  (match v 
         [#f 0]
         [_ 1]))


;; token checkers
(define (is-int-token? v)
  (and (string->number v) (exact-integer? (string->number v))))
(define (is-float-token? v)
  (and (string->number v) (flonum? (string->number v))))
(define (is-string-token? v)
         (and (string? v) (string=? (substring v 0 1) "\"")))
(define (is-var-token? v) 
  (and (not (is-keyword? v)) (regexp-match? #rx"^[a-zA-Z_][a-zA-Z1-9_]*[?]*[']*$" v)))
