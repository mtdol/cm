#lang racket
(require "ast.rkt")
(provide interp)

(define (eval-string s) (eval (read (open-input-string s))))
(define (get-type v)
  (cond 
    [(string? v) "string"]
    [(flonum? v) "float"]
    [(integer? v) "int"]
    [(null? v) "null"]
    [else "unknown"]))

(define (string-coerce v) 
  (match (get-type v)
         ["string" v]
         ["null" ""]
         ["int" (number->string v)]
         ["float" (number->string v)]
         [_ (error "String coersion error.")]))
(define (int-coerce v) 
  (match (get-type v) 
         ["string" (exact-floor (string->number v))]
         ["null" 0]
         ["int" v]
         ["float" (exact-floor v)]
         [_ (error "Int coersion error.")]))
(define (float-coerce v) 
  (match (get-type v) ["string" (+ (string->number v) 0.0)]
         ["null" 0.0]
         ["int" (+ v 0.0)]
         ["float" v]
         [_ (error "Float coersion error.")]))


(define (interp ast)
  (match ast
        [(Prim1 op e1 e2) 
           (match op 
                  ['cons (cons (interp e1) (interp e2))]
                  ['and (and (interp e1) (interp e2))]
                  ['or (or (interp e1) (interp e2))]
                  ['cat (string-append (string-coerce (interp e1))
                                       (string-coerce (interp e2)))]
                  ['gt (> (interp e1) (interp e2))]
                  ['lt (< (interp e1) (interp e2))]
                  ['ge (>= (interp e1) (interp e2))]
                  ['le (<= (interp e1) (interp e2))]
                  ['equal (= (interp e1) (interp e2))]
                  ['add (+ (interp e1) (interp e2))]
                  ['sub (- (interp e1) (interp e2))]
                  ['mult (* (interp e1) (interp e2))]
                  ['div (/ (interp e1) (interp e2))]
                  ['mod (modulo (interp e1) (interp e2))]
                  ['exp (expt (interp e1) (interp e2))])]
        [(Prim2 op e) 
           (match op 
                  ['pos (interp e)]
                  ['neg (- (interp e))]
                  ['type (get-type (interp e))] 
                  ['string (string-coerce (interp e))] 
                  ['int (int-coerce (interp e))] 
                  ['float (float-coerce (interp e))] 
                  ['not (not (interp e))])]
        [(Print e) (print (interp e))] 
        [(Eval s) (eval-string (interp s))] 
        [(Int i) i]
        [(Float f) f]
        [(Null) null]
        [(String s) s]))
