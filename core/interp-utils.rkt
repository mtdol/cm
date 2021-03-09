#lang racket
(require cm/core/ast cm/core/types cm/core/error)
(provide eval-string ast-cons-to-racket apply-if-type
         apply-if-type-1 assign-type-check valid-against-schema?)
(define error-id 3)

;;
;; Utilities for interpretation
;;

(define (eval-string s) (eval (read (open-input-string s))))

;; turns an ast cons node(s) into a racket pair, list
(define (ast-cons-to-racket node)
  (match node
         [(Prim2 'cons a b) (cons (ast-cons-to-racket a) (ast-cons-to-racket b))]
         [(Null) '()]
         [a a]))

;; applies the op to the given arg list if the type of arg1 matches type
(define (apply-if-type types op op-name arg1 arg2)
  (if (member (get-type arg1) types)
    (op arg1 arg2)
    (cm-error error-id 
        (format "Attempted to apply ~a onto ~a." op-name (get-type arg1)))))
(define (apply-if-type-1 types op op-name arg)
  (if (member (get-type arg) types)
    (op arg)
    (cm-error error-id 
        (format "Attempted to apply ~a onto ~a." op-name (get-type arg)))))

;; throws an exception if the given type and the type of the value do not match
;; returns true if types match
(define (assign-type-check type value var)
  (if (string=? type "dynamic") #t 
  (let ([v-type (get-type value)]) 
    (if (or (string=? type v-type)
            ;; exceptions
            ;; a null is a list
            (and (string=? type "list") (string=? v-type "null"))
            ;; a non-null list is a pair
            (and (string=? type "pair") (string=? v-type "list")))
      #t
      (cm-error error-id 
        (format "Recieved type ~a for var ~a but expected ~a." v-type var type))))))

;; checks if the given list matches the schema for the type
(define (valid-against-schema? label schema lst)
  ;(let ([schema (get-type-data label)])
    (let aux ([lst lst] [schema-lst schema])
      (match lst
             ['() (null? schema-lst)]
             [(cons h t) 
              (match schema-lst
                     ['() #f]
                     [(cons (Var v) schema-t) (aux t schema-t)]
                     [(cons (Prim1 'dynamic (Var v)) schema-t) (aux t schema-t)]
                     [(cons (Prim1 'int (Var v)) schema-t)
                      (if (is-int? h) (aux t schema-t) #f)]
                     [(cons (Prim1 'float (Var v)) schema-t)
                      (if (is-float? h) (aux t schema-t) #f)]
                     [(cons (Prim1 'bool (Var v)) schema-t)
                      (if (is-bool? h) (aux t schema-t) #f)]
                     [(cons (Prim1 'string? (Var v)) schema-t)
                      (if (is-string? h) (aux t schema-t) #f)]
                     [(cons (Prim1 'pair (Var v)) schema-t)
                      (if (is-pair? h) (aux t schema-t) #f)]
                     [(cons (Prim1 'list (Var v)) schema-t)
                      (if (is-list? h) (aux t schema-t) #f)]
                     [(cons (Prim1 'fun (Var v)) schema-t)
                      (if (is-fun? h) (aux t schema-t) #f)]
                     [(cons (Prefix2 'struct (Var label) (Var v)) schema-t)
                      (match h
                             [(CmStruct l2 _) #:when (string=? l2 label) (aux t schema-t)]
                             [_ #f])]
                      ;; the schema should have been validated, so we will
                      ;; only end up here if something is wrong
                      [_ (cm-error error-id (format "Struct instance declaration could not be understood. Struct ~a" label))])]
             )))

;; converts a pair to a list if it wasn't already
(define (pair-to-list p)
  (match p
        ['() '()] ;; already a list
        [(cons h t) (cons h (pair-to-list t))]
        [h (cons h '())]))

;; Ensures that the subarguments given are all lists.
(define (check-list-arguments args op)
    (if (not (list? args))
        (cm-error error-id (string-append "Arguments to " op " were not a list. "
                              "Perhaps you forgot a semicolon."))
    (let aux ([lst args])
        (match lst
            ['() '()]
            [(cons h t) #:when (not (list? h)) 
                (cm-error error-id (string-append "All subarguments to " op 
                        " must be lists. "
                       "Perhaps you forgot a semicolon."))]
            [(cons h t) (cons h (aux t))]))))

(define (check-argument-dimensions args minargs maxargs op)
    (let aux ([lst args])
        (match lst
            ['() '()]
            [(cons h t) #:when 
              (or (> (length h) maxargs) (< (length h) minargs))
                (cm-error error-id
                  (string-append "Incorrect number of subarguments to " op ". "
                       "Min number of args: " (number->string minargs) ". "
                       "Max number of args: " (number->string maxargs) ". "))]
            [(cons h t) (cons h (aux t))])))

