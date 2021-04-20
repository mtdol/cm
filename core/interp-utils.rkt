#lang racket
(require racket/lazy-require
         cm/core/ast cm/core/types cm/core/error)
(lazy-require [cm/core/interp (trace-interp-expr)])
(provide fix-string ast-cons-to-racket apply-if-type
         assert-contract
         var-container? get-var-label check-var-string-name
         apply-if-type-1 assign-type-check check-types-list
         valid-against-schema?
         interp-def-list interp-lambda-list struct-schema->string)

;;
;; Utilities for interpretation
;;

(define (fix-string str) 
  (string-replace str "\\\"" "\""))

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
    (cm-error "CONTRACT" 
        (format "Attempted to apply ~a onto ~a." op-name (get-type arg1)))))
(define (apply-if-type-1 types op op-name arg)
  (if (member (get-type arg) types)
    (op arg)
    (cm-error "CONTRACT" 
        (format "Attempted to apply ~a onto ~a." op-name (get-type arg)))))

;; tells if the type matches for the given value
;; string, value, string -> bool
(define (types-match? type value)
  (if (string=? type "dynamic") #t 
  (let ([v-type (get-type value)]) 
    (or (string=? type v-type)
        ;; special case for pair value, since a list is technicaly a pair
        (and (string=? type "pair")
             (not (is-null? value))
             (string=? v-type "list")))
            )))


;; throws an exception if the given type and the type of the value do not match
;; returns true if types match
;;
;; string list, value, string -> void | exception
(define (assign-type-check types value id)
   (if (ormap (lambda (type) (types-match? type value)) types)
     (void)
     (cm-error "CONTRACT" 
       (if (= 1 (length types))
         (format "Received type \"~a\" for var ~a but expected ~a." 
               (get-type value) id (string-coerce (car types)))
         (format "Received type \"~a\" for var ~a but expected one of ~a." 
               (get-type value) id (string-coerce types))))))

(define (assert-contract types value label)
   (if (ormap (lambda (type) (types-match? type value)) types)
     (void)
     (cm-error "CONTRACT" 
       (if (= 1 (length types))
         (format "Received type \"~a\" for ~a but expected ~a.\nReceived: ~a" 
               (get-type value) label 
               (string-coerce (car types)) (string-coerce value))
         (format "Received type \"~a\" for ~a but expected one of ~a.\nReceived: ~a" 
               (get-type value) label 
               (string-coerce types) (string-coerce value))))))

;; checks that the types list is formated correctly and then yields it, else
;; throws an exception
;;
;; ? -> string list | error
(define (check-types-list types)
  (match types
         ['() (cm-error "CONTRACT" "Type arguments not present.")]
         ;; TODO check that args are lists
         [types-lst #:when (and (list? types-lst) 
                                (andmap (lambda (elem) (string? elem)) types-lst)) 
                    types-lst]
         [_ (cm-error "CONTRACT" "Type arguments to types must be a list of strings.")]))

(define (var-container? expr)
  (match expr
    [(Var _) #t]
    [(Prim1 'var _) #t]
    [_ #f]))

;; gets the label of a var
(define (get-var-label expr context module-id debug)
  (match expr
    [(Var label) label]
    [(Prim1 'var var) 
     (check-var-string-name 
       (trace-interp-expr var context module-id debug))]
    [_ #f]
    ))

;; checks that the argument to `var` is valid
(define (check-var-string-name v)
  (when (not (string? v))
    (cm-error "CONTRACT" "Argument to var must be a string."))
  (when (string=? v "")
    (cm-error "CONTRACT" "Argument to var must be a non-empty string."))
  (when (> (string-length v) (get-max-var-length))
    (cm-error 
      "CONTRACT" 
      (format "Argument to var must not exceed ~a characters." (get-max-var-length))))
  v)


;; checks if the given list matches the schema for the type
(define (valid-against-schema? label schemas vs)
 (let aux ([vs vs] [schemas schemas])
   (match vs
     ['() (null? schemas)]
     [(cons v vs) 
      (match schemas
        ['() #f]
        [(cons (SchemaElem types _) schemas)
         (if (is-types? types v) (aux vs schemas) #f)]
        ;; the schema should have been validated, so we will
        ;; only end up here if something is wrong
        [_ 
          (cm-error 
             "SYNTAX" 
                 (format "Struct instance declaration could not be understood. Struct ~a" label))])]
          )))

;; turns a (valid) struct schema to a string
(define (struct-schema->string schema)
  (match schema
    ['() "()"]
    [(cons (SchemaElem types label) '()) 
     (format "types ~a ~a;" types label)]
    [(cons (SchemaElem types label) t) 
     (string-append (format "types ~a ~a, " types label) 
                    (struct-schema->string t))]
    ))

;; converts a pair to a list if it wasn't already
(define (pair-to-list p)
  (match p
        ['() '()] ;; already a list
        [(cons h t) (cons h (pair-to-list t))]
        [h (cons h '())]))

;; repackages def x,y = 5 as def x = def y = 5
;; es = "x,y", rexpr = "5"
;; ast pair | ast, ast -> ast
(define (interp-def-list es rexpr)
  (match es
         [(Prim2 'cons e1 e2) (Def e1 (Assign (interp-def-list e2 rexpr)))]
         [e (Def e (Assign rexpr))]))
(define (interp-lambda-list es rexpr)
  (match es
         [(Prim2 'cons e1 e2) (Lambda e1 (Assign (interp-lambda-list e2 rexpr)))]
         [e (Lambda e (Assign rexpr))]))

;; Ensures that the subarguments given are all lists.
(define (check-list-arguments args op)
    (if (not (list? args))
        (cm-error "SYNTAX" (string-append "Arguments to " op " were not a list. "
                              "Perhaps you forgot a semicolon."))
    (let aux ([lst args])
        (match lst
            ['() '()]
            [(cons h t) #:when (not (list? h)) 
                (cm-error "SYNTAX" (string-append "All subarguments to " op 
                        " must be lists. "
                       "Perhaps you forgot a semicolon."))]
            [(cons h t) (cons h (aux t))]))))

(define (check-argument-dimensions args minargs maxargs op)
    (let aux ([lst args])
        (match lst
            ['() '()]
            [(cons h t) #:when 
              (or (> (length h) maxargs) (< (length h) minargs))
                (cm-error "CONTRACT"
                  (string-append "Incorrect number of subarguments to " op ". "
                       "Min number of args: " (number->string minargs) ". "
                       "Max number of args: " (number->string maxargs) ". "))]
            [(cons h t) (cons h (aux t))])))

