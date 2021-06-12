#lang racket
(require racket/lazy-require racket/hash
         cm/core/ast cm/core/types cm/core/error)
(lazy-require [cm/core/interp (trace-interp-expr interp-apply)])
(provide fix-string ast-cons-to-racket apply-if-type
         assert-contract
         var-container? get-var-label check-var-string-name
         apply-if-type-1 assign-type-check check-types-list
         valid-against-schema?
         value->list value-length
         interp-lambda-list
         struct-schema->string
         merge-contexts)

;; Matthew Dolinka
;;

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
        (format "Attempted to apply \"~a\" onto \"~a\"" op-name (get-type arg1)))))
(define (apply-if-type-1 types op op-name arg)
  (if (member (get-type arg) types)
    (op arg)
    (cm-error "CONTRACT" 
        (format "Attempted to apply \"~a\" onto \"~a\"" op-name (get-type arg)))))

;; tells if the type matches for the given value
;; (string | fun), value -> bool
(define (guard-type-matches? type value params)
  (match type
    ;; type is a function guard
    [(? is-fun?) 
     (let ([res (interp-apply type value params)])
     (unless (is-bool? res)
       (cm-error 
         "CONTRACT" 
         (format "Value returned by guard must be bool,\ngot: ~a" res)))
     (bool-to-racket res))]
    ["dynamic" #t]
    [_
     (let ([v-type (get-type value)]) 
        (or (string=? type v-type)
        ;; special case for pair value, since a list is technicaly a pair
          (and (string=? type "pair")
             (not (is-null? value))
             (string=? v-type "list")))
            )
      ])

  )
(define (guard-types-match? types value params)
  (ormap (lambda (type) (guard-type-matches? type value params)) types))



(define (assign-type-check types value params label) 
  (assert-contract types value params (string-append "var " label)))

;; throws an exception if the given type and the type of the value do not match
;;
;; string list, value, string -> void | exception
(define (assert-contract types value params label)
   (if (guard-types-match? types value params)
     (void)
     (cm-error "CONTRACT" 
      (match types
        [(list (? is-fun?))
         (format "Received type \"~a\" for ~a but expected a value matching ~a.\nReceived: ~a" 
               (get-type value) label 
               (string-coerce (car types)) (string-coerce value))]
        [_ 
         #:when (= 1 (length types))
         (format "Received type \"~a\" for ~a but expected ~a.\nReceived: ~a" 
               (get-type value) label 
               (string-coerce (car types)) (string-coerce value))]
        [_ 
         (format "Received type \"~a\" for ~a but expected one of ~a.\nReceived: ~a" 
               (get-type value) label 
               (string-coerce types) (string-coerce value))])
         )))

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
(define (get-var-label expr context params module-id debug)
  (match expr
    [(Var label) label]
    [(Prim1 'var var) 
     (check-var-string-name 
       (trace-interp-expr var context params module-id debug))]
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
(define (valid-against-schema? label schemas vs params)
 (let aux ([vs vs] [schemas schemas])
   (match vs
     ['() (null? schemas)]
     [(cons v vs) 
      (match schemas
        ['() #f]
        [(cons (SchemaElem types _) schemas)
         (if (guard-types-match? types v params) (aux vs schemas) #f)]
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
    [(cons (SchemaElem (list (? is-fun? func)) label) '()) 
     (format "? ~a ~a;" label (string-coerce func))]
    [(cons (SchemaElem (list (? is-fun? func)) label) t) 
     (string-append (format "? ~a ~a, " label (string-coerce func)) 
                    (struct-schema->string t))]
    [(cons (SchemaElem types label) '()) 
     (format "types ~a ~a;" types label)]
    [(cons (SchemaElem types label) t) 
     (string-append (format "types ~a ~a, " types label) 
                    (struct-schema->string t))]
    ))

;; follows down a set of pairs and appends a null at the tail
(define (value->list vs)
  (match vs
        [(cons v vs) (cons v (value->list vs))]
        [v (cons v '())]))

;; returns the length of a incomplete list
(define (value-length vs)
  (let aux ([vs vs] [num 1])
    (match vs
          [(cons v vs) (aux vs (add1 num))]
          [v num])))

;; repackages \x,y -> x-y as \x -> \y -> x-y
;; es = "x,y", rexpr = "x-y"
;; ast pair | ast, ast -> ast
(define (interp-lambda-list es rexpr)
  (match es
         [(Prim2 'cons e1 e2) (Lambda e1 (Yields (interp-lambda-list e2 rexpr)))]
         [e (Lambda e (Yields rexpr))]))

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


(define (merge-contexts c1 c2)
  (hash-union 
    c1 c2
    #:combine/key (lambda (k v1 v2) v2)))
