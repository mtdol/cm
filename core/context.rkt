#lang racket
(require cm/core/error)
(provide (all-defined-out))

(struct ContextEntry (value))
(struct TypeEntry (value))
;; what a lambda expr yields
(struct Fun (var type context expr))

(define global-context (make-hash))
(define global-types (make-hash))

(define (set-global-var! var value) 
  (hash-set! global-context var (ContextEntry value)))

(define (get-global-var-data var)
        (match (hash-ref global-context var (lambda () #f))
               [#f #f]
               [res (match res [(ContextEntry data) data])]))

(define (set-type! type schema)
  (hash-set! global-types type (TypeEntry schema)))

(define (get-type-data type)
        (match (hash-ref global-types type (lambda () #f))
               [#f #f]
               [res (match res [(TypeEntry data) data])]))

(define (type-exists? type)
  (hash-has-key? global-types type))



;; local getters, setters

;; returns a hash
(define (set-local-var var value context) 
  (hash-set context var (ContextEntry value)))

(define (get-local-var-data var context)
        (match (hash-ref context var (lambda () #f))
               [#f #f]
               [res (match res [(ContextEntry data) data])]))


;;
;; Macro context
;;

;; print the macro-context (for debugging)
(define (print-macro-context)
  (let aux ([ms (hash->list macro-context)])
    (match ms
        ['() (void)]
        [(cons (cons label entries) t) 
         (display (format "label: ~a\n" label))
         (print-macro-entries entries)
         (displayln "")
         (aux t)]
    )))

(define (print-macro-entries es)
  (match es
         ['() (void)]
         [(cons (MacroEntry vars body) t)
          (display (format "vars:\n~a\nbody:\n~a\n" vars body))
          (print-macro-entries t)]))

(struct MacroEntry (vars body))
(define macro-context (make-hash))

;; clean redefine the macro
(define (set-macro! label vars value) 
  (hash-set! macro-context label (list (MacroEntry (check-macro-vars vars) value))))

;; append onto the macros definitions
(define (append-to-macro! label vars value)
  (if (hash-has-key? macro-context label)
      (hash-set! macro-context label 
                 (append (hash-ref macro-context label) 
                         (list (MacroEntry (check-macro-vars vars) value))))
      (set-macro! label vars value)
                 ))

(define (get-macro-defs label)
        (match (hash-ref macro-context label (lambda () #f))
               [#f #f]
               [res res]))

;; checks that macro vars are well formed
;;
;; string list -> string list | error
(define (check-macro-vars vars)
  (match vars
         ['() '()]
         ;; vars is valid if REST only appears at the end
         [vs #:when (not (member "REST" (cdr (reverse vars)))) vs]
         [vs (cm-error "SYNTAX" (format "Invalid Macro vars: ~a" vs))]))


;; string, string list list -> string list
(define (apply-macro label args) 
  (let aux ([entry (get-macro-defs label)])
    (match entry
           [#f (cm-error "MACRO" (format "Macro not defined: ~a" label))]
           ['() (cm-error "MACRO" (format "No matching rule for macro with label: ~a" label))]
           [(cons (MacroEntry vars body) t) 
            #:when (args-match-macro-entry? args vars)
                (apply-macro-args vars args body)]
           [(cons _ t) (aux t)]
           )))

;; replaces each instance of each var in the body with its accompanying arg
;;
;; string list, string list list, string list -> string list
(define (apply-macro-args vars args body) 
  (match (cons vars args)
         [(cons '() _) body]
         [(cons (cons var t1) (cons arg t2)) 
          (apply-macro-args t1 t2 (apply-macro-var var arg t2 body))]))

;; applies the given var onto body
;;
;; string, string list, string list list, string list -> string list
(define (apply-macro-var var arg rest body)
    (flatten (foldl 
                (lambda (elem acc)
                  (if (string=? elem var) 
                    (cond
                        [(string=? var "REST") (append acc (transform-rest (cons arg rest)))]
                        [else (append acc arg)])
                    (append acc (list elem))
                    )
                  
                  )
                '() body)))

;; ensures that the given args are valid for the macro schema
;; ie. {a|b} is valid for '(arg1 arg2), but not '(arg1)
;; and {a|REST} is valid for '(arg1 ...) but not '(arg1)
;;
;; string list list, string list -> bool
(define (args-match-macro-entry? args vars)
  (cond
    [(equal? args '(())) (null? vars)]
    [(and (not (null? vars)) (string=? "REST" (last vars))) (>= (length args) (length vars))]
    [else (= (length args) (length vars))]))

;; '(("1" "+" "2") ("3")) -> '("1" "+" "2" "case" "3")
;;
;; string list list -> string list
(define (transform-rest rest)
  (match rest
         ['() '()]
         [(cons h '()) h]
         [(cons h t) (append h (list "case") (transform-rest t))]))

;; turns "\\|" to "case"
;;
;; string list -> string list
;(define (unescape-bars ts)
  ;(match ts
         ;['() '()]
         ;[(cons "\\|" t) (cons "case" (unescape-bars t))]
         ;[(cons h t) (cons h (unescape-bars t))]))


