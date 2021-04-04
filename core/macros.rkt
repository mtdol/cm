#lang racket
(require racket/lazy-require cm/core/error)
(lazy-require (cm/core/context 
                [get-macro-defs print-macro-context]) 
              (cm/core/types [is-string-token?])
              (cm/core/pre-lex [unwrap-string]))
(provide (all-defined-out))

(struct MacroRule (vars body module-id))

(define (invalid-args-error label args)
  (let ([num-args (if (equal? args '(())) 0 (length args))])
    (cm-error 
      "MACRO" 
      (format "No matching rule for macro with label ~a and ~a args." label num-args))))

;; a|b|c -> bca
(define (apply-reverse-macro args)
  (flatten (reverse args)))

;; turns a list of tokens into a space seperated string
(define (apply-string-macro args)
  (unless (= (length args) 1) (invalid-args-error "string" args))
  (list (string-append 
    "\"" 
    (string-trim (foldl 
        (lambda (elem acc) (string-append acc elem " "))
        ""
        (flatten args)))
    "\"")))

;; {ifdef "macro_name" | then | else}
(define (apply-ifdef-macro args module-id)
  (unless (= (length args) 3) (invalid-args-error "ifdef" args))
  (when (null? (car args)) 
    (cm-error "MACRO" "ifdef is missing a guard argument."))
  (when (or (not (= 1 (length (car args)))) 
            (not (is-string-token? (caar args)))) 
    (cm-error "MACRO" "Guard argument to ifdef must be a single string."))
  (if (get-macro-defs (unwrap-string (caar args)) module-id)
    (list-ref args 1)
    (list-ref args 2)))


;;
;; Macro Utils
;;

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
(define (apply-macro label args module-id) 
  (match label
    ;; first check if a preloaded macro
    ["reverse" (apply-reverse-macro args)]
    ["string" (apply-string-macro args)]
    ["ifdef" (apply-ifdef-macro args module-id)]
    ;; then run user defined macros
    [_ 
     (let aux ([entry (get-macro-defs label module-id)])
      (match entry
             [#f (cm-error "MACRO" (format "Macro not defined: ~a" label))]
             ['() (invalid-args-error label args)]
             [(cons (MacroRule vars body module-id) t) 
              #:when (args-match-macro-entry? args vars)
              (apply-macro-args vars args body)]
             [(cons _ t) (aux t)]
             ))]))

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
