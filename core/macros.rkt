#lang racket
(require racket/lazy-require cm/core/error cm/core/tokens)
(lazy-require (cm/core/context 
                [get-macro-defs print-macro-context]) 
              (cm/core/types [is-string-token?])
              (cm/core/pre-lex [unwrap-string])
              (cm/core/modules [get-filename file-name->module-id]))
(provide (all-defined-out))

(struct MacroRule (vars body module-id))

(define (invalid-args-error label args)
  (let ([num-args (if (equal? args '(())) 0 (length args))])
    (cm-error-linenum (macros:get-current-module-id) (get-current-linenum) 
      "MACRO" 
      (format "No matching rule for macro with label ~a and ~a args." label num-args))))

;; turns a list of tokens into a space seperated string
(define (apply-string-macro args)
  (unless (= (length args) 1) (invalid-args-error "string" args))
  (list (Token (string-append 
    "\"" 
    (string-trim (foldl 
        (lambda (elem acc) (string-append acc (tok elem) " "))
        ""
        (flatten args)))
    "\""))))

;; {ifdef "macro_name" | then | else}
(define (apply-ifdef-macro args module-id)
  (unless (= (length args) 3) (invalid-args-error "ifdef" args))
  (when (null? (car args)) 
    (cm-error-linenum (macros:get-current-module-id) (get-current-linenum)
                      "MACRO" "ifdef is missing a guard argument."))
  (when (or (not (= 1 (length (car args)))) 
            (not (is-string-token? (tok (caar args))))) 
    (cm-error-linenum (macros:get-current-module-id) (get-current-linenum)
              "MACRO" "Guard argument to ifdef must be a single string."))
  (if (get-macro-defs (unwrap-string (tok (caar args))) module-id)
    (list-ref args 1)
    (list-ref args 2)))

(define (apply-current-module-macro args module-id)
  (unless (equal? args '(())) (invalid-args-error "current_module" args))
  (list (Token (string-append "\"" module-id "\""))))

;; turns a module style string into a in language string
(define (apply-to-module-id-macro args)
  (unless (and (= (length args) 1) (not (equal? args '(()))))
    (invalid-args-error "->module_id" args))
  (unless (not (equal? (tok (caar args)) "\"\""))
    (cm-error-linenum (macros:get-current-module-id) (get-current-linenum)
      "MACRO" "->module_id requires a non-empty argument."))
  (list (Token (string-append 
    "\"" (file-name->module-id 
           (get-filename (unwrap-string (tok (caar args)))))
    "\""))))


;;
;; Macro Utils
;;

(define macros:current-module-id "0")
(define (macros:get-current-module-id) (string-copy macros:current-module-id))
(define (macros:set-current-module-id! id) (set! macros:current-module-id id))

;; checks that macro vars are well formed
;;
;; string list -> string list | error
(define (check-macro-vars vars)
  (match vars
         ['() '()]
         ;; vars is valid if REST only appears at the end
         [vs #:when (not (member "REST" (cdr (reverse vars)))) vs]
         [vs (cm-error "SYNTAX" (format "Invalid Macro vars: ~a" vs))]))


;; token, token list list, string -> token list
(define (apply-macro label args module-id) 
  (macros:set-current-module-id! module-id)
  (match (tok label)
    ;; first check if a preloaded macro
    ["string" (apply-string-macro args)]
    ["ifdef" (apply-ifdef-macro args module-id)]
    ["current_module" (apply-current-module-macro args module-id)]
    ["->module_id" (apply-to-module-id-macro args)]
    ;; then run user defined macros
    [_ 
     (let aux ([entry (get-macro-defs (tok label) module-id)])
      (match entry
             [#f (cm-error-linenum 
                   module-id (get-current-linenum)
                   "MACRO" (format "Macro not defined: ~a" (tok label)))]
             ['() (invalid-args-error (tok label) args)]
             [(cons (MacroRule vars body module-id) t) 
              #:when (args-match-macro-entry? args vars)
              (apply-macro-args vars args body)]
             [(cons _ t) (aux t)]
             ))]))

;; replaces each instance of each var in the body with its accompanying arg
;;
;; string list, token list list, token list -> token list
(define (apply-macro-args vars args body) 
  (match (cons vars args)
         [(cons '() _) body]
         [(cons (cons var t1) (cons arg t2)) 
          (apply-macro-args t1 t2 (apply-macro-var var arg t2 body))]))

;; applies the given var onto body
;;
;; string, token list, token list list, token list -> token list
(define (apply-macro-var var arg rest body)
    (flatten (foldl 
                (lambda (elem acc)
                  (if (string=? (tok elem) var) 
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
;; token list list, string list -> bool
(define (args-match-macro-entry? args vars)
  (cond
    [(equal? args '(())) (null? vars)]
    [(and (not (null? vars)) (string=? "REST" (last vars))) (>= (length args) (length vars))]
    [else (= (length args) (length vars))]))

;; '(("1" "+" "2") ("3")) -> '("1" "+" "2" "case" "3")
;;
;; token list list -> token list
(define (transform-rest rest)
  (match rest
         ['() '()]
         [(cons h '()) h]
         [(cons h t) (append h (list (Token "case")) (transform-rest t))]))
