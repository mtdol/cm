#lang racket
(require cm/core/error)
(provide (all-defined-out))

(struct ContextEntry (value))
(struct VarEntry (value private?))
(struct VarKey (name module-id) #:transparent)
(struct TypeEntry (value private?))
(struct TypeKey (name module-id) #:transparent)
(struct MacroRule (vars body))
(struct MacroEntry (defs private?))
(struct MacroKey (name module-id) #:transparent)
(struct Reference (target))
;; what a lambda expr yields
(struct Fun (var type context expr))

(define current-module-id "0")
(define (get-current-module-id) current-module-id)
(define (set-current-module-id! val) (set! current-module-id val))


;;
;; Global context
;;

(define global-context (make-hash))

(define (set-global-var! var value private?) 
  (hash-set! global-context (VarKey var current-module-id) (VarEntry value private?)))

;; context members can reference other context members, so we will follow
;; down the references
(define (follow-var-data-references ref)
  (match ref
         [(VarKey _ _) 
          (follow-var-data-references (hash-ref global-context ref))]
         [(VarEntry data _) data]
         [(Reference ref) (follow-var-data-references ref)]
         [_ #f]
         ))

(define (get-global-var-data var)
        (match (hash-ref global-context (VarKey var current-module-id) (lambda () #f))
               [#f #f]
               [(VarEntry data _) data]
               [(Reference ref) (follow-var-data-references ref)]))


(define (set-type! type schema private?)
  (hash-set! global-context
             (TypeKey type current-module-id)
             (TypeEntry schema private?)))

(define (follow-type-data-references ref)
  (match ref
         [(TypeKey _ _) 
          (follow-type-data-references (hash-ref global-context ref))]
         [(TypeEntry data _) data]
         [(Reference ref) (follow-type-data-references ref)]
         [_ #f]
         ))

(define (get-type-data type)
        (match (hash-ref global-context (TypeKey type current-module-id) (lambda () #f))
               [#f #f]
               [(TypeEntry data _) data]
               [(Reference ref) (follow-type-data-references ref)]))

(define (type-exists? type)
  (hash-has-key? global-context (TypeKey type current-module-id)))


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

;; clean redefine the macro
(define (set-macro! label vars value private?) 
  (hash-set! global-context (MacroKey label current-module-id) 
             (MacroEntry (list (MacroRule (check-macro-vars vars) value)) private?)))

;; append onto the macros definitions
(define (append-to-macro! label vars value)
  (if (hash-has-key? global-context (MacroKey label current-module-id))
    (match (hash-ref global-context (MacroKey label current-module-id))
     [(MacroEntry defs private?)
      (hash-set! global-context (MacroKey label current-module-id) 
                 (MacroEntry (append defs
                         (list (MacroRule (check-macro-vars vars) value))) private?))])
      (set-macro! label vars value (var-name-private? label))
                 ))

(define (follow-macro-data-references ref)
  (match ref
         [(MacroKey _ _) 
          (follow-macro-data-references (hash-ref global-context ref))]
         [(MacroEntry data _) data]
         [(Reference ref) (follow-macro-data-references ref)]
         [_ #f]
         ))

(define (get-macro-defs label)
        (match (hash-ref global-context (MacroKey label current-module-id) (lambda () #f))
               [#f #f]
               [(MacroEntry defs _) defs]
               [(Reference ref) (follow-macro-data-references ref)]))

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
           [(cons (MacroRule vars body) t) 
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

;;
;; Utils
;;

;; checks if the naming style of the variable indicates a private var
;; ie. _var
(define (var-name-private? name) (string=? "_" (substring name 0 1)))

;; populates the current module space with references to every item
;; included in the items list. If the items list is null, then
;; returns references to everything in the module space.
;; Every item is added with the given prefix
;;
;; string, list, string, bool -> void
(define (set-refs-from-module-space! module-id items prefix public-only?)
  (map 
    (lambda (elem) 
      (match elem
        [(cons (VarKey id _) _) 
         (hash-set! global-context 
                    (VarKey (string-append prefix id) current-module-id)
                    (Reference (car elem)))]
        [(cons (TypeKey id _) _) 
         (hash-set! global-context 
                    (TypeKey (string-append prefix id) current-module-id)
                    (Reference (car elem)))]
        [(cons (MacroKey id _) _) 
         (hash-set! global-context 
                    (MacroKey (string-append prefix id) current-module-id)
                    (Reference (car elem)))]
        ))
    (append
      (get-var-context-pairs module-id items public-only?)
      (get-type-context-pairs module-id items public-only?)
      (get-macro-context-pairs module-id items public-only?)
      )))

;; maps a reference to the given name in module-id and returns #t, if it exists,
;; else returns #f
;;
;; string = ("macro" "type" "var"), string, string, string, bool -> bool
(define (map-reference type name module-id prefix public-only?)
    (match type
           ["macro" 
            (if (and 
                  (hash-has-key? global-context (MacroKey name module-id))
                  (match 
                    (hash-ref global-context (MacroKey name module-id))
                        [(MacroEntry _ private?) 
                         (if public-only? 
                           (not private?)
                           #t)]
                        [_ #f]))
              (begin 
                (hash-set! global-context 
                           (MacroKey (string-append prefix name) current-module-id)
                           (Reference (MacroKey name module-id)))
                #t)
              #f)]
           ["type" 
            (if (and 
                  (hash-has-key? global-context (TypeKey name module-id))
                  (match 
                    (hash-ref global-context (TypeKey name module-id))
                        [(TypeEntry _ private?) 
                         (if public-only? 
                           (not private?)
                           #t)]
                        [_ #f]))
              (begin 
                (hash-set! global-context 
                           (TypeKey (string-append prefix name) current-module-id)
                           (Reference (TypeKey name module-id)))
                #t)
              #f)]
           ["var" 
            (if (and 
                  (hash-has-key? global-context (VarKey name module-id))
                  (match 
                    (hash-ref global-context (VarKey name module-id))
                        [(VarEntry _ private?) 
                         (if public-only? 
                           (not private?)
                           #t)]
                        [_ #f]))
              (begin 
                (hash-set! global-context 
                           (VarKey (string-append prefix name) current-module-id)
                           (Reference (VarKey name module-id)))
                #t)
              #f)]
           [_ #f]
           )
  )

;; gets all var-context mappings of the given module-id
;; if module-id is #f then returns all var-context mappings.
;; If public-only? is true then only public elems will be gathered.
;; The items parameter is limits the returned items to only the names
;; in the list. If items is null then all items will be returned.
;;
;; string | bool, list, bool -> (VarKey . VarEntry) list
(define (get-var-context-pairs module-id items public-only?) 
  (filter 
    (lambda (elem) 
      (match elem 
        [(cons (VarKey name id) (VarEntry _ private?)) 
         (and 
           (if module-id (equal? id module-id) #t)
           (if public-only? (not private?) #t)
           (if (not (null? items)) (member name items) #t))] 
        [_ #f]))
    (hash->list global-context)))

;; string | bool, list, bool -> (TypeKey . TypeEntry) list
(define (get-type-context-pairs module-id items public-only?) 
  (filter 
    (lambda (elem) 
      (match elem 
        [(cons (TypeKey name id) (TypeEntry _ private?)) 
         (and 
           (if module-id (equal? id module-id) #t)
           (if public-only? (not private?) #t)
           (if (not (null? items)) (member name items) #t))] 
        [_ #f]))
    (hash->list global-context)))

;; string | bool, list, bool -> (MacroKey . MacroEntry) list
(define (get-macro-context-pairs module-id items public-only?) 
  (filter 
    (lambda (elem) 
      (match elem 
        [(cons (MacroKey name id) (MacroEntry _ private?)) 
         (and 
           (if module-id (equal? id module-id) #t)
           (if public-only? (not private?) #t)
           (if (not (null? items)) (member name items) #t))] 
        [_ #f]))
    (hash->list global-context)))


;; print the macro-context (for debugging)
(define (print-macro-context)
  (let aux ([ms (get-macro-context-pairs #f #f)])
    (match ms
        ['() (void)]
        [(cons (cons label (MacroEntry defs _)) t) 
         (display (format "label: ~a\n" label))
         (print-macro-entries defs)
         (displayln "")
         (aux t)]
    )))

(define (print-macro-entries es)
  (match es
         ['() (void)]
         [(cons (MacroRule vars body) t)
          (display (format "vars:\n~a\nbody:\n~a\n" vars body))
          (print-macro-entries t)]))
