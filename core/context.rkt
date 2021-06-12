#lang racket
(require cm/core/error cm/core/macros)
(provide (all-defined-out))

;; Matthew Dolinka
;;

(struct ContextEntry (value))
(struct GlobalContextEntry (value private? static?))
;; (GlobalContextKey "var" x "/home/user/dir/f.cm")
;; type = "var" | "type" | "macro"
(struct GlobalContextKey (type name module-id) #:transparent)
(struct Reference (target))


;; context members can reference other context members, so we will follow
;; down the references
(define (follow-key-to-entry ref)
  (match ref
         [(GlobalContextKey _ _ _) 
          (follow-key-to-entry (hash-ref global-context ref))]
         [(GlobalContextEntry _ _ _) ref]
         [(Reference ref)
          (follow-key-to-entry ref)]
         [_ #f]
         ))

(define (follow-key-to-data ref)
  (match (follow-key-to-entry ref)
         [(GlobalContextEntry data _ _) data]
         [_ #f]
         ))


;; gets the key just before an entry
(define (follow-key-to-key ref)
  (let aux ([curr ref] [last ref])
      (match curr
             [(GlobalContextKey _ _ _) 
              (aux (hash-ref global-context curr) curr)]
             [(GlobalContextEntry _ _ _) last]
             [(Reference ref) (aux ref curr)]
             [_ #f]
         )))

;; #t if key directly maps to reference, #f otherwise
;;
;; GlobalContextKey -> bool
(define (maps-to-reference? key)
  (if (not (hash-has-key? global-context key))
    #f
    (match (hash-ref global-context key)
           [(Reference _) #t]
           [_ #f])))

;;
;; Global context
;;

;; Vars

(define global-context (make-hash))

; `true` if global var exists in the given module, else `false`
(define (global-var-exists? label module-id)
  (hash-has-key? global-context (GlobalContextKey "var" label module-id)))

(define (set-global-var! var value private? static? module-id) 
  (hash-set! global-context (GlobalContextKey "var" var module-id) 
             (GlobalContextEntry value private? static?)))

;; derefs to the source of a mapping and alters it
(define (update-global-var! var value module-id)
  (let ([key (GlobalContextKey "var" var module-id)])
      (unless (hash-has-key? global-context key) 
        (cm-error "UNDEFINED" 
            (format "Var ~a has not yet been defined and thus cannot be set." var)))
      (let* 
        ([key2 (follow-key-to-key key)] 
         [entry2 (hash-ref global-context key2)])
        (match entry2
                [(GlobalContextEntry _ private? static?) 
                    (hash-set! global-context key2
                         (GlobalContextEntry value private? static?))]))))

(define (get-global-var-data var module-id)
        (match (hash-ref global-context (GlobalContextKey "var" var module-id) (lambda () #f))
               [#f #f]
               [(GlobalContextEntry data _ _) data]
               [(Reference ref) (follow-key-to-data ref)]))

;; Types

(define (set-type! type schema private? module-id)
  (hash-set! global-context
             (GlobalContextKey "type" type module-id)
             (GlobalContextEntry schema private? #t)))


(define (get-type-data type module-id)
        (match (hash-ref global-context (GlobalContextKey "type" type module-id) (lambda () #f))
               [#f #f]
               [(GlobalContextEntry data _ _) data]
               [(Reference ref) (follow-key-to-data ref)]))

(define (type-exists? type module-id)
  (hash-has-key? global-context (GlobalContextKey "type" type module-id)))


;; Macros

;; clean redefine the macro
(define (set-macro! label vars value private? module-id) 
  (hash-set! global-context (GlobalContextKey "macro" label module-id) 
             (GlobalContextEntry 
               (list (MacroRule 
                  (check-macro-vars vars)
                  value
                  module-id))
               private? #t)))

;; append onto the macros definitions
(define (append-to-macro! label vars value module-id)
  (if (hash-has-key? global-context (GlobalContextKey "macro" label module-id))
    (match (hash-ref global-context (GlobalContextKey "macro" label module-id))
     [(GlobalContextEntry defs private? static?)
      (hash-set! global-context (GlobalContextKey "macro" label module-id) 
                 (GlobalContextEntry (append defs
                         (list (MacroRule 
                                 (check-macro-vars vars)
                                 value 
                                 module-id))) private? static?))])
      (set-macro! label vars value (var-name-private? label))
                 ))


(define (get-macro-defs label module-id)
        (match (hash-ref global-context (GlobalContextKey "macro" label module-id) (lambda () #f))
               [#f #f]
               [(GlobalContextEntry defs _ _) defs]
               [(Reference ref) (follow-key-to-data ref)]))

;;
;; local getters, setters
;;

;; returns a hash
(define (set-local-var var value context) 
  (hash-set context var (ContextEntry value)))

(define (get-local-var-data var context)
        (match (hash-ref context var (lambda () #f))
               [#f #f]
               [res (match res [(ContextEntry data) data])]))



;;
;; Utils
;;

;; checks if the naming style of the variable indicates a private var
;; ie. _var
(define (var-name-private? name) (string=? "_" (substring name 0 1)))

;; follows down a key to its resulting entry and returns #t if it is
;; private else #f
;;
;; GlobalContextKey -> bool
(define (leads-to-private? key)
  (match (follow-key-to-entry key)
         [(GlobalContextEntry _ private? _) private?]))

;; follows down a key to its resulting entry and returns #t if it is
;; static else #f
;;
;; GlobalContextKey -> bool
(define (leads-to-static? key)
  (match (follow-key-to-entry key)
         [(GlobalContextEntry _ _ static?) static?]))

;; #t if the key leads to a valid entry, else false.
;; if public-only? then only returns #t if the resulting entry is public
;;
;; GlobalContextKey, bool -> bool
(define (entry-exists? key public-only?)
  (and 
      (hash-has-key? global-context key)
      (if public-only? (not (leads-to-private? key)) #t)))


;; maps a reference to the given name in module-id and returns #t, if it exists,
;; else returns #f
;;
;; string = ("macro" "type" "var"), string, string, string, bool -> bool
(define (map-reference type name from-module-id to-module-id prefix public-only?)
  (let ([key (GlobalContextKey type name from-module-id)])
  (if (and 
        (entry-exists? key public-only?)
        (not (maps-to-reference? key)))
    (begin 
      (hash-set! global-context 
                 (GlobalContextKey type (string-append prefix name) to-module-id)
                 (Reference key))
      #t)
    #f)))

;; copies the item (not a reference!) to the current module space if it exists and returns #t,
;; if it does not exist, then returns #f and does nothing
;;
;; string = ("macro" | "type" | "var"), string, string, string, bool -> bool
(define (clone-item type name from-module-id to-module-id prefix make-private? public-only?)
  (let ([key (GlobalContextKey type name from-module-id)])
  (if (entry-exists? key public-only?)
    (begin 
      ;; get direct value and map directly in current module-space
      (match (follow-key-to-entry key)
        [(GlobalContextEntry value _ static?)
        (hash-set! global-context 
                 (GlobalContextKey 
                   type (string-append prefix name) to-module-id)
                 (GlobalContextEntry value make-private? static?))])
      #t)
    #f)))

;; attemps to map the item, if it cannot be mapped, attempts to clone the item,
;; if it cannot be cloned, returns #f else #t.
(define (map-or-clone-item type name from-module-id to-module-id prefix public-only?)
  (if (map-reference type name from-module-id to-module-id prefix public-only?)
    #t
    (clone-item type name from-module-id to-module-id prefix #t public-only?)))

;; Populates the current module space with references (or copies if entry is not static)
;; to every item included in the given module referenced by module-id. 
;; Every item is added with the given prefix.
;;
;; string, string, list, string, bool -> void
(define (migrate-context! 
          from-module-id to-module-id prefix public-only?)
  (map 
    ;; maps each reference; only keys that lead to public members will succeed
    (lambda (elem) 
      (match elem
             ;; non-static elems will be directly copied over
             ;; as private members
             [(GlobalContextKey type name id) 
              #:when (not (leads-to-static? elem))
              (clone-item type name id to-module-id prefix #t public-only?)]
             ;; static elems will be mapped to with references in the
             ;; current module space
             [(GlobalContextKey type name id) 
              (map-reference type name id to-module-id prefix public-only?)]))
    ;; only use keys that belong to the current module
    (get-global-context-keys '() from-module-id public-only?))
  (void))

;; gets all GlobalContextKeys from global context that meet the given criteria
;;
;; if type = '(), then all types will be returned
;; if module-id = '(), then items are grabbed from all modules
;; if public-only? = #t, then only public members will be returned
;;
;; (string | '()), (string | '()), bool -> GlobalContextKey list
(define (get-global-context-keys type module-id public-only?)
  (filter 
        (lambda (elem) 
          (match elem 
                 [(GlobalContextKey type2 _ id2) 
                  (and
                    (if (not (null? type))
                      (equal? type type2)
                      #t)
                    (if (not (null? module-id))
                      (equal? module-id id2)
                      #t)
                    (if public-only?
                     (not (leads-to-private? elem))
                      #t)
                  )])) 
        (hash-keys global-context)))


;; (string | '()), (string | '()), bool ->
;;      (GlobalContextKey . GlobalContextEntry) list
(define (get-global-context-pairs type module-id public-only?)
  (foldl 
    (lambda (elem acc) 
      (cons (cons elem (follow-key-to-entry elem)) acc))
    '()
    (get-global-context-keys type module-id public-only?))
  )

(define (get-label-of-key key)
  (match key
         [(GlobalContextKey _ label _) label]
         [_ #f]))

(define (sort-global-context-pairs pairs)
  (sort  
     pairs
     (lambda (elem1 elem2) 
       (string<?
        (get-label-of-key (car elem1))
        (get-label-of-key (car elem2))))))

;; print the global-context (for debugging)
(define (print-global-context)
  (let aux ([ms (append 
                  (sort-global-context-pairs 
                    (get-global-context-pairs "macro" '() #f))
                  (sort-global-context-pairs
                    (get-global-context-pairs "type" '() #f))
                  (sort-global-context-pairs 
                    (get-global-context-pairs "var" '() #f))
                  )])
    (match ms
        ['() (void)]
        [(cons (cons label (GlobalContextEntry defs _ _)) t) 
         (display (format "label: ~a\n" label))
         (print-entries defs)
         (displayln "")
         (aux t)]
    ))
  )

(define (print-entries es)
  (match es
         ['() (void)]
         [(cons (MacroRule vars body module-id) t)
          (display (format "module:~a\nvars:\n~a\nbody:\n~a\n" module-id vars body))
          (print-entries t)]
         [e (displayln e)]))
