#lang racket
(require racket/hash racket/lazy-require
         cm/core/ast cm/core/error cm/core/types cm/core/context
         cm/core/parse-stat cm/core/lex cm/core/interp-utils cm/core/modules)
(lazy-require [cm/core/main (run run-expr run-file)])
(provide interp trace-interp-expr interp-apply interp-expr)

;; Matthew Dolinka
;; cm interpreter


(define (interp-prep expr linenum module-id)
  (set-current-linenum! linenum)
  (trace-interp-expr expr (hash) (hash) module-id (DebugData linenum)))

;; converts values into the output format
(define (prepare-for-output v)
    v)

;; takes in an expr or a statement and returns a list of all accumulated values
(define (interp ast module-id) 
  (match ast
    [(Stat i e st) 
     ;; add to trace-stack for error messages
     ;(push-elem-to-trace-stack! 
       ;(TraceElem "statement" "" module-id i))
     (match st
            ;; each statement in a chain of statements is consed together
            ;; into a list
            [(EOP) 
             (cons (prepare-for-output (interp-prep e i module-id)) '())]
            [_ (cons (prepare-for-output (interp-prep e i module-id)) 
                     (interp st module-id))])]
    ;; if we get an EOP only, just return void
    [(EOP) (prepare-for-output 
             (trace-interp-expr (Prim0 'void) (hash) (hash) module-id (DebugData -1)))]
    ;; expr only case, no list, just a value returned
    [_ (prepare-for-output (trace-interp-expr ast (hash) (hash) module-id (DebugData -1)))]))

;; interps expr while updating the stack trace
(define (trace-interp-expr e context params module-id debug)
  ;(push-elem-to-trace-stack! (ast-node-to-trace-elem e module-id debug))
  (interp-expr e context params module-id debug))

(define (interp-expr ast context params module-id debug)
  (match ast
    [(Prim2 'when _ _) (cm-error "SYNTAX" "Cannot use `when` outside of match-expr.")]
    [(Prefix2 '? _ _) (cm-error "SYNTAX" "Cannot use `?` outside of match-expr.")]
    [(Prim0 '...) (cm-error "SYNTAX" "Cannot use `...` outside of match-expr.")]
    [(? var-container? var) 
     (interp-var (get-var-label var context params module-id debug) context params module-id)]
    [(Prim1 'get_param var) 
     (interp-get-param var context params module-id debug)]
    [(Prim1 'schemaof e1) (interp-schemaof e1 context params module-id debug)]
    [(Prefix2 'internal_op e1 e2)
     (interp-internal-ops
       (trace-interp-expr e1 context params module-id debug)
       (trace-interp-expr e2 context params module-id debug)
       context params module-id debug)]
    [(Prefix2 'load e1 e2)
     (interp-load 
       (trace-interp-expr e1 context params module-id debug)
       (trace-interp-expr e2 context params module-id debug)
       params module-id)]
    [(Prefix2 'defined? e1 e2)
     (interp-defined?
       (trace-interp-expr e1 context params module-id debug)
       (trace-interp-expr e2 context params module-id debug)
       "all" context params)]
    [(Prefix2 'global.defined? e1 e2)
     (interp-defined?
       (trace-interp-expr e1 context params module-id debug)
       (trace-interp-expr e2 context params module-id debug)
       "global" context params)]
    [(Prefix2 'struct e1 e2) (interp-struct e1 e2 context params module-id debug)]
    [(Prefix2 'struct? e1 e2) (interp-struct? e1 e2 context params module-id debug)]
    [(Prefix2 'appl e1 e2) (interp-appl e1 e2 context params module-id debug)]
    [(Prefix2 'index e1 e2) (interp-index e1 e2 context params module-id debug)]
    [(Prim2 ':: e1 e2) (interp-index e1 e2 context params module-id debug)]
    [(Prefix2 'appendstrf e1 e2) 
     (interp-appendstrf 
       (trace-interp-expr e1 context params module-id debug)
       (trace-interp-expr e2 context params module-id debug)
       params)]
    ;; short circuiting and/or
    [(Prim2 'and e1 e2) 
     (let ([v1 (trace-interp-expr e1 context params module-id debug)])
       (assert-contract (list "bool") v1 params "and")
       (if 
         (bool-to-racket v1)
         (let ([v2 (trace-interp-expr e2 context params module-id debug)])
           (assert-contract (list "bool") v2 params "and")
           v2)
         (racket-to-bool #f)))]
    [(Prim2 'or e1 e2) 
     (let ([v1 (trace-interp-expr e1 context params module-id debug)])
       (assert-contract (list "bool") v1 params "or")
       (if 
         (bool-to-racket v1)
         (racket-to-bool #t)
         (let ([v2 (trace-interp-expr e2 context params module-id debug)])
           (assert-contract (list "bool") v2 params "or")
           v2)))]
    ;; general prim cases
    [(Prim2 op e1 e2) (interp-prim2 
                        op 
                        (trace-interp-expr e1 context params module-id debug) 
                        (trace-interp-expr e2 context params module-id debug)
                        params module-id debug)]
    [(Prim1 op e) (interp-prim1 
                    op (trace-interp-expr e context params module-id debug) 
                    context params module-id debug)]
    [(Prim0 op) (interp-prim0 op)]
    [(If e1 (Then e2) (Else e3)) (interp-if e1 e2 e3 context params module-id debug)]
    [(If _ _ _) (cm-error "SYNTAX" "Improperly formed `if`.")]
    [(Cond e) 
     (match e
            [(Case e1 e2 e3) (interp-case e1 e2 e3 context params module-id debug)]
            [_ (cm-error "SYNTAX" "`cond` is missing a case.")])]
    [(Case e1 e2 e3) (interp-case e1 e2 e3 context params module-id debug)]
    [(While e1 (Do e2)) (interp-while e1 e2 context params module-id debug)]
    [(While _ _) (cm-error "SYNTAX" "`while` is missing a do.")]
    [(Foreach e1 (In e2) (Do e3))
     (let ([vs (trace-interp-expr e2 context params module-id debug)])
       (if (is-list? vs)
           (interp-foreach e1 vs e3 context params module-id debug)
           (cm-error "CONTRACT" "Argument to `foreach` guard must be a list.")))]
    [(Foreach _ _ _) (cm-error "SYNTAX" "`foreach` is improperly formed.")]
    [(Try e1 (Catch e2) (With e3))
     (interp-try-catch e1 e2 e3 context params module-id debug)]
    [(Try _ _ _) (cm-error "SYNTAX" "Improperly formed `try`.")]
    [(Match e1 e2) (interp-match 
                     (trace-interp-expr e1 context params module-id debug)
                     e2 context context params module-id debug)]
    [(Def e1 (Assign e2)) 
      (match (interp-def-list e1 e2)
             [(Def e1-2 (Assign e2-2)) 
              (let ([v2 (trace-interp-expr e2-2 context params module-id debug)])
              (let-values ([(guard-types label)
                            (interp-left-hand-expr e1-2 context params module-id debug)])
                (interp-def guard-types label v2 "def" context params module-id debug)))])]
    [(Def _ _) (cm-error "SYNTAX" "Improperly formed `def`.")]
    [(Static e1 (Assign e2)) 
      (match (interp-static-list e1 e2)
        [(Static e1-2 (Assign e2-2)) 
         (let-values ([(guard-types label)
                       (interp-left-hand-expr e1-2 context params module-id debug)])
         ;; only interp and assign if not already defined, else return void
         (if (get-global-var-data label module-id)
           (Prim0 'void)
           (let ([v2 (trace-interp-expr e2-2 context params module-id debug)])
             (interp-def guard-types label v2 "static" context params module-id debug))))])]
    [(Static _ _) (cm-error "SYNTAX" "Improperly formed `static`.")]
    [(Set e1 (Assign e2)) 
     (let-values ([(guard-types label) 
                   (interp-left-hand-expr e1 context params module-id debug)])
     (interp-def 
       guard-types label
       (trace-interp-expr e2 context params module-id debug)
       "set" context params module-id debug))]
    [(Set _ _) (cm-error "SYNTAX" "Improperly formed `set`.")]
    [(Defun (? var-container? var) e1 (Assign e2))
     (let ([name (get-var-label var context params module-id debug)])
     (match (interp-lambda-list e1 e2)
       [(Lambda e1-2 (Yields e2-2))
        (interp-def '("dynamic") name
          (interp-lambda e1-2 e2-2 name context params module-id debug)
          "def" context params module-id debug)
        ])
      )]
    [(Defun _ _ _) (cm-error "SYNTAX" "Improperly formed `defun`.")]
    [(Let e1 (Assign e2) (In e3)) (interp-let e1 e2 e3 context params module-id debug)]
    [(Let _ _ _) (cm-error "SYNTAX" "Improperly formed `let`.")]
    [(Param e1 (Assign e2) (In e3)) (interp-param e1 e2 e3 context params module-id debug)]
    [(Param _ _ _) (cm-error "SYNTAX" "Improperly formed `param`.")]
    [(Letrec (? var-container? var) args-e (Assign body-e) (In in-e))
     (interp-letrec (get-var-label var context params module-id debug)
                    args-e body-e in-e
                    context params module-id debug)]
    [(Letrec _ _ _ _) (cm-error "SYNTAX" "Improperly formed `letrec`.")]
    [(Letaux (? var-container? var) expr)
     (interp-letaux (get-var-label var context params module-id debug)
                    expr context params module-id debug)]
    [(Letaux _ _) (cm-error "SYNTAX" "Improperly formed `letaux`.")]
    [(Lambda e1 (Yields e2)) 
      (match (interp-lambda-list e1 e2)
             [(Lambda e1-2 (Yields e2-2)) 
              (interp-lambda e1-2 e2-2 '() context params module-id debug)])]
    [(Lambda _ _) (cm-error "SYNTAX" "`lambda` is missing an assignment.")]
    [(Typedef e1 (Assign e2)) (interp-typedef e1 e2 context params module-id debug)]
    [(Let _ _ _) (cm-error "SYNTAX" "Improperly formed `typedef`.")]
    [(Int i) i]
    [(Float f) f]
    [(Bool i) (Bool i)]
    [(Null) null]
    [(String s) (fix-string s)]
    [e (cm-error "SYNTAX" (format "Unknown expression: ~a." e))]))

(define (and-op arg1 arg2) (and (bool-to-racket arg1) (bool-to-racket arg2)))
(define (or-op arg1 arg2) (or (bool-to-racket arg1) (bool-to-racket arg2)))
(define (xor-op arg1 arg2) (xor (bool-to-racket arg1) (bool-to-racket arg2)))
(define (not-equal? arg1 arg2) (not (equal? arg1 arg2)))
(define (not-eqq? arg1 arg2) (not (deep-equal? arg1 arg2)))
(define (not-op arg) (not (bool-to-racket arg)))

(define (interp-prim2 op v1 v2 params module-id debug)
  (match op
        ['cons (cons v1 v2)]
        [': (interp-apply v1 v2 params)]
        ['$ (string-append (string-coerce v1)
                           (string-coerce v2))]

        ['++ 
         (assert-contract (list "list") v1 params "++")
         (assert-contract (list "list") v2 params "++")
         (append v1 v2)]
        ['comma v2]
        ['== (racket-to-bool (deep-equal? v1 v2))]
        ['!== (racket-to-bool (not-eqq? v1 v2))]
        ;; everything below is checked for same operands
        [op #:when (not (string=? (get-type v1) (get-type v2)))
          (cm-error "CONTRACT" (string-append "Operands of " (symbol->string op)
                " are not of the same type. Given " (get-type v1)
                ", " (get-type v2) "."))]
        ['xor (racket-to-bool 
               (apply-if-type '("bool") xor-op "xor" v1 v2))]
        ['> (racket-to-bool 
               (apply-if-type '("int" "float") > ">" v1 v2))]
        ['< (racket-to-bool 
               (apply-if-type '("int" "float") < "<" v1 v2))]
        ['>= (racket-to-bool 
               (apply-if-type '("int" "float") >= ">=" v1 v2))]
        ['<= (racket-to-bool 
               (apply-if-type '("int" "float") <= "<=" v1 v2))]
        ['= (racket-to-bool 
               (apply-if-type '("int" "float" "string" "list" "pair" "bool" "null")
                              equal? "=" v1 v2))]
        ['!= (racket-to-bool 
               (apply-if-type '("int" "float" "string" "list" "pair" "bool" "null")
                              not-equal? "!=" v1 v2))]
        ['+ (apply-if-type '("int" "float") + "+" v1 v2)]
        ['- (apply-if-type '("int" "float") - "-" v1 v2)]
        ['* (apply-if-type '("int" "float") * "*" v1 v2)]
        ['/ (if (zero? v2) (cm-error "CONTRACT" "Divide by zero.")
         (apply-if-type '("float") / "/" v1 v2))]
        ['% (if (zero? v2) (cm-error "CONTRACT" "`mod` by zero undefined.")
         (apply-if-type '("int") modulo "%" v1 v2))
         ]
        ['^ (apply-if-type '("int" "float") expt "^" v1 v2)]))


(define (interp-prim1 op v context params module-id debug)
  (match op
        ['print (interp-print v)]
        ['local.defined? (interp-defined? v "" "local" context params)]
        ['params.defined? (interp-defined? v "" "params" context params)]
        [':> (interp-null-apply v params)]
        ;; get-global-var-data will return false if v not yet defined
        ['pos  #:when (or (string=? (get-type v) "int" ) 
                        (string=? (get-type v) "float"))
        (+ v)]
        ['pos (cm-error "CONTRACT" (string-append 
            "Applied positive to non number. Given type "
            (get-type v) "."))]
        ['neg  #:when (or (string=? (get-type v) "int" ) 
                        (string=? (get-type v) "float"))
        (- v)]
        ['neg (cm-error "CONTRACT" (string-append 
            "Applied negative to non number. Given type "
            (get-type v) "."))]
        ['head  #:when (or (string=? (get-type v) "list" ) 
                        (string=? (get-type v) "pair"))
        (when (null? v) (cm-error "CONTRACT" "Cannot `head` deref null."))
        (car v)]
        ['head (cm-error "CONTRACT" (string-append 
            "Applied head to non list or pair. Given type "
            (get-type v) "."))]
        ['tail  #:when (or (string=? (get-type v) "list" ) 
                        (string=? (get-type v) "pair"))
        (when (null? v) (cm-error "CONTRACT" "Cannot `tail` deref null."))
        (cdr v)]
        ['tail (cm-error "CONTRACT" (string-append 
            "Applied `tail` to non list or pair. Given type "
            (get-type v) "."))]
        ['length  #:when (or (string=? (get-type v) "list")
                           (string=? (get-type v) "null")) 
        (length v)]
        ['length  #:when (string=? (get-type v) "string" ) 
        (string-length v)]
        ['length (cm-error "CONTRACT" (string-append 
            "Applied `length` to non list or string. Given type "
            (get-type v) "."))]
        ['typeof (get-type v)] 
        ['string (string-coerce v)] 
        ['int (int-coerce v)] 
        ['float (float-coerce v)] 
        ['bool (bool-coerce v)] 
        ['string? (racket-to-bool (is-string? v))] 
        ['int? (racket-to-bool (is-int? v))] 
        ['float? (racket-to-bool (is-float? v))] 
        ['bool? (racket-to-bool (is-bool? v))] 
        ['list? (racket-to-bool (is-list? v))] 
        ['pair? (racket-to-bool (is-pair? v))] 
        ['null? (racket-to-bool (is-null? v))] 
        ['void? (racket-to-bool (is-void? v))] 
        ['eof? (racket-to-bool (is-eof? v))] 
        ['fun? (racket-to-bool (is-fun? v))] 
        ['not (racket-to-bool
            (apply-if-type-1 '("bool") not-op "not" v))]

        ))

(define (interp-prim0 op)
  (match op
         ['void (Prim0 'void)]
         ['eof (Prim0 'eof)]
         ))

;;
;; non-standard interp cases
;;

(define (interp-if e1 e2 e3 context params module-id debug)
  (if (bool-to-racket (trace-interp-expr e1 context params module-id debug))
    (trace-interp-expr e2 context params module-id debug)
    (trace-interp-expr e3 context params module-id debug)))

(define (interp-while e1 e2 context params module-id debug) 
  (cond
    [(bool-to-racket (trace-interp-expr e1 context params module-id debug)) 
     (trace-interp-expr e2 context params module-id debug)
     (interp-while e1 e2 context params module-id debug)]
    [else (Prim0 'void)]))

(define (interp-foreach e1 vs e2 context params module-id debug)
  (match vs
    ['() (Prim0 'void)]
    [(cons v t)
      (match (match-expr e1 v context context params module-id debug)
            [#f (cm-error "MATCH" "Could not match `foreach` guard expr.")]
            [c2 (trace-interp-expr e2 c2 params module-id debug)
                (interp-foreach e1 t e2 context params module-id debug)]
            )]
    ))

(define (interp-try-catch e1 e2 e3 context params module-id debug)
  (match e2
    [(? var-container? var) 
       (with-handlers* ([exn:fail? (lambda err 
           (match err [(list (exn:fail err-msg _))
               (let ([err-struct 
                   (CmStruct "Error" (list (get-id-from-message err-msg) err-msg))])
          (trace-interp-expr 
            e3 
            (set-local-var 
              (get-var-label var context params module-id debug) err-struct context)
            params module-id debug))]))])
                       (trace-interp-expr e1 context params module-id debug))]
    [_ (cm-error "SYNTAX" "Missing var for `try-catch`.")]))

;; match context is the internal context used inside the match, context is just the usual
;; local context
;;
;; value, expr hash, hash, hash, string, debug -> value
(define (interp-match v e match-context context params module-id debug)
  (match e 
    [(Case ce1 ce2 ce3)
     (match ce2
       [(Yields ce2-1)
        (match ce1
          [(Prim2 'when ce1-1 ce1-2)
           (let ([match-context-2 (match-expr ce1-1 v (hash) context params module-id debug)])
             (let ([merged-context (if match-context-2 
                                     (hash-union match-context match-context-2
                                       #:combine/key (lambda (k v1 v2) v2))
                                     #f)])
           (if (and merged-context 
                    (bool-to-racket 
                      (trace-interp-expr ce1-2 merged-context params module-id debug)))
                 (trace-interp-expr ce2-1 merged-context params module-id debug)
             (interp-match v ce3 match-context context params module-id debug)
             )))
           ]
          [_ 
           (let ([match-context-2 (match-expr ce1 v (hash) context params module-id debug)])
             ;; hashes evaluate to #t in racket
           (if match-context-2
             ;; merge the local contexts so that match-context 2 vals replace
             ;; match context vals
             (let ([merged-context (hash-union match-context match-context-2
                                       #:combine/key (lambda (k v1 v2) v2))])
                 (trace-interp-expr ce2-1 merged-context params module-id debug))
             (interp-match v ce3 match-context context params module-id debug)
             ))

            ]
          )
        ]
       [_ (cm-error "SYNTAX" "Missing yields for match case.")])
     ]
    [(Prim0 'end) (cm-error "GENERIC" (format "Matching failed for ~a." (string-coerce v)))]
    [_ (cm-error "SYNTAX" "Invalid match syntax.")]))

;; ast, value, hash, string, debug -> hash | #f
(define (match-expr e v match-context context params module-id debug)
  (match e
         [(Int i) (if (equal? i v) match-context #f)]
         [(Float f) (if (equal? f v) match-context #f)]
         [(Bool i1) (match v [(Bool i2) (if (equal? i1 i2) match-context #f)] [_ #f])]
         [(Null) (if (null? v) match-context #f)]
         [(Prim0 'void) (match v [(Prim0 'void) match-context] [_ #f])]
         [(String s) (if (string=? s v) match-context #f)]
         [(Prefix2 '? (? var-container? var) expr) 
          (let* ([func (trace-interp-expr expr context params module-id debug)]
                 [res (interp-apply func v params)])
            (unless (is-bool? res) 
              (cm-error "CONTRACT"
                (format "`?`: Function must return a bool value,\ngot: ~a" 
                  (string-coerce res))))
            (if (bool-to-racket res) 
              (match-expr var v match-context context params module-id debug)
              #f)
            )]
         ;; implied dynamic var
         [(? var-container? var) 
          (match (get-var-label var context params module-id debug)
            ;; wildcard, always match
            ["_" match-context]
            [label (match-var label (list "dynamic") v match-context)])]
         [(Prim1 op (? var-container? var))
          #:when (member op guard-types)
          (match (get-var-label var context params module-id debug)
            ["_" (cm-error "CONTRACT" "Only plain wildcard is acceptable in match expr.")]
            [label 
              (match-var label (list (symbol->string op)) v match-context)])]
         [(Prefix2 'types types-expr (? var-container? var)) 
          (match (get-var-label var context params module-id debug)
            ["_" (cm-error "CONTRACT" "Only plain wildcard is acceptable in match expr.")]
            [label 
              (match-var
                label
                (check-types-list (trace-interp-expr types-expr context params module-id debug))
                v match-context)])]
         [(Prefix2 'struct (? var-container? var) lst) 
          #:when (expr-is-list? lst)
          (match (get-var-label var context params module-id debug)
            ["_" (cm-error "CONTRACT" "Only plain wildcard is acceptable in match expr.")]
            [label 
              (match v
                [(CmStruct label2 lst2) 
                 #:when (equal? label label2)
                 (match-expr lst lst2 match-context context params module-id debug)]
                [_ #f]
            )])]

         ;; a, ...
         [(Prim2 'cons (? var-container? var) (Prim0 '...))
          ;; just return everything
          (match-var 
            (get-var-label var context params module-id debug) 
            (list "dynamic") (value->list v) match-context
            )]
         [(Prim2 'cons _ (Prim0 '...))
          (cm-error "SYNTAX" "Improperly formed ellipsis in match expr.")]
         ;; a, ..., b; and such
         [(Prim2 'cons (? var-container? var) (Prim2 'cons (Prim0 '...) es))
          ;; get all elems within this range
          (let ([num (- (value-length v) (count-match-elems es))])
            (match/values (match-ellipsis v num)
              ;; might have matched but ran out of values
              [(#f _) #f]
              ;; did not match
              [(_ #f) #f]
              ;; (items-yet-to-match items-matched)
              [(remaining res) 
               (let ([match-context 
                       (match-var 
                        (get-var-label var context params module-id debug) 
                        (list "dynamic") res match-context)])
                 (if (equal? match-context #f) #f
                   (match-expr 
                     es remaining match-context
                     context params module-id debug)))]))]
         [(Prim2 'cons _ (Prim2 'cons (Prim0 '...) _))
          (cm-error "SYNTAX" "Improperly formed ellipsis in match expr.")]

         [(Prim2 'cons e1 e2)
          (match v
                 [(cons v1 v2)
                    (let ([mc2 (match-expr e1 v1 match-context context params module-id debug)])
                        (if (not (equal? mc2 #f)) 
                          (match-expr e2 v2 mc2 context params module-id debug) #f))]
                 [_ #f])]

         [_ (cm-error "SYNTAX" "Invalid syntax for `match`.")]))

;; Checks that types of var and value match (if they don't returns false).
;; If types match then returns modified context with itself included
;;
;; string, string list, value, hash -> hash | #f
(define (match-var id types v match-context)
  (if (ormap (lambda (elem) (is-type? elem v)) types)
    ;; check if we have already seen the var
    (if (hash-has-key? match-context id)
      (if (deep-equal? v (get-local-var-data id match-context))
        ;; successful match
        match-context
        ;; does not match earlier var, failed match
        #f
        )
      (set-local-var id v match-context)
      )
    ;; false if types don't match
    #f
    ))

;; matches `num` elements from `vs`
;;
;; the first arg is the remaining values to match
;; the second arg is the items matched by the ellipsis
;;
;; if there are no more elements to match then the first arg is #f
;; if there was a match failure then the second arg is #f
;;
;; value, integer -> (values value, value)
(define (match-ellipsis vs num)
  (let aux ([vs vs] [acc '()] [num num])
    (cond
      [(zero? num) (values vs (reverse acc))]
      [else 
        (match vs
          [(cons v vs) (aux vs (cons v acc) (sub1 num))]
          [v #:when (= 1 num) (values #f (reverse (cons v acc)))]
          [_ (values #f #f)])])))

;; counts the number of elements in `es`.
;; Raises an exception if a `...` is found
(define (count-match-elems es)
  (let aux ([es es] [count 1])
    (match es
      [(Prim2 'cons (Prim0 '...) _)
       (cm-error "SYNTAX" "Cannot have duplicate ellipsis in match-expr.")]
      [(Prim0 '...)
       (cm-error "SYNTAX" "Cannot have duplicate ellipsis in match-expr.")]
      [(Prim2 'cons _ es) (aux es (add1 count))]
      [_ count])))

(define (interp-case e1 e2 e3 context params module-id debug)
  (match e2
         [(Yields e2-2)
          (match e3
                 [(Case e3-1 e3-2 e3-3) 
                  (if (bool-to-racket 
                        (trace-interp-expr e1 context params module-id debug)) 
                    (trace-interp-expr e2-2 context params module-id debug)
                    (interp-case e3-1 e3-2 e3-3 context params module-id debug))]
                 [(Else e3-3)
                  (if (bool-to-racket (trace-interp-expr e1 context params module-id debug))
                    (trace-interp-expr e2-2 context params module-id debug)
                    (trace-interp-expr e3-3 context params module-id debug))]
                 [_ (cm-error "SYNTAX" "Case must end in else or another case.")]
                 )]
         [_ (cm-error "SYNTAX" "Case is missing yields.")]))

(define (interp-var label context params module-id)
  (cond
    ;; first local context
    [(hash-has-key? context label) (get-local-var-data label context)]
    ;; then global context
    [else (match (get-global-var-data label module-id)
      [#f 
       (cond 
        ;; then params
        [(hash-has-key? params label) (get-local-var-data label params)]
        [else (cm-error "UNDEFINED" 
                 (format "Var \"~a\" has not yet been defined." label))])]
      [res res])]))

 
(define (interp-get-param var context params module-id debug)
  (match var
    [(? var-container?)
     (let ([label (get-var-label var context params module-id debug)])
       (match (get-local-var-data label params)
         [#f 
          (cm-error 
            "UNDEFINED"
            (format "Could not find parameter: \"~a\"" label))]
         [res res]))]
    [_ 
      (cm-error 
        "CONTRACT" 
        (format "`get_param` requires a variable argument.\ngot: ~a" var))]))


;; gets the label from a left-side expr (such as `int x` in (def int x := 3)),
;; checks it against the given value,
;; then returns the guard type(s) and label. If the guard is improperly formed, then
;; the procedure yields (values #f #f).
;;
;; If the guard is a null expr then (values '() '()) is returned
;;
;; guard-expr, context, string, debug ->
;;      (values (string | fun) list, (string | null)) | (values bool, bool)
(define (interp-left-hand-expr e context params module-id debug)
  (match e
      [(Null) (values '() '())]
      ;; function guard
      [(Prefix2 '? (? var-container? var) expr) 
       (let ([func (trace-interp-expr expr context params module-id debug)]
             [label (get-var-label var context params module-id debug)])
         (unless (is-fun? func) 
           (cm-error "CONTRACT" 
             (format "Var `~a`: Argument to `?` must be a function." label)))
         (values (list func) label))]
      [(Prim1 op (? var-container? var)) #:when (member op guard-types)
       (values (list (symbol->string op)) 
               (get-var-label var context params module-id debug))]
      [(Prefix2 'types types-expr (? var-container? var))
       (let ([types-list 
               (check-types-list (trace-interp-expr types-expr context params module-id debug))]) 
           (values types-list (get-var-label var context params module-id debug)))]
      [(Prefix2 'struct (? var-container? s-var) (? var-container? var))
       (let ([guard-type 
               (get-struct-type-string (get-var-label s-var context params module-id debug))])
           (values (list guard-type) (get-var-label var context params module-id debug)))]
      ;; implied dynamic case
      [(? var-container? var) 
       (values (list "dynamic") 
         (get-var-label var context params module-id debug))]
      [_ (values #f #f)]))

;; turns a improper list expr of left-hand-exprs into guard-types and labels
;; yields (values #f #f) if the arg-exprs are incorrectly formated
;;
;; (Prim2 'cons (Prim1 'int (Var "x")) (Prim1 'float (Var "y"))) -> 
;;   (values (list (list "float") (list "int")) (list "x" "y"))
;;
;; expr -> (values ((string | fun) list list) (string list)) | (values #f #f)
(define (interp-left-hand-exprs args-e context params module-id debug)
  (match args-e
    [(Prim2 'cons e es) 
     (match/values (interp-left-hand-exprs es context params module-id debug)
        [(#f #f) (values #f #f)]
        [(guard-types/lst label/lst)
         (match/values (interp-left-hand-expr e context params module-id debug)
           [(guard-types label) 
            (values (cons guard-types guard-types/lst) (cons label label/lst))]
        )])]
    [e 
      (match/values (interp-left-hand-expr e context params module-id debug)
        [(#f #f) (values #f #f)]
        [(guard-types label) (values (cons guard-types '()) (cons label '()))]
        )]
    ))

;; op-type = "def" | "set" | "static"
(define (interp-def guard-types label v op-type context params module-id debug)
 (match label
      [#f #:when (equal? op-type "set")
        (cm-error "SYNTAX" "Unknown Item on left hand of `set`.")] 
      [#f #:when (equal? op-type "static") 
        (cm-error "SYNTAX" "Unknown Item on left hand of `static`.")] 
      [#f (cm-error "SYNTAX" "Unknown Item on left hand of `def`.")]
      ['() (cm-error "SYNTAX" "`def` is missing a variable.")]
      ;; sets var in global context hash and returns value to caller
      [_
        (assign-type-check guard-types v params label)
        (match op-type
          ;; `set`
          ["set"
            (update-global-var! label v module-id)]
          ;; `static`
          ["static"
            (set-global-var! label v (var-name-private? label) #t module-id)]
          ;; `def`
          ["def"
            (set-global-var! label v (var-name-private? label) #f module-id)]
          )
       v]))
         
(define (interp-let e1 e2 e3 context params module-id debug)
  (let-values ([(v2) (trace-interp-expr e2 context params module-id debug)] 
               [(guard-types label)
                (interp-left-hand-expr e1 context params module-id debug)])
    (match label
         [#f (cm-error "SYNTAX" "Unknown Item on left hand of `let`.")]
         ['() (cm-error "SYNTAX" "`let` is missing a variable.")]
         [_ 
            (assign-type-check guard-types v2 params label)
            (trace-interp-expr 
              e3 (set-local-var label v2 context) params module-id debug)])))

(define (interp-param e1 e2 e3 context params module-id debug)
  (let-values ([(v2) (trace-interp-expr e2 context params module-id debug)] 
               [(guard-types label)
                (interp-left-hand-expr e1 context params module-id debug)])
    (match label
         [#f (cm-error "SYNTAX" "Unknown Item on left hand of `param`.")]
         ['() (cm-error "SYNTAX" "`param` is missing a variable.")]
         [_ 
            (assign-type-check guard-types v2 params label)
            (trace-interp-expr 
              e3 context (set-local-var label v2 params) module-id debug)])))

(define (interp-letrec label args-e body-e in-e context params module-id debug)
  (match (interp-lambda-list args-e body-e)
    [(Lambda args-e2 (Yields body-e2))
     (let ([f 
            (interp-lambda args-e2 body-e2
                           label context params module-id debug)])
       (trace-interp-expr 
         ;; add reference to `f` in both local context and as a param
         in-e (set-local-var label f context) 
         (set-local-var label f params) module-id debug)
       )]))

(define (interp-letaux label e context params module-id debug)
  (let-values ([(args-e body-e in-e) (interp-letaux-aux e)])
    (interp-letrec 
      label args-e 
      ;; auto let bind for convenience
      (Let (Var label) (Assign (Prim1 'get_param (Var label))) (In in-e))
      ;; `in` section of `letrec` will just 
      ;;   apply the arguments onto the parameters
      (Prefix2 'appl (Prim1 'get_param (Var label)) body-e)
      context params module-id debug)))

;; parses the case portion of `letaux`
;; when label = "f"
;; `| int x := 3 | y := "a" in x + y`
;; becomes
;; (values 
;;    (Prim2 'cons (Prim1 'int (Var "x")) (Var "y")) 
;;    (Prim2 'cons (Int 3) (Prim2 'cons (String "a") (Null)))
;;    (Prim2 'add (Var "x") (Var "y"))
;;      )
;;
;; raises an exception if the expr is improperly formated
;;
;; expr -> (values expr expr expr)
(define (interp-letaux-aux e)
  (match e
    [(Case e1 (Assign e2) e3)
     (match/values (interp-letaux-aux e3)
        ;; first ret val must be an incomplete list
        [((Null) e2-2 e3-2)
         (values e1
                 (Prim2 'cons e2 e2-2)
                 e3-2)]
        [(e1-2 e2-2 e3-2)
         (values (Prim2 'cons e1 e1-2)
                 (Prim2 'cons e2 e2-2)
                 e3-2)]
        )]
    [(In e1)
     (values (Null)
             (Null)
             e1)]
    [_ (cm-error "SYNTAX" "Improperly formed `letaux`.")]
    ))

;; name is the name of the function and is either a string 
;; or '() for anonymous functions
(define (interp-lambda e1 e2 name context params module-id debug)
  (let-values ([(guard-types label) 
                 (interp-left-hand-expr e1 context params module-id debug)])
    (match label
           [#f (cm-error "SYNTAX" "Unknown Item on left hand of `lambda`")]
           ['() (Fun name '() '() context e2 module-id debug)]
           [_ (Fun name label guard-types context e2 module-id debug)]
    )))

(define (interp-apply v1 v2 params)
  (let ([trace
          (lambda (name id debug)
            (push-elem-to-trace-stack! 
              (TraceElem "fun" (if (null? name) "lambda" name)
                    id (debug->linenum debug))))])
  (match v1
    ;; no arg lambda
    [(Fun name '() '() fcontext fexpr fmodule-id fdebug)
     (cm-error "CONTRACT" 
        (format "Attempted to apply argument onto null-arg lambda: ~a\ngot: ~a" name v2))]
    [(Fun name var types fcontext fexpr fmodule-id fdebug)
     ;; trace
     (trace name fmodule-id fdebug)

     ;; check val against guards
     (assign-type-check types v2 params var)
     (trace-interp-expr fexpr (set-local-var var v2 fcontext) params fmodule-id fdebug)]
    [_ (cm-error "CONTRACT" "Attempted to apply onto a non function.")])))

(define (interp-null-apply v params)
  (let ([trace
          (lambda (name id debug)
            (push-elem-to-trace-stack! 
              (TraceElem "fun" (if (null? name) "lambda" name)
                    id (debug->linenum debug))))])
  (match v
    ;; no arg lambda
    [(Fun name '() '() fcontext fexpr fmodule-id fdebug)
     ;; trace
     (trace name fmodule-id fdebug)

     (trace-interp-expr fexpr fcontext params fmodule-id fdebug)]
    [(Fun name var types fcontext fexpr fmodule-id fdebug)
     (cm-error "CONTRACT" 
        (format "Attempted to null-apply non-null arg lambda: ~a" name))]
    [_ (cm-error "CONTRACT" "Attempted to apply onto a non function.")])))


(define (interp-appl e1 e2 context params module-id debug)
  (let ([v1 (trace-interp-expr e1 context params module-id debug)]
        [v2 (trace-interp-expr e2 context params module-id debug)])
    (assert-contract (list "list") v2 params "appl")
    (match v2
      ['() (interp-null-apply v1 params)]
      [_ 
        (foldl 
          (lambda (elem acc) 
            (interp-apply acc elem params))
          v1 v2)])))

(define (interp-typedef e1 e2 context params module-id debug)
 (let ([lst2 
         (match (ast-cons-to-racket e2)
           [(? list? res) res] 
           [_ (cm-error 
                "SYNTAX" 
                "Invalid schema for `typedef`. Schema must be a list.")])]
       [id 
         (match e1
           [(? var-container? var)
            (get-var-label var context params module-id debug)]
           [_ (cm-error "SYNTAX" "Missing Label for `typedef`.")]
           )])
  (let verify-schema ([lst lst2] [acc '()] [acc/labels '()])
   (match lst
      ;; set schema if no errors were found
      ['() (set-type! id (reverse acc) (var-name-private? id) module-id) (Prim0 'void)]
      [(cons h t)
       (let-values 
         ([(guard-types label) (interp-left-hand-expr h context params module-id debug)])
         (match label
          [#f (cm-error "SYNTAX" 
           (format "Unknown element ~a inside `typedef` schema." h))]
          [_
            (when (member label acc/labels) 
              (cm-error 
                "CONTRACT" 
                (format 
                  (string-append "struct ~a: Cannot have duplicate "
                                 "identifier within `typedef`: \"~a\"") 
                  id label)))
            (verify-schema t 
              (cons (SchemaElem guard-types label) acc)
              (cons label acc/labels))]))]))))

(define (interp-struct e1 e2 context params module-id debug)
  (match e1
         [(? var-container? var)
          (let ([label (get-var-label var context params module-id debug)])
          (match (trace-interp-expr e2 context params module-id debug)
                 ;; struct args must be a list (null for no args, still technically a list)
                 [res #:when (list? res) 
                      (match (get-type-data label module-id)
                             [#f (cm-error "UNDEFINED" (format "Type \"~a\" has not been declared." label))]
                             [schema
                            (match (valid-against-schema? label schema res params)
                                   [#t (CmStruct label res)]
                                   [#f (cm-error "CONTRACT" 
                (format (string-append "Could not validate struct against type schema:"
                                       "\nstruct:\n~a ~a\nschema:\n~a")
                        label (string-coerce res) (struct-schema->string (get-type-data label module-id))))])])]
                 [_ (cm-error "SYNTAX" "Arguments to `struct` must be a list or null.")]))]
         [_ (cm-error "SYNTAX" "Missing label for `struct`.")]))

(define (interp-struct? e1 e2 context params module-id debug)
  (match e1
   [(? var-container? var) 
    (let ([label (get-var-label var context params module-id debug)])
      (match label
        ;; wildcard accepts any struct
        ["_" 
         (racket-to-bool 
           (is-struct? (trace-interp-expr e2 context params module-id debug)))]
        [_ 
           (racket-to-bool 
             (is-struct-type? 
               label
               (trace-interp-expr e2 context params module-id debug)))]))]
   [_ (cm-error "SYNTAX" "Missing label for struct question.")]))

(define (interp-index e1 e2 context params module-id debug)
  (match (trace-interp-expr e1 context params module-id debug)
         [vs #:when (list? vs) 
             (match (trace-interp-expr e2 context params module-id debug)
                    [is #:when (and (list? is) (= (length is) 2) 
                                    (integer? (list-ref is 0))
                                    (integer? (list-ref is 1))) 
                        (match is
                               [(list i1 i2) #:when (and (<= i1 i2)
                                                         (>= i1 0)
                                                         (<= i2 (length vs)))
                                              (take (drop vs i1) (- i2 i1))]
                               [_ (cm-error 
                                "CONTRACT" (format "Index ~a is out of range for ~a" 
                                                   (string-coerce is) (string-coerce vs)))]

                          )]
                    [i #:when (integer? i) 
                       (if (and (< i (length vs)) (>= i 0))
                        (list-ref vs i)
                        (cm-error "CONTRACT" (format "Index ~a is out of range for ~a" 
                                                (string-coerce i) (string-coerce vs))))]
                    [i (cm-error "CONTRACT" (format "Invalid index: ~a" (string-coerce i)))]
                    )

               ]
         [s #:when (string? s) 
             (match (trace-interp-expr e2 context params module-id debug)
                    [is #:when (and (list? is) (= (length is) 2) 
                                    (integer? (list-ref is 0))
                                    (integer? (list-ref is 1))) 
                        (match is
                               [(list i1 i2) #:when (and (<= i1 i2)
                                                         (>= i1 0)
                                                         (<= i2 (string-length s)))
                                             (substring s i1 i2)]
                               [_ (cm-error "CONTRACT" (format "index ~a is out of range for ~a" 
                                                               (string-coerce is) (string-coerce s)))]

                          )]
                    [i #:when (integer? i) 
                       (if (and (< i (string-length s)) (>= i 0))
                        (substring s i (add1 i))
                        (cm-error "CONTRACT" (format "index ~a is out of range for ~a" 
                                                     (string-coerce i) (string-coerce s))))]
                    [i (cm-error "CONTRACT" (format "Invalid index ~a for ~a" 
                                                    (string-coerce i) (string-coerce s)))]
                    )]
         [h #:when (is-hash? h) 
            (interp-hash-ref h (trace-interp-expr e2 context params module-id debug) params)]
         [(CmStruct label lst)
            (let ([index
                    (match e2 
                      [(? var-container? var)
                       (get-var-label var context params module-id debug)]
                      [_
                       (cm-error "SYNTAX" 
                        (format "Index for struct ~a must be a variable." label))])]
                  [elems (get-struct-schema-labels label module-id)])
              (match (index-of elems index)
                [#f (cm-error "CONTRACT" 
                        (format "Invalid index ~a for struct ~a" index label))]
                [int-index (list-ref lst int-index)]))]
         [s (cm-error "CONTRACT" (format "Invalid operand for index: ~a" (string-coerce s)))]
         ))

;;
;; hash ops
;;

(define (interp-make-hash as)
  (match as
    [(list "mutable") (CmHash (make-hash) "mutable" '())]
    [(list "immutable") (CmHash (hash) "immutable" '())]
    [(list "immutable" f) #:when (is-fun? f) (CmHash (hash) "immutable" f)]
    [(list "mutable" f) #:when (is-fun? f) (CmHash (make-hash) "mutable" f)]
    [(list (CmHash _ "immutable" _)) (car as)]
    ;; deep copy of mutable hash
    [(list (CmHash h "mutable" handler)) (CmHash (hash-copy h) "mutable" handler)]
    [_ (cm-error "CONTRACT" "Invalid arg(s) to `make_hash`.")]
    ))

(define (interp-hash-set v1 v2 v3)
  (match v1
        [(CmHash h "immutable" handler) (CmHash (hash-set h v2 v3) "immutable" handler)]
        [(CmHash h "mutable" handler) (hash-set! h v2 v3) (Prim0 'void)]
        [_ (cm-error "CONTRACT" "First argument to `hash_set` must be a hash.")]
        ))

(define (interp-hash-ref v1 v2 params)
  (match v1
         [(CmHash h _ '()) (hash-ref h v2 
                    (lambda () (cm-error "HASHREF" 
                        (format "Could not find key ~a in hash." (string-coerce v2)))))]
         [(CmHash h _ handler) (hash-ref h v2
                    (lambda () (interp-apply handler v2 params)))]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_ref`.")]))

(define (interp-hash-has-key? v1 v2)
  (match v1
         [(CmHash h _ _) (racket-to-bool (hash-has-key? h v2))]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_has_key?`.")]))

(define (interp-hash-remove v1 v2)
  (match v1
         [(CmHash h "immutable" handler) 
          (CmHash (hash-remove h v2) "immutable" handler)]
         [(CmHash h "mutable" _) 
          (hash-remove! h v2)
          (Prim0 'void)]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_remove`.")]))

(define (interp-hash-size v1)
  (match v1
         [(CmHash h _ _) (hash-count h)]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_size`.")]))

(define (interp-hash-keys v1)
  (match v1
         [(CmHash h _ _) (hash-keys h)]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_keys`.")]))

(define (interp-hash-values v1)
  (match v1
         [(CmHash h _ _) (hash-values h)]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_values`.")]))

(define (interp-hash-to-list v1)
  (match v1
         [(CmHash h _ _) (hash->list h)]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_to_list`.")]))

(define (interp-handler.hash-ref v1 v2 v3 params)
  (match v1
         [(CmHash h _ _)
            (if (is-fun? v3)
                (hash-ref h v2
                    (lambda () (interp-apply v3 v2 params)))
                (cm-error "CONTRACT" "Third arg to `hash_ref_check` must be a function."))]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_ref_check`.")]))

(define (interp-hash? v params)
  (match v
    [(CmHash _ _ _) (racket-to-bool #t)]
    [_ (racket-to-bool #f)]))

(define (interp-mutable-hash? v params)
  (match v
    [(CmHash _ "mutable" _) (racket-to-bool #t)]
    [_ (racket-to-bool #f)]))

;;
;; system ops
;;

(define (interp-exit v params)
  ;; exit with 1 if error code is screwy
  (let ([i (match v
             [(? is-int?) #:when (and (>= v 0) (<= v 255)) v]
             [_ 1])])
    (exit i)))

(define (interp-load prefix arg params module-id)
  (process-import arg prefix module-id)
  (Prim0 'void))

(define (interp-error as params)
  (match as
    [(list id msg) (cm-error id msg)]
    [(list msg) #:when (string? msg) (cm-error "GENERIC" msg)]
    [_ (cm-error "CONTRACT" (format "Invalid argument(s) to error: ~a" as))]))

(define (interp-system-type)
  (symbol->string (system-type)))

(define (interp-system v params)
  (assert-contract (list "string") v params "system")
  (match v
    [_ (racket-to-bool (system v))]))

(define (interp-sysres v params)
  (assert-contract (list "string") v params "sysres")
  (match v
    [_ (with-output-to-string (lambda () (system v)))]))

(define (interp-eval v params)
  (assert-contract (list "string") v params "eval")
  (run v))

(define (interp-evalxp v params)
  (assert-contract (list "string") v params "evalxp")
  (run-expr v))

;;
;; type conversions
;;

(define (interp-int-to-char v params)
  (assert-contract (list "int") v params "int_to_char")
  (with-handlers* 
   ([exn:fail? 
      (lambda (exn) 
        (cm-error "CONTRACT"
         (format "`int_to_char`: invalid value: ~a" v)))])
   (string (integer->char v))))

(define (interp-char-to-int v params)
  (assert-contract (list "string") v params "char_to_int")
  (match (string->list v)
    [(list c) (char->integer c)]
    [_ (cm-error "CONTRACT" "Argument to `char_to_int` must be a single char")]))

;;
;; file/io ops
;;

(define (interp-print v)
 (begin (displayln (string-coerce v)) 
        v))

;; reads until i chars or eof
(define (interp-read-string v1 params)
  (assert-contract (list "int") v1 params "read_string")
  (unless (>= v1 0) 
    (cm-error "CONTRACT" 
        (format "Argument to `read_string` must be greater than zero.\nGot: ~a" v1)))
  (read-string v1))


(define (interp-appendstrf v1 v2 params)
  (assert-contract (list "string") v1 params "appendstrf")
  (assert-contract (list "string") v2 params "appendstrf")
  (match (cons v1 v2) 
    [(cons v1 v2) #:when (or (string=? v1 "") (string=? v2 "")) 
                  (cm-error "CONTRACT" "Missing argument to `appendstrf`.")]
    [(cons v1 v2)
     (with-handlers* 
       ([exn:fail? (lambda (exn) 
         (cm-error "SYSTEM" (format "Could not append to file \"~a\"" v2)))])
       (display-to-file v1 v2 #:exists 'append))
     (Prim0 'void)]))

(define (interp-writestrf v1 v2 params)
  (assert-contract (list "string") v1 params "writestrf")
  (assert-contract (list "string") v2 params "writestrf")
  (match (cons v1 v2) 
   [(cons v1 v2) #:when (or (string=? v1 "") (string=? v2 "")) 
                 (cm-error "CONTRACT" "Missing argument to `writestrf`.")]
   [(cons v1 v2)
    (with-handlers* 
      ([exn:fail? (lambda (exn) 
        (cm-error "SYSTEM" (format "Could not write to file \"~a\"" v2)))])
      (display-to-file v1 v2 #:exists 'replace))
    (Prim0 'void)]))

(define (interp-getlinesf v params)
  (assert-contract (list "string") v params "getlinesf")
  (match v
    ["" (cm-error "CONTRACT" "Missing argument to `getlinesf`.")]
    [_ (try-with-error "SYSTEM" (format "getlinesf: Could not load file \"~a\"." v)
                            file->lines (list v))]))

(define (interp-ls v params)
  (assert-contract (list "string") v params "ls")
  (match v
    ["" (map path->string 
              (try-with-error "GENERIC" "ls: Could not load directory \"\"."
                              directory-list (list ".")))]
    [_ (map path->string 
              (try-with-error "GENERIC" (format "ls: Could not load directory \"~a\"." v)
              directory-list (list v)))]))

(define (interp-cd v params)
  (assert-contract (list "string") v params "cd")
  (match v
    ["" (path->string 
          (try-with-error "SYSTEM" "cd: Could not load directory \".\"."
                          current-directory '()))]
    [_ 
      (try-with-error "SYSTEM" (format "cd: Could not load directory \"~a\"." v)
                           current-directory (list v)) (Prim0 'void)]))

(define (interp-mv v1 v2 params)
  (assert-contract (list "string") v1 params "mv")
  (assert-contract (list "string") v2 params "mv")
  (when (or (equal? v1 "") (equal? v2 ""))
    (cm-error "CONTRACT" "Cannot move to or from empty directory."))
  (try-with-error "SYSTEM" 
        (format "mv: Could not move directory \"~a\" to \"~a\"." v1 v2)
        rename-file-or-directory (list v1 v2)) 
  (Prim0 'void))

(define (interp-cp v1 v2 params)
  (assert-contract (list "string") v1 params "cp")
  (assert-contract (list "string") v2 params "cp")
  (match (cons v1 v2) 
    [(cons v1 v2) 
     #:when (or (string=? v1 "") (string=? v2 "")) 
     (cm-error "CONTRACT" "Cannot move to or from empty directory.")]
    [(cons v1 v2)
     (with-handlers* 
      ([exn:fail? (lambda (exn) 
       (cm-error "SYSTEM" (format "Could not copy \"~a\" to \"~a\"" v1 v2)))])
      (copy-directory/files v1 v2))
     (Prim0 'void)]))

(define (interp-rm v params)
  (assert-contract (list "string") v params "rm")
  (match v
    ["" (cm-error "CONTRACT" "Missing argument to rm")]
    [_ 
      (try-with-error "SYSTEM" (format "rm: Could not delete file or directory \"~a\"." v)
                          delete-directory/files (list v)) (Prim0 'void)]))
(define (interp-mkdir v params)
  (assert-contract (list "string") v params "mkdir")
  (match v
        ["" (cm-error "CONTRACT" "Missing argument to `mkdir`.")]
        [_ 
          (try-with-error "SYSTEM" (format "mkdir: Could not make directory \"~a\"." v)
                             make-directory (list v)) (Prim0 'void)]))

(define (interp-expand-path v params)
  (assert-contract (list "string") v params "expand_path")
  (match v
    ["" ""]
     ;(cm-error "CONTRACT" "`expand_path`: string argument cannot be empty.")]
    [_ (path->string 
         (simplify-path 
           (path->complete-path (expand-user-path v))))]))

(define (interp-directory? v params)
  (assert-contract (list "string") v params "directory?")
  (match v
    ["" (racket-to-bool #f)]
    [v1 (racket-to-bool (directory-exists? v1))]))

(define (interp-file? v params)
  (assert-contract (list "string") v params "file?")
  (match v
    ["" (racket-to-bool #f)]
    [v1 (racket-to-bool (file-exists? v1))]))

(define (interp-read-line) (read-line))

(define (interp-write-string v1 params)
  (assert-contract (list "string") v1 params "write_string")
  (display v1) (Prim0 'void))

(define last-gensym-id 0)
(define (interp-gensym v1 params)
  (assert-contract (list "string") v1 params "gensym")
  (let ([id last-gensym-id])
    (set! last-gensym-id (add1 id))
    (string-append v1 (number->string id))
    ))

;; `id` := the variable name
;; `module` := the module to look in
;; `space` := "local" | "global" | "params" | "all"
;;      where `local` is the local context
;;            `global` is the global context
;;            `params` is the param context
;;            `all` is any context
(define (interp-defined? id module space context params) 
  (assert-contract (list "string") id params "defined?")
  (assert-contract (list "string") module params "defined?")
  (assert-contract (list "string") space params "defined?")
  (match space
    ["local"
     (racket-to-bool (hash-has-key? context id))]
    ["params"
     (racket-to-bool (hash-has-key? params id))]
    ["global"
     (let ([res (get-global-var-data id module)])
       (racket-to-bool (not (not res))))]
    ["all"
     (racket-to-bool 
       (or 
         (bool-to-racket (interp-defined? id module "local" context params))
         (bool-to-racket (interp-defined? id module "params" context params))
         (bool-to-racket (interp-defined? id module "global" context params))))]
    [_ (cm-error "CONTRACT" 
                 (format "Invalid space for `defined?`: ~a" space))]))


(define (interp-schemaof e1 context params module-id debug)
  (match e1
    [(? var-container? var)
     (get-struct-schema-labels
       (get-var-label var context params module-id debug) module-id)]
    [_ (cm-error "CONTRACT" "Argument to `schemaof` must be a variable.")]
    ))

(define (interp-regex v1 params)
  (assert-contract (list "list") v1 params "regex")
   ;; the second argument to `regex` takes a list of the form (type, regex;),
   ;; where type = "quote" | "px" | "rx",
   ;; and regex is the actual regex.
   ;;
   ;; "quote" means the regex will be formed using `regexp-quote`
   ;; in racket
  (let ([pattern 
          (match v1 
             [(list type pattern args ...)
              (assert-contract (list "list") pattern params
                        (format "regex -> ~a" type))
              (with-handlers* 
                ([exn:fail?
                 (lambda (e) 
                    (match e
                      [(exn:fail msg _)
                         (cm-error "GENERAL" msg)])
                    )])
                (match pattern
                  [(list "quote" regex) (regexp-quote regex)]
                  [(list "rx" regex) (regexp regex)]
                  [(list "px" regex) (pregexp regex)])
                )]
             [_ (cm-error "CONTRACT" "Invalid arguments to `regex`.")])])
  (match v1
         [(list "regexp-match" _ str)
          (assert-contract (list "string") str params "regex -> regexp-match")
          (match (regexp-match pattern str)
                 [(? boolean? res) (racket-to-bool res)]
                 [res res])
          ]
         [(list "regexp-match*" _ str)
          (assert-contract (list "string") str params "regex -> regexp-match*")
          (match (regexp-match* pattern str #:match-select values)
                 [res res])
          ]
         [(list "regexp-match?" _ str)
          (assert-contract (list "string") str params "regex -> regexp-match?")
          (racket-to-bool (regexp-match? pattern str))]
         [(list "regexp-split" _ str)
          (assert-contract (list "string") str params "regex -> regexp-split")
          (match (regexp-split pattern str)
                 [res res])
          ]
         [(list "regexp-replace" _ str insert)
          (assert-contract (list "string") str params "regex -> regexp-replace")
          (assert-contract (list "string") insert params "regex -> regexp-replace")
          (match (regexp-replace pattern str insert)
                 [res res])
          ]
         [(list "regexp-replace*" _ str insert)
          (assert-contract (list "string") str params "regex -> regexp-replace")
          (assert-contract (list "string") insert params "regex -> regexp-replace")
          (match (regexp-replace* pattern str insert)
                 [res res])
          ]
         [_ (cm-error "CONTRACT" "Invalid arguments to `regex`.")]
         )))

(define (interp-random v)
  (match v
    ['() (random)]
    [(list (? integer? k))
     #:when (> k 0)
     (random k)]
    [(list (? integer? n1) (? integer? n2))
     #:when (> n2 n1)
     (random n1 n2)]
    [(list "seed" (? integer? k))
     #:when (and (> k 0) (< k (sub1 (expt 2 31))))
     (random-seed k) (Prim0 'void)]
    [_ (cm-error "CONTRACT" 
        (format "Invalid arguments to `random`: ~a" (string-coerce v)))]))


;; processes lesser operators such as `regex` and `make_hash` that do
;; not deserve their own operators
(define (interp-internal-ops v1 v2 context params module-id debug)
  ;; Asserts that the list argument to internal_op is a list and a list
  ;; of the correct length.
  ;;
  ;; The number of args should be [n1 n2]
  (define (assert-num-args n1 n2)
    (match v2
      [(list as ...)
       #:when (and (>= (length as) n1) (<= (length as) n2))
       (void)]
      [_ (cm-error "CONTRACT" (format "Invalid args to `~a`: ~a" v1 v2))]))
  (assert-contract (list "string") v1 params "internal_op")
  (assert-contract (list "list") v2 params "internal_op")
  (match v1
    ["regex" 
     (interp-regex v2 params)]
    ["random"
     (interp-random v2)]
    ["gensym"
     (assert-num-args 1 1)
     (interp-gensym (car v2) params)]
    ["expand_path"
     (assert-num-args 1 1)
     (interp-expand-path (car v2) params)]
    ["directory?"
     (assert-num-args 1 1)
     (interp-directory? (car v2) params)]
    ["file?"
     (assert-num-args 1 1)
     (interp-file? (car v2) params)]
    ["ls"
     (assert-num-args 1 1)
     (interp-ls (car v2) params)]
    ["cd"
     (assert-num-args 1 1)
     (interp-cd (car v2) params)]
    ["mv"
     (assert-num-args 2 2)
     (interp-mv (car v2) (cadr v2) params)]
    ["cp"
     (assert-num-args 2 2)
     (interp-cp (car v2) (cadr v2) params)]
    ["rm"
     (assert-num-args 1 1)
     (interp-rm (car v2) params)]
    ["mkdir"
     (assert-num-args 1 1)
     (interp-mkdir (car v2) params)]
    ["writestrf"
     (assert-num-args 2 2)
     (interp-writestrf (car v2) (cadr v2) params)]
    ["appendstrf"
     (assert-num-args 2 2)
     (interp-appendstrf (car v2) (cadr v2) params)]
    ["getlinesf"
     (assert-num-args 1 1)
     (interp-getlinesf (car v2) params)]
    ["make_hash"
     (interp-make-hash v2)]
    ["hash_ref"
     (assert-num-args 2 2)
     (interp-hash-ref (car v2) (cadr v2) params)]
    ["handler.hash_ref"
     (assert-num-args 3 3)
     (interp-handler.hash-ref (car v2) (cadr v2) (caddr v2) params)]
    ["hash_set"
     (assert-num-args 3 3)
     (interp-hash-set (car v2) (cadr v2) (caddr v2))]
    ["hash_keys"
     (assert-num-args 1 1)
     (interp-hash-keys (car v2))]
    ["hash_values"
     (assert-num-args 1 1)
     (interp-hash-values (car v2))]
    ["hash_to_list"
     (assert-num-args 1 1)
     (interp-hash-to-list (car v2))]
    ["hash_has_key?"
     (assert-num-args 2 2)
     (interp-hash-has-key? (car v2) (cadr v2))]
    ["hash_remove"
     (assert-num-args 2 2)
     (interp-hash-remove (car v2) (cadr v2))]
    ["hash_size"
     (assert-num-args 1 1)
     (interp-hash-size (car v2))]
    ["hash?"
     (assert-num-args 1 1)
     (interp-hash? (car v2) params)]
    ["mutable_hash?"
     (assert-num-args 1 1)
     (interp-mutable-hash? (car v2) params)]
    ["system_type"
     (assert-num-args 0 0)
     (interp-system-type)]
    ["system"
     (assert-num-args 1 1)
     (interp-system (car v2) params)]
    ["sysres"
     (assert-num-args 1 1)
     (interp-sysres (car v2) params)]
    ["write_string"
     (assert-num-args 1 1)
     (interp-write-string (car v2) params)]
    ["read_string"
     (assert-num-args 1 1)
     (interp-read-string (car v2) params)]
    ["read_line"
     (assert-num-args 0 0)
     (interp-read-line)]
    ["eval"
     (assert-num-args 1 1)
     (interp-eval (car v2) params)]
    ["evalxp"
     (assert-num-args 1 1)
     (interp-evalxp (car v2) params)]
    ["exit"
     (assert-num-args 1 1)
     (interp-exit (car v2) params)]
    ["error"
     (interp-error v2 params)]
    ["int_to_char"
     (assert-num-args 1 1)
     (interp-int-to-char (car v2) params)]
    ["char_to_int"
     (assert-num-args 1 1)
     (interp-char-to-int (car v2) params)]
    [_ 
      (cm-error "CONTRACT" (format "Invalid op-id to `internal_op`: ~a" v1))]))
