#lang racket
(require racket/hash
         cm/core/ast cm/core/error cm/core/types cm/core/context
         cm/core/parse-stat cm/core/lex cm/core/interp-utils cm/core/modules)
(provide interp)

;; Matthew Dolinka
;; cm interpreter


(define (interp-prep expr linenum)
    (cm-error-with-line-handler linenum interp-expr (list expr (hash))))

;; converts values into the output format
(define (prepare-for-output v)
    v)

;; takes in an expr or a statement and returns a list of all accumulated values
(define (interp ast) 
  (match ast
        [(Stat i e st) 
         (match st
                [(EOP) (prepare-for-output (interp-prep e i))]
                [_ (flatten (list (prepare-for-output (interp-prep e i)) (interp st)))])]
        [(EOP) (prepare-for-output (interp-expr (Prim0 'void) (hash)))]
        [_ (prepare-for-output (interp-expr ast (hash)))]))

(define (interp-expr ast context)
  (match ast
        [(Prefix2 'struct e1 e2) (interp-struct e1 e2 context)]
        [(Prefix2 'struct? e1 e2) (interp-struct? e1 e2 context)]
        [(Prefix2 'appl e1 e2) (interp-appl e1 e2 context)]
        [(Prefix2 'index e1 e2) (interp-index e1 e2 context)]
        ;; general prim cases
        [(Prim2 op e1 e2) (interp-prim2 op (interp-expr e1 context) (interp-expr e2 context))]
        [(Prim1 op e) (interp-prim1 op (interp-expr e context))]
        [(Prim0 op) (interp-prim0 op)]
        [(If e1 e2) (interp-if e1 e2 context)]
        [(Cond e) 
         (match e
                [(Case e1 e2 e3) (interp-case e1 e2 e3 context)]
                [_ (cm-error "SYNTAX" "Cond is missing a case.")])]
        [(Case e1 e2 e3) (interp-case e1 e2 e3 context)]
        [(While e1 (Do e2)) (interp-while e1 e2 context)]
        [(While _ _) (cm-error "SYNTAX" "While is missing a do.")]
        [(Try e1 e2) (interp-try-catch e1 e2 context)]
        [(Match e1 e2) (interp-match (interp-expr e1 context) e2 context context)]
        [(Def e1 (Assign e2)) 
          (match (interp-def-list e1 e2)
                 [(Def e1-2 e2-2) (interp-def e1-2 e2-2 context)])]
        [(Def _ _) (cm-error "SYNTAX" "Def is missing an assignment.")]
        [(Defun (Var id) e1 (Assign e2))
          (interp-expr 
            (Def (Var id) (Assign (Lambda e1 (Assign e2))))
            context)]
        [(Defun _ _ _) (cm-error "SYNTAX" "Improperly formed defun.")]
        [(Let e1 e2 e3) (interp-let e1 e2 e3 context)]
        [(Lambda e1 (Assign e2)) 
          (match (interp-lambda-list e1 e2)
                 [(Lambda e1-2 e2-2) (interp-lambda e1-2 e2-2 context)])]
        [(Lambda _ _) (cm-error "SYNTAX" "Lambda is missing an assignment.")]
        [(Typedef e1 e2) (interp-typedef e1 e2 context)]
        [(Var v) (interp-var v context)]
        [(Int i) i]
        [(Float f) f]
        [(Bool i) (Bool i)]
        [(Null) null]
        [(String s) s]
        [e (cm-error "SYNTAX" (format "Unknown expression: ~a." e))]))

(define (and-op arg1 arg2) (and (bool-to-racket arg1) (bool-to-racket arg2)))
(define (or-op arg1 arg2) (or (bool-to-racket arg1) (bool-to-racket arg2)))
(define (xor-op arg1 arg2) (xor (bool-to-racket arg1) (bool-to-racket arg2)))
(define (not-equal? arg1 arg2) (not (equal? arg1 arg2)))
(define (not-eqq? arg1 arg2) (not (deep-equal? arg1 arg2)))
(define (not-op arg) (not (bool-to-racket arg)))

(define (interp-prim2 op v1 v2)
  (match op
        ['cons (cons v1 v2)]
        ['apply (interp-apply v1 v2)]
        ['cat (string-append (string-coerce v1)
                           (string-coerce v2))]
        ['comma v2]
        ['eqq (racket-to-bool (deep-equal? v1 v2))]
        ['neqq (racket-to-bool (not-eqq? v1 v2))]
        ;; everything below is checked for same operands
        [op #:when (not (string=? (get-type v1) (get-type v2)))
          (cm-error "CONTRACT" (string-append "Operands of " (symbol->string op)
                " are not of the same type. Given " (get-type v1)
                ", " (get-type v2) "."))]
        ['and (racket-to-bool 
               (apply-if-type '("bool") and-op "and" v1 v2))]
        ['or (racket-to-bool 
               (apply-if-type '("bool") or-op "or" v1 v2))]
        ['xor (racket-to-bool 
               (apply-if-type '("bool") xor-op "xor" v1 v2))]
        ['gt (racket-to-bool 
               (apply-if-type '("int" "float") > "gt" v1 v2))]
        ['lt (racket-to-bool 
               (apply-if-type '("int" "float") < "lt" v1 v2))]
        ['ge (racket-to-bool 
               (apply-if-type '("int" "float") >= "ge" v1 v2))]
        ['le (racket-to-bool 
               (apply-if-type '("int" "float") <= "le" v1 v2))]
        ['eq (racket-to-bool 
               (apply-if-type '("int" "float" "string" "list" "pair" "bool" "null")
                              equal? "equal" v1 v2))]
        ['neq (racket-to-bool 
               (apply-if-type '("int" "float" "string" "list" "pair" "bool" "null")
                              not-equal? "not_equal" v1 v2))]
        ['add (apply-if-type '("int" "float") + "plus" v1 v2)]
        ['sub (apply-if-type '("int" "float") - "minus" v1 v2)]
        ['mult (apply-if-type '("int" "float") * "mult" v1 v2)]
        ['div (if (zero? v2) (cm-error "CONTRACT" "Divide by zero.")
         (apply-if-type '("float") / "div" v1 v2))]
        ['mod (apply-if-type '("int") modulo "mod" v1 v2)]
        ['exp (apply-if-type '("int" "float") expt "exp" v1 v2)]))
 
(define (interp-prim1 op v)
  (match op
        ['print (interp-print v)]
        ['error 
         (match v
                ;; TODO use cm error struct and use id
                [(list id msg) (cm-error-no-linenum id msg)]
                [msg #:when (string? msg) (cm-error-no-linenum "GENERIC" msg)]
                [_ (cm-error "CONTRACT" "Invalid argument(s) to error.")])]
        ['eval (eval-string (string-coerce v))] 
        ['load 
         (match v 
                [s #:when (string? s) (process-import-string s)]
                [_ (cm-error "CONTRACT" "Argument to load must be a file.")])
             ]
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
        (car v)]
        ['head (cm-error "CONTRACT" (string-append 
            "Applied head to non list or pair. Given type "
            (get-type v) "."))]
        ['tail  #:when (or (string=? (get-type v) "list" ) 
                        (string=? (get-type v) "pair"))
        (cdr v)]
        ['tail (cm-error "CONTRACT" (string-append 
            "Applied tail to non list or pair. Given type "
            (get-type v) "."))]
        ['length  #:when (or (string=? (get-type v) "list")
                           (string=? (get-type v) "null")) 
        (length v)]
        ['length  #:when (string=? (get-type v) "string" ) 
        (string-length v)]
        ['length (cm-error "CONTRACT" (string-append 
            "Applied length to non list or string. Given type "
            (get-type v) "."))]
        ['type (get-type v)] 
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
        ['fun? (racket-to-bool (is-fun? v))] 
        ['not (racket-to-bool
            (apply-if-type-1 '("bool") not-op "not" v))]

        ))

(define (interp-prim0 op)
  (match op
         ['void (Prim0 'void)]
         ))

;;
;; non-standard interp cases
;;

(define (interp-if e1 e2 context)
  (match e2
         [(Then e3 e4) 
          (match e4
                 [(Else e5)
                     (if (bool-to-racket (interp-expr e1 context))
                       (interp-expr e3 context) (interp-expr e5 context))]
                 [_ (cm-error "SYNTAX" "Then clause is missing an else.")])]
         [_ (cm-error "SYNTAX" "If clause is missing a then.")]))

(define (interp-while e1 e2 context) 
  (cond
    [(bool-to-racket (interp-expr e1 context)) 
     (interp-expr e2 context)
     (interp-while e1 e2 context)]
    [else (Prim0 'void)]))

(define (interp-try-catch e1 e2 context)
  (match e2
         [(Catch (Var id) (With e3)) 
            (with-handlers* ([exn:fail? (lambda err 
                (match err [(list (exn:fail err-msg _))
                    (let ([err-struct 
                        (CmStruct "Error" (list (get-id-from-message err-msg) err-msg))])
               (interp-expr e3 (set-local-var id err-struct context) ))
                                        ])
                                                    )])
                            (interp-expr e1 context))

          ]
         [_ (cm-error "SYNTAX" "Improperly formed try-catch.")]

         ))

;; match context is the internal context used inside the match, context is just the usual
;; local context
;;
;; value, expr hash, hash -> value
(define (interp-match v e match-context context)
  (match e 
         [(Case ce1 ce2 ce3)
          (match ce2
               [(Yields ce2-1)
                (match ce1
                       [(Prim2 'when ce1-1 ce1-2)
                        (let ([match-context-2 (match-expr ce1-1 v (hash) context)])
                          (let ([merged-context (if match-context-2 
                                                  (hash-union match-context match-context-2
                                                    #:combine/key (lambda (k v1 v2) v2))
                                                  #f)])
                        (if (and merged-context (bool-to-racket (interp-expr ce1-2 merged-context)))
                              (interp-expr ce2-1 merged-context)
                          (interp-match v ce3 match-context context)
                          )))
                        ]
                       [_ 
                        (let ([match-context-2 (match-expr ce1 v (hash) context)])
                          ;; hashes evaluate to #t in racket
                        (if match-context-2
                          ;; merge the local contexts so that match-context 2 vals replace
                          ;; match context vals
                          (let ([merged-context (hash-union match-context match-context-2
                                                    #:combine/key (lambda (k v1 v2) v2))])
                              (interp-expr ce2-1 merged-context))
                          (interp-match v ce3 match-context context)
                          ))

                         ]
                       )
                ]
               [_ (cm-error "SYNTAX" "Missing yields for match case.")])
          ]
         [(Prim0 'end) (cm-error "GENERIC" (format "Matching failed for ~a." (string-coerce v)))]
         [_ (cm-error "SYNTAX" "Invalid match syntax.")]))

;; ast, value, hash -> hash | #f
(define (match-expr e v match-context context)
  (match e
         [(Int i) (if (equal? i v) match-context #f)]
         [(Float f) (if (equal? f v) match-context #f)]
         [(Bool i1) (match v [(Bool i2) (if (equal? i1 i2) match-context #f)] [_ #f])]
         [(Null) (if (null? v) match-context #f)]
         [(Prim0 'void) (match v [(Prim0 'void) match-context] [_ #f])]
         [(String s) (if (string=? s v) match-context #f)]
         ;; wildcard, always match
         [(Var "_") match-context]
         [(Prim1 op (Var "_")) (cm-error "SYNTAX" "Cannot type-guard wildcard.")]
         ;; implied dynamic var
         [(Var id) (match-var id (list "dynamic") v match-context)]
         [(Prim1 op (Var id)) #:when (member op guard-types)
                        (match-var id (list (symbol->string op)) v match-context)]
         [(Prefix2 'types types-expr (Var id)) 
          (match-var id (check-types-list (interp-expr types-expr context))
                     v match-context)]
         [(Prefix2 'struct (Var label) lst) #:when (expr-is-list? lst)
          (match v
            [(CmStruct label2 lst2) #:when (equal? label label2)
                    (match-expr lst lst2 match-context context)]
            [_ #f])]
         ;[(Lambda id (Assign e2))]
         [(Prim2 'cons e1 e2)
          (match v
                 [(cons v1 v2)
                    (let ([mc2 (match-expr e1 v1 match-context context)])
                        (if (not (equal? mc2 #f)) (match-expr e2 v2 mc2 context) #f))]
                 [_ #f])]

         [_ (cm-error "SYNTAX" "Invalid syntax for match.")]))

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

(define (interp-case e1 e2 e3 context)
  (match e2
         [(Yields e2-2)
          (match e3
                 [(Case e3-1 e3-2 e3-3) 
                  (if (bool-to-racket (interp-expr e1 context)) (interp-expr e2-2 context)
                    (interp-case e3-1 e3-2 e3-3 context))]
                 [(Else e3-3)
                  (if (bool-to-racket (interp-expr e1 context)) (interp-expr e2-2 context)
                    (interp-expr e3-3 context))]
                 [_ (cm-error "SYNTAX" "Case must end in else or another case.")]
                 )]
         [_ (cm-error "SYNTAX" "Case is missing yields.")]))

(define (interp-var var context)
  ;; check local context first
  (let ([res (get-local-var-data var context)])
  (if (not (false? res)) res 
  ;; then check global
  (match (get-global-var-data var)
         [#f (cm-error "UNDEFINED" (format "Var ~a has not yet been defined." var))]
         [res res]))))


(define (interp-def e1 e2 context)
  (match e2
         [(Assign e3) 
          (let ([v3 (interp-expr e3 context)])
            (match e1
                   ;; sets var in global context hash and returns value to caller
                   [(Prim1 op (Var v)) #:when (member op guard-types)
                    (assign-type-check (list (symbol->string op)) v3 v) (set-global-var! v v3) v3]
                   [(Prefix2 'types types-expr (Var v))
                    (assign-type-check 
                      (check-types-list (interp-expr types-expr context)) v3 v)
                    (set-global-var! v v3) v3]
                   [(Prim1 'dynamic (Var v))
                    (set-global-var! v v3) v3]
                   [(Prefix2 'struct (Var label) (Var v))
                    (assign-type-check (list (get-struct-type-string label)) v3 v)
                    (set-global-var! v v3) v3]
                   ;; implied dynamic case
                   [(Var v)
                    (set-global-var! v v3) v3]
                   [_ (cm-error "SYNTAX" "Unknown Item on left hand of def.")]


                   ))]
         [_ (cm-error "SYNTAX" "Def is missing an assignment.")]))

(define (interp-let e1 e2 e3 context)
  (match (cons e2 e3)
         [(cons (Assign e2-2) (In e3-2)) 
          (let ([v2 (interp-expr e2-2 context)])
            (match e1
                   ;; sets var in global context hash and returns value to caller
                   [(Prim1 op (Var v)) #:when (member op guard-types)
                    (assign-type-check (list (symbol->string op)) v2 v)
                    (interp-expr e3-2 (set-local-var v v2 context))]
                   [(Prefix2 'types types-expr (Var v))
                    (assign-type-check 
                      (check-types-list (interp-expr types-expr context)) v2 v)
                                (interp-expr e3-2 (set-local-var v v2 context))]
                   [(Prefix2 'struct (Var label) (Var v))
                    (assign-type-check (list (get-struct-type-string label)) v2 v)
                    (interp-expr e3-2 (set-local-var v v2 context))]
                   [(Prim1 'dynamic (Var v))
                    (interp-expr e3-2 (set-local-var v v2 context))]
                   ;; implied dynamic case
                   [(Var v)
                    (interp-expr e3-2 (set-local-var v v2 context))]
                   [_ (cm-error "SYNTAX" "Unknown Item on left hand of let.")]


                   ))]
         [_ (cm-error "SYNTAX" "Let is missing an assignment or in.")]))

(define (interp-lambda e1 e2 context)
  (match e2
         [(Assign e3) 
            (match e1
                   ;; sets var in global context hash and returns value to caller
                   [(Prim1 op (Var v)) #:when (member op guard-types)
                     (Fun v (list (symbol->string op)) context e3)]
                   [(Prefix2 'types types-expr (Var v))
                    (Fun v (check-types-list (interp-expr types-expr context))
                         context e3)]
                   ;; struct guard
                   [(Prefix2 'struct (Var label) (Var v)) 
                     (Fun v (list (get-struct-type-string label)) context e3)]
                   ;; implied dynamic case
                   [(Var v)
                     (Fun v (list "dynamic") context e3)]
                   [_ (cm-error "SYNTAX" "Unknown Item on left hand of lambda")]
                   )]
         [_ (cm-error "SYNTAX" "Lambda is missing an assignment.")]))

(define (interp-apply v1 v2)
  (match v2
         [(Fun var type fcontext fexpr) 
            ;; check that the application matches the functions type
            (assign-type-check type v1 var)
            ;; interp with modified context
            (interp-expr fexpr (set-local-var var v1 fcontext))]
         [_ (cm-error "CONTRACT" "Attempted to apply onto a non function.")]))


(define (interp-appl e1 e2 context)
  (match (interp-expr e2 context)
    [l1 #:when (list? l1) 
      (let aux ([lst l1] [res (interp-expr e1 context)])
        (match lst
               ['() res]
               [(cons h t) (aux t (interp-apply h res))]

               ))]
    [_ (cm-error "CONTRACT" "Arguments to appl must be a list.")]

          ))

(define (interp-typedef e1 e2 context)
  (match e2 [(Assign e2-2)
  (let ([lst2 (ast-cons-to-racket e2-2)])
  (match e1
         [(Var v) #:when (list? lst2)
          (let verify-schema ([lst lst2] [acc '()])
            (match lst
                   ;; set schema if no errors were found
                   ['() (set-type! v (reverse acc)) (Prim0 'void)]
                   [(cons (Var _) t) (verify-schema t (cons (car lst) acc))]
                   [(cons (Prim1 grd (Var _)) t) #:when 
                                (member grd guard-types) 
                        (verify-schema t (cons (car lst) acc))]
                   [(cons (Prefix2 'types types-expr (Var id)) t)
                    (check-types-list (interp-expr types-expr context))
                    ;; we place a modified types node in the resulting schema
                    ;; (the types list is aleady interp'd and checked)
                    (verify-schema t 
                        (cons 
                          (Prefix2 'types 
                                   (check-types-list (interp-expr types-expr context))
                                   (Var id))
                          acc))]
                   ;; struct inside a struct
                   [(cons (Prefix2 'struct label (Var _)) t) 
                        (verify-schema t (cons (car lst) acc))]
                   [(cons h t) (cm-error "SYNTAX" 
                        (format "Unknown element ~a inside typedef schema." h))]))]
         [(Var v) (cm-error "SYNTAX" "Invalid schema for typedef. Schema must be a list.")]
         [_ (cm-error "SYNTAX" "Missing Label for typedef.")]))
    ]
    [_ (cm-error "SYNTAX" "Improperly formed typedef.")]))

(define (interp-struct e1 e2 context)
  (match e1
         [(Var v)
          (match (interp-expr e2 context)
                 ;; struct args must be a list (null for no args, still technically a list)
                 [res #:when (list? res) 
                      (match (get-type-data v)
                             [#f (cm-error "GENERIC" (format "Type ~a has not been declared." v))]
                             [schema
                            (match (valid-against-schema? v schema res)
                                   [#t (CmStruct v res)]
                                   [#f (cm-error "GENERIC" 
                (format (string-append "Could not validate struct against type schema:"
                                       "\nstruct:\n~a ~a\nschema:\n~a")
                        v res (get-type-data v)))])])]
                 [_ (cm-error "SYNTAX" "Arguments to struct must be a list or null.")])]
         [_ (cm-error "SYNTAX" "Missing label for struct.")]))

(define (interp-struct? e1 e2 context)
  (match e1
         [(Var label1) (racket-to-bool (is-struct-type? label1 (interp-expr e2 context)))]
         [_ (cm-error "SYNTAX" "Missing label for struct question.")]))

(define (interp-index e1 e2 context)
  (match (interp-expr e1 context)
         [vs #:when (list? vs) 
             (match (interp-expr e2 context)
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
             (match (interp-expr e2 context)
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
         [s (cm-error "CONTRACT" (format "Invalid operand for index: ~a" (string-coerce s)))]
         ))

(define (interp-print v)
 (begin (displayln (string-append (string-coerce v))) 
        v))
