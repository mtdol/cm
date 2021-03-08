#lang racket
(require cm/core/ast cm/core/error cm/core/types cm/core/context
         cm/core/parse-stat cm/core/lex cm/core/interp-utils cm/core/modules)
(provide interp)
(define error-id 3)

;; Matthew Dolinka
;; cm interpreter


(define (interp-prep expr linenum)
    (cm-error-with-line-handler linenum interp-expr (list expr (hash))))

;; converts values into the output format
(define (prepare-for-output v)
  ;(cond
    ;[(is-fun? v) v]
    ;[(is-struct? v) v]
    ;[else (string-coerce v)]
    
    ;))
    v)

;; takes in an expr or a statement and returns a list of all accumulated values
(define (interp ast) 
  (match ast
        [(Stat i e st) 
         (match st
                [(EOP) (prepare-for-output (interp-prep e i))]
                [_ (flatten (list (prepare-for-output (interp-prep e i)) (interp st)))])]
        [(EOP) (prepare-for-output (interp-expr (Void) (hash)))]
        [_ (prepare-for-output (interp-expr ast (hash)))]))

(define (interp-expr ast context)
  (match ast
        [(Void) (Void)]
        [(Prim2 'apply e1 e2) (interp-apply (interp-expr e1 context) (interp-expr e2 context))]
        [(Prim2 op e1 e2) (interp-prim2 op (interp-expr e1 context) (interp-expr e2 context))]
        [(Prim1 op e) (interp-prim1 op (interp-expr e context))]
        [(If e1 e2) (interp-if e1 e2 context)]
        [(Cond e) 
         (match e
                [(Case e1 e2 e3) (interp-case e1 e2 e3 context)]
                [_ (cm-error error-id "Cond is missing a case.")])]
        [(Case e1 e2 e3) (interp-case e1 e2 e3 context)]
        [(Match e1 e2) (interp-match (interp-expr e1 context) e2 context)]
        [(Wrap e) (interp-expr e context)]
        [(Def e1 e2) (interp-def e1 e2 context)]
        [(Let e1 e2) (interp-let e1 e2 context)]
        [(Lambda e1 e2) (interp-lambda e1 e2 context)]
        [(Print e) (interp-print e context)]
        [(Error e) (error (string-coerce (interp-expr e context)))] 
        [(Eval s) (eval-string (string-coerce (interp-expr s context)))] 
        [(Typedef e1 e2) (interp-typedef e1 e2)]
        [(Struct e1 e2) (interp-struct e1 e2 context)]
        [(IsStruct e1 e2) (interp-struct? e1 e2 context)]
        [(Appl e1 e2) (interp-appl e1 e2 context)]
        [(Load e) 
         (match (interp-expr e context)
                [s #:when (string? s) (process-import-string s)]
                [_ (cm-error error-id "Argument to load must be a file.")])
             ]
        [(Var v) (interp-var v context)]
        [(Int i) i]
        [(Float f) f]
        [(Bool i) (Bool i)]
        [(Null) null]
        [(String s) s]
        [e (cm-error error-id (format "Unknown expression: ~a." e))]))

(define (and-op arg1 arg2) (and (bool-to-racket arg1) (bool-to-racket arg2)))
(define (or-op arg1 arg2) (or (bool-to-racket arg1) (bool-to-racket arg2)))
(define (xor-op arg1 arg2) (xor (bool-to-racket arg1) (bool-to-racket arg2)))
(define (not-equal? arg1 arg2) (not (equal? arg1 arg2)))
(define (not-op arg) (not (bool-to-racket arg)))

(define (interp-prim2 op v1 v2)
  (match op
        ['cons (cons v1 v2)]
        ['cat (string-append (string-coerce v1)
                           (string-coerce v2))]
        ['comma v2]
        [op #:when (not (string=? (get-type v1) (get-type v2)))
          (cm-error error-id (string-append "Operands of " (symbol->string op)
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
        ['div (if (zero? v2) (cm-error error-id "Divide by zero.")
         (apply-if-type '("float") / "div" v1 v2))]
        ['mod (apply-if-type '("int") modulo "mod" v1 v2)]
        ['exp (apply-if-type '("int" "float") expt "exp" v1 v2)]))
 
(define (interp-prim1 op v)
  (match op
        ['pos  #:when (or (string=? (get-type v) "int" ) 
                        (string=? (get-type v) "float"))
        (+ v)]
        ['pos (cm-error error-id (string-append 
            "Applied positive to non number. Given type "
            (get-type v) "."))]
        ['neg  #:when (or (string=? (get-type v) "int" ) 
                        (string=? (get-type v) "float"))
        (- v)]
        ['neg (cm-error error-id (string-append 
            "Applied negative to non number. Given type "
            (get-type v) "."))]
        ['head  #:when (or (string=? (get-type v) "list" ) 
                        (string=? (get-type v) "pair"))
        (car v)]
        ['head (cm-error error-id (string-append 
            "Applied head to non list or pair. Given type "
            (get-type v) "."))]
        ['tail  #:when (or (string=? (get-type v) "list" ) 
                        (string=? (get-type v) "pair"))
        (cdr v)]
        ['tail (cm-error error-id (string-append 
            "Applied tail to non list or pair. Given type "
            (get-type v) "."))]
        ['length  #:when (or (string=? (get-type v) "list")
                           (string=? (get-type v) "null")) 
        (length v)]
        ['length  #:when (string=? (get-type v) "string" ) 
        (string-length v)]
        ['length (cm-error error-id (string-append 
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
                 [_ (cm-error error-id "Then clause is missing an else.")])]
         [_ (cm-error error-id "If clause is missing a then.")]))

(define (interp-match v e context)
  (match e 
         [(Case ce1 ce2 ce3)
          (match ce2
               [(Yields ce2-1)
                (match ce1
                       [(When ce1-1 ce1-2)
                        (let ([context-2 (match-expr ce1-1 v (hash))])
                        (if (and context-2 (bool-to-racket (interp-expr ce1-2 context-2)))
                          (interp-expr ce2-1 context-2)
                          (interp-match v ce3 context)
                          ))
                        ]
                       [_ 
                        (let ([context-2 (match-expr ce1 v (hash))])
                          ;; hashes evaluate to #t in racket
                        (if context-2
                          (interp-expr ce2-1 context-2)
                          (interp-match v ce3 context)
                          ))

                         ]
                       )
                ]
               [_ (cm-error error-id "Missing yields for match case.")])
          ]
         [(End) (cm-error error-id (format "Matching failed for ~a." (string-coerce v)))]
         [_ (cm-error error-id "Invalid match syntax.")]))

;; ast, value, hash -> hash | #f
(define (match-expr e v context)
  (match e
         [(Int i) (if (equal? i v) context #f)]
         [(Float f) (if (equal? f v) context #f)]
         [(Bool i1) (match v [(Bool i2) (if (equal? i1 i2) context #f)] [_ #f])]
         [(Null) (if (null? v) context #f)]
         [(Void) (match v [(Void) context] [_ #f])]
         [(String s) (if (string=? s v) context #f)]
         ;; wildcard, always match
         [(Var "_") context]
         ;; implied dynamic var
         [(Var id) (match-var id "dynamic" v context)]
         [(Prim1 op (Var id)) #:when (member op guard-types)
                        (match-var id (symbol->string op) v context)]
         [(Struct (Var label) lst) #:when (expr-is-list? lst)
          (match v
            [(CmStruct label2 lst2) #:when (equal? label label2)
                    (match-expr lst lst2 context)]
            [_ #f])]
         ;[(Lambda id (Assign1 e2))]
         [(Prim2 'cons e1 e2)
          (match v
                 [(cons v1 v2)
                    (let ([c2 (match-expr e1 v1 context)])
                        (if (not (equal? c2 #f)) (match-expr e2 v2 c2) #f))]
                 [_ #f])]

         [_ (cm-error error-id "Invalid syntax for match.")]))

;; Checks that types of var and value match (if they don't returns false).
;; If types match then returns modified context with itself included
;;
;; string, string, value, hash -> hash | #f
(define (match-var id type v context)
  (if (is-type? type v)
    ;; check if we have already seen the var
    (if (hash-has-key? context id)
      (if (deep-equal? v (get-local-var-data id context))
        ;; successful match
        context
        ;; does not match earlier var, failed match
        #f
        )
      (set-local-var id v context)
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
                 [_ (cm-error error-id "Case must end in else or another case.")]
                 )]
         [_ (cm-error error-id "Case is missing yields.")]))

(define (interp-var var context)
  ;; check local context first
  (let ([res (get-local-var-data var context)])
  (if (not (false? res)) res 
  ;; then check global
  (match (get-global-var-data var)
         [#f (cm-error error-id (format "Var ~a has not yet been defined." var))]
         [res res]))))


(define (interp-def e1 e2 context)
  (match e2
         [(Assign1 e3) 
          (let ([v3 (interp-expr e3 context)])
            (match e1
                   ;; sets var in global context hash and returns value to caller
                   [(Prim1 'int (Var v))
                    (assign-type-check "int" v3 v) (set-global-var! v v3) v3]
                   [(Prim1 'float (Var v))
                    (assign-type-check "float" v3 v) (set-global-var! v v3) v3]
                   [(Prim1 'string (Var v))
                    (assign-type-check "string" v3 v) (set-global-var! v v3) v3]
                   [(Prim1 'bool (Var v))
                    (assign-type-check "bool" v3 v) (set-global-var! v v3) v3]
                   [(Prim1 'list (Var v))
                    (assign-type-check "list" v3 v) (set-global-var! v v3) v3]
                   [(Prim1 'pair (Var v))
                    (assign-type-check "pair" v3 v) (set-global-var! v v3) v3]
                   [(Prim1 'fun (Var v))
                    (assign-type-check "fun" v3 v) (set-global-var! v v3) v3]
                   [(Prim1 'dynamic (Var v))
                    (set-global-var! v v3) v3]
                   [(Struct (Var label) (Var v))
                    (assign-type-check (get-struct-type-string label) v3 v) (set-global-var! v v3) v3]
                   ;; implied dynamic case
                   [(Var v)
                    (set-global-var! v v3) v3]
                   [_ (cm-error error-id "Unknown Item on left hand of def.")]


                   ))]
         [_ (cm-error error-id "Def is missing an assignment.")]))

(define (interp-let e1 e2 context)
  (match e2
         [(Assign2 e3 (In e4)) 
          (let ([v3 (interp-expr e3 context)])
            (match e1
                   ;; sets var in global context hash and returns value to caller
                   [(Prim1 'int (Var v))
                    (assign-type-check "int" v3 v) (interp-expr e4 (set-local-var v v3 context))]
                   [(Prim1 'float (Var v))
                    (assign-type-check "float" v3 v) (interp-expr e4 (set-local-var v v3 context))]
                   [(Prim1 'string (Var v))
                    (assign-type-check "string" v3 v) (interp-expr e4 (set-local-var v v3 context))]
                   [(Prim1 'bool (Var v))
                    (assign-type-check "bool" v3 v) (interp-expr e4 (set-local-var v v3 context))]
                   [(Prim1 'list (Var v))
                    (assign-type-check "list" v3 v) (interp-expr e4 (set-local-var v v3 context))]
                   [(Prim1 'pair (Var v))
                    (assign-type-check "pair" v3 v) (interp-expr e4 (set-local-var v v3 context))]
                   [(Prim1 'fun (Var v))
                    (assign-type-check "fun" v3 v) (interp-expr e4 (set-local-var v v3 context))]
                   [(Struct (Var label) (Var v))
                    (assign-type-check (get-struct-type-string label) v3 v) (interp-expr e4 (set-local-var v v3 context))]
                   [(Prim1 'dynamic (Var v))
                    (interp-expr e4 (set-local-var v v3 context))]
                   ;; implied dynamic case
                   [(Var v)
                    (interp-expr e4 (set-local-var v v3 context))]
                   [_ (cm-error error-id "Unknown Item on left hand of let.")]


                   ))]
         [_ (cm-error error-id "Let is missing an assignment or in.")]))

(define (interp-lambda e1 e2 context)
  (match e2
         [(Assign1 e3) 
            (match e1
                   ;; sets var in global context hash and returns value to caller
                   [(Prim1 op (Var v)) #:when (member op guard-types)
                     (Fun v (symbol->string op) context e3)]
                   ;; struct guard
                   [(Struct (Var label) (Var v)) 
                     (Fun v (get-struct-type-string label) context e3)]
                   ;; implied dynamic case
                   [(Var v)
                     (Fun v "dynamic" context e3)]
                   [_ (cm-error error-id "Unknown Item on left hand of lambda")]
                   )]
         [_ (cm-error error-id "Lambda is missing an assignment.")]))

(define (interp-apply v1 v2)
  (match v2
         [(Fun var type fcontext fexpr) 
            ;; check that the application matches the functions type
            (assign-type-check type v1 var)
            ;; interp with modified context
            (interp-expr fexpr (set-local-var var v1 fcontext))]
         [_ (cm-error error-id "Attempted to apply onto a non function.")]))


(define (interp-appl e1 e2 context)
  (match (interp-expr e2 context)
    [l1 #:when (list? l1) 
      (let aux ([lst l1] [res (interp-expr e1 context)])
        (match lst
               ['() res]
               [(cons h t) (aux t (interp-apply h res))]

               ))]
    [_ (cm-error error-id "Arguments to appl must be a list.")]

          ))

(define (interp-typedef e1 e2)
  (match e2 [(Assign1 e2-2)
  (let ([lst2 (ast-cons-to-racket e2-2)])
  (match e1
         [(Var v) #:when (list? lst2)
          (let verify-schema ([lst lst2] [acc '()])
            (match lst
                   ;; set schema if no errors were found
                   ['() (set-type! v (reverse acc)) (Void)]
                   [(cons (Var _) t) (verify-schema t (cons (car lst) acc))]
                   [(cons (Prim1 grd (Var _)) t) #:when 
                                (member grd guard-types) 
                        (verify-schema t (cons (car lst) acc))]
                   ;; struct inside a struct
                   [(cons (Struct label (Var _)) t) 
                        (verify-schema t (cons (car lst) acc))]
                   [(cons h t) (cm-error error-id 
                        (format "Unknown element ~a inside typedef schema." h))]))]
         [(Var v) (cm-error error-id "Invalid schema for typedef. Schema must be a list.")]
         [_ (cm-error error-id "Missing Label for typedef.")]))
    ]
    [_ (cm-error error-id "Improperly formed typedef.")]))

(define (interp-struct e1 e2 context)
  (match e1
         [(Var v)
          (match (interp-expr e2 context)
                 ;; struct args must be a list (null for no args, still technically a list)
                 [res #:when (list? res) 
                      (match (get-type-data v)
                             [#f (cm-error error-id (format "Type ~a has not been declared." v))]
                             [schema
                            (match (valid-against-schema? v schema res)
                                   [#t (CmStruct v res)]
                                   [#f (cm-error error-id 
                (format (string-append "Could not validate struct against type schema:"
                                       "\nstruct:\n~a ~a\nschema:\n~a")
                        v res (get-type-data v)))])])]
                 [_ (cm-error error-id "Arguments to struct must be a list or null.")])]
         [_ (cm-error error-id "Missing label for struct.")]))

(define (interp-struct? e1 e2 context)
  (match e1
         [(Var label1) (racket-to-bool (is-struct-type? label1 (interp-expr e2 context)))]
         [_ (cm-error error-id "Missing label for struct question.")]))

(define (interp-print e context)
  (let ([v (interp-expr e context)])
     (match (cons (displayln (string-append (string-coerce v))) v)
            [(cons _ res) res])))
