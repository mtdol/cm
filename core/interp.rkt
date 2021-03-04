#lang racket
(require cm/core/ast cm/core/error cm/core/types cm/core/context)
(provide interp)
(define error-id 3)

;; Matthew Dolinka
;; cm interpreter


(define (interp-prep expr linenum)
    (cm-error-with-line-handler linenum interp-expr (list expr (hash))))

;; converts values into the output format
(define (prepare-for-output v)
  (cond
    [(is-fun? v) v]
    [(is-struct? v) v]
    [else (string-coerce v)]))

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
        [(Prim2 'apply e1 e2) (interp-apply e1 e2 context)]
        [(Prim2 op e1 e2) (interp-prim2 op (interp-expr e1 context) (interp-expr e2 context))]
        [(Prim1 op e) (interp-prim1 op (interp-expr e context))]
        [(If e1 e2) (interp-if e1 e2 context)]
        [(Cond e) 
         (match e
                [(Case _ _ _) (interp-expr e context)]
                [_ (cm-error error-id "Cond is missing a case.")])]
        [(Case e1 e2 e3) 
         (if (bool-to-racket (interp-expr e1 context)) (interp-expr e2 context) 
           (match e3 
                  [(Case _ _ _) (interp-expr e3 context)]
                  [(Else _) (interp-expr e3 context)]
                  [(End) (cm-error error-id "Cond matching failed. Hit end.")]
                  [_ (cm-error error-id "Cond is missing its components.")]))]
        [(Yields e) (interp-expr e context)]
        [(Else e) (interp-expr e context)]
        [(Def e1 e2) (interp-def e1 e2 context)]
        [(Let e1 e2) (interp-let e1 e2 context)]
        [(Lambda e1 e2) (interp-lambda e1 e2 context)]
        [(Print e) (interp-print e context)]
        [(Error e) (error (string-coerce (interp-expr e context)))] 
        [(Eval s) (eval-string (string-coerce (interp-expr s context)))] 
        [(Struct e1 e2) (interp-struct e1 e2 context)]
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
        ['div (apply-if-type '("float") / "div" v1 v2)]
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
;; sub cases
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
                    (assign-type-check 'int v3 v) (set-global-var! v 'int v3) v3]
                   [(Prim1 'float (Var v))
                    (assign-type-check 'float v3 v) (set-global-var! v 'float v3) v3]
                   [(Prim1 'string (Var v))
                    (assign-type-check 'string v3 v) (set-global-var! v 'string v3) v3]
                   [(Prim1 'bool (Var v))
                    (assign-type-check 'bool v3 v) (set-global-var! v 'bool v3) v3]
                   [(Prim1 'list (Var v))
                    (assign-type-check 'list v3 v) (set-global-var! v 'list v3) v3]
                   [(Prim1 'pair (Var v))
                    (assign-type-check 'pair v3 v) (set-global-var! v 'pair v3) v3]
                   [(Prim1 'fun (Var v))
                    (assign-type-check 'fun v3 v) (set-global-var! v 'fun v3) v3]
                   [(Prim1 'dynamic (Var v))
                    (set-global-var! v 'dynamic v3) v3]
                   ;; implied dynamic case
                   [(Var v)
                    (set-global-var! v 'dynamic v3) v3]
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
                    (assign-type-check 'int v3 v) (interp-expr e4 (set-local-var v 'int v3 context))]
                   [(Prim1 'float (Var v))
                    (assign-type-check 'float v3 v) (interp-expr e4 (set-local-var v 'float v3 context))]
                   [(Prim1 'string (Var v))
                    (assign-type-check 'string v3 v) (interp-expr e4 (set-local-var v 'string v3 context))]
                   [(Prim1 'bool (Var v))
                    (assign-type-check 'bool v3 v) (interp-expr e4 (set-local-var v 'bool v3 context))]
                   [(Prim1 'list (Var v))
                    (assign-type-check 'list v3 v) (interp-expr e4 (set-local-var v 'list v3 context))]
                   [(Prim1 'pair (Var v))
                    (assign-type-check 'pair v3 v) (interp-expr e4 (set-local-var v 'pair v3 context))]
                   [(Prim1 'fun (Var v))
                    (assign-type-check 'fun v3 v) (interp-expr e4 (set-local-var v 'fun v3 context))]
                   [(Prim1 'dynamic (Var v))
                    (assign-type-check 'dynamic v3 v) (interp-expr e4 (set-local-var v 'dynamic v3 context))]
                   ;; implied dynamic case
                   [(Var v)
                    (interp-expr e4 (set-local-var v 'dynamic v3 context))]
                   [_ (cm-error error-id "Unknown Item on left hand of let.")]


                   ))]
         [_ (cm-error error-id "Let is missing an assignment or in.")]))

(define (interp-lambda e1 e2 context)
  (match e2
         [(Assign1 e3) 
            (match e1
                   ;; sets var in global context hash and returns value to caller
                   [(Prim1 op (Var v)) #:when (member op 
                                '(int float string bool list pair fun dynamic))
                     (Fun v op context e3)]
                   ;; implied dynamic case
                   [(Var v)
                     (Fun v 'dynamic context e3)]
                   [_ (cm-error error-id "Unknown Item on left hand of lambda")]
                   )]
         [_ (cm-error error-id "Lambda is missing an assignment.")]))

(define (interp-apply e1 e2 context)
  (match (interp-expr e2 context)
         [(Fun var type fcontext fexpr) 
          (let ([v1 (interp-expr e1 context)])
            ;; check that the application matches the functions type
            (assign-type-check type v1 var)
            ;; interp with modified context
            (interp-expr fexpr (set-local-var var type v1 fcontext)))]
         [_ (cm-error error-id "Attempted to apply onto a non function.")]))

(define (interp-struct e1 e2 context)
  (match e1
         [(Var v)
          (match (interp-expr e2 context)
                 ;; struct args must be a list (null for no args, still technically a list)
                 [res #:when (list? res) (CmStruct v res)]
                 [_ (cm-error error-id "Arguments to struct must be a list or null.")])]
         [_ (cm-error error-id "Missing Label for struct.")]))

(define (interp-print e context)
  (let ([v (interp-expr e context)])
     (match (cons (displayln (string-append (string-coerce v))) v)
            [(cons _ res) res])))


;;
;; Auxiliaries
;;

(define (eval-string s) (eval (read (open-input-string s))))

;; applies the op to the given arg list if the type of arg1 matches type
(define (apply-if-type types op op-name arg1 arg2)
  (if (member (get-type arg1) types)
    (op arg1 arg2)
    (cm-error error-id 
        (format "Attempted to apply ~a onto ~a." op-name (get-type arg1)))))
(define (apply-if-type-1 types op op-name arg)
  (if (member (get-type arg) types)
    (op arg)
    (cm-error error-id 
        (format "Attempted to apply ~a onto ~a." op-name (get-type arg)))))

;; throws an exception if the given type and the type of the value do not match
;; returns true if types match
(define (assign-type-check type value var)
  (if (equal? type 'dynamic) #t 
  (let ([v-type (get-type value)]) 
    (if (or (string=? (symbol->string type) v-type)
            ;; exceptions
            ;; a null is a list
            (and (equal? type 'list) (string=? v-type "null"))
            ;; a non-null list is a pair
            (and (equal? type 'pair) (string=? v-type "list")))
      #t
      (cm-error error-id 
        (format "Recieved type ~a for var ~a but expected ~a." v-type var type))))))

;; converts a pair to a list if it wasn't already
(define (pair-to-list p)
  (match p
        ['() '()] ;; already a list
        [(cons h t) (cons h (pair-to-list t))]
        [h (cons h '())]))

;; Ensures that the subarguments given are all lists.
(define (check-list-arguments args op)
    (if (not (list? args))
        (cm-error error-id (string-append "Arguments to " op " were not a list. "
                              "Perhaps you forgot a semicolon."))
    (let aux ([lst args])
        (match lst
            ['() '()]
            [(cons h t) #:when (not (list? h)) 
                (cm-error error-id (string-append "All subarguments to " op 
                        " must be lists. "
                       "Perhaps you forgot a semicolon."))]
            [(cons h t) (cons h (aux t))]))))

(define (check-argument-dimensions args minargs maxargs op)
    (let aux ([lst args])
        (match lst
            ['() '()]
            [(cons h t) #:when 
              (or (> (length h) maxargs) (< (length h) minargs))
                (cm-error error-id
                  (string-append "Incorrect number of subarguments to " op ". "
                       "Min number of args: " (number->string minargs) ". "
                       "Max number of args: " (number->string maxargs) ". "))]
            [(cons h t) (cons h (aux t))])))
