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
  (trace-interp-expr expr (hash) module-id (DebugData linenum)))

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
                 (trace-interp-expr (Prim0 'void) (hash) module-id (DebugData -1)))]
        ;; expr only case, no list, just a value returned
        [_ (prepare-for-output (trace-interp-expr ast (hash) module-id (DebugData -1)))]))

;; interps expr while updating the stack trace
(define (trace-interp-expr e context module-id debug)
  ;(push-elem-to-trace-stack! (ast-node-to-trace-elem e module-id debug))
  (interp-expr e context module-id debug))

(define (interp-expr ast context module-id debug)
  (match ast
    [(Prim2 'when _ _) (cm-error "SYNTAX" "Cannot use `when` outside of match-expr.")]
    [(Prefix2 '? _ _) (cm-error "SYNTAX" "Cannot use `?` outside of match-expr.")]
    [(Prim0 '...) (cm-error "SYNTAX" "Cannot use `...` outside of match-expr.")]
    [(? var-container? var) 
     (interp-var (get-var-label var context module-id debug) context module-id)]
    [(Prim1 'schemaof e1) (interp-schemaof e1 context module-id debug)]
    [(Prefix2 'struct e1 e2) (interp-struct e1 e2 context module-id debug)]
    [(Prefix2 'struct? e1 e2) (interp-struct? e1 e2 context module-id debug)]
    [(Prefix2 'appl e1 e2) (interp-appl e1 e2 context module-id debug)]
    [(Prefix2 'index e1 e2) (interp-index e1 e2 context module-id debug)]
    [(Prim2 'appindex e1 e2) (interp-index e1 e2 context module-id debug)]
    [(Prefix2 'writestrf e1 e2) 
     (interp-writestrf 
       (trace-interp-expr e1 context module-id debug)
       (trace-interp-expr e2 context module-id debug))]
    [(Prefix2 'appendstrf e1 e2) 
     (interp-appendstrf 
       (trace-interp-expr e1 context module-id debug)
       (trace-interp-expr e2 context module-id debug))]
    [(Prefix2 'cp e1 e2) 
     (interp-cp 
       (trace-interp-expr e1 context module-id debug)
       (trace-interp-expr e2 context module-id debug))]
    [(Prefix2 'mv e1 e2) 
     (trace-interp-expr (Prefix2 'cp e1 e2) context module-id debug)
     (trace-interp-expr (Prim1 'rm e1) context module-id debug)]
    [(Prefix2 'hash_ref e1 e2) 
     (interp-hash-ref 
       (trace-interp-expr e1 context module-id debug)
       (trace-interp-expr e2 context module-id debug))]
    [(Prefix2 'hash_has_key? e1 e2) 
     (interp-hash-has-key? 
       (trace-interp-expr e1 context module-id debug)
       (trace-interp-expr e2 context module-id debug))]
    [(Prefix2 'peek_string e1 e2) 
     (interp-peek-string 
       (trace-interp-expr e1 context module-id debug)
       (trace-interp-expr e2 context module-id debug))]
    [(Prefix3 'hash_set e1 e2 e3) 
     (interp-hash-set 
       (trace-interp-expr e1 context module-id debug)
       (trace-interp-expr e2 context module-id debug)
       (trace-interp-expr e3 context module-id debug)
       )]
    [(Prefix3 'hash_ref_check e1 e2 e3) 
     (interp-hash-ref-check 
       (trace-interp-expr e1 context module-id debug)
       (trace-interp-expr e2 context module-id debug)
       (trace-interp-expr e3 context module-id debug)
       )]
    ;; short circuiting and/or
    [(Prim2 'and e1 e2) 
     (let ([v1 (trace-interp-expr e1 context module-id debug)])
       (assert-contract (list "bool") v1 "and")
       (if 
         (bool-to-racket v1)
         (let ([v2 (trace-interp-expr e2 context module-id debug)])
           (assert-contract (list "bool") v2 "and")
           v2)
         (racket-to-bool #f)))]
    [(Prim2 'or e1 e2) 
     (let ([v1 (trace-interp-expr e1 context module-id debug)])
       (assert-contract (list "bool") v1 "or")
       (if 
         (bool-to-racket v1)
         (racket-to-bool #t)
         (let ([v2 (trace-interp-expr e2 context module-id debug)])
           (assert-contract (list "bool") v2 "or")
           v2)))]
    ;; general prim cases
    [(Prim2 op e1 e2) (interp-prim2 
                        op 
                        (trace-interp-expr e1 context module-id debug) 
                        (trace-interp-expr e2 context module-id debug)
                        module-id debug)]
    [(Prim1 op e) (interp-prim1 
                    op (trace-interp-expr e context module-id debug) 
                    context module-id debug)]
    [(Prim0 op) (interp-prim0 op)]
    [(If e1 (Then e2) (Else e3)) (interp-if e1 e2 e3 context module-id debug)]
    [(If _ _ _) (cm-error "SYNTAX" "Improperly formed `if`.")]
    [(Cond e) 
     (match e
            [(Case e1 e2 e3) (interp-case e1 e2 e3 context module-id debug)]
            [_ (cm-error "SYNTAX" "`cond` is missing a case.")])]
    [(Case e1 e2 e3) (interp-case e1 e2 e3 context module-id debug)]
    [(While e1 (Do e2)) (interp-while e1 e2 context module-id debug)]
    [(While _ _) (cm-error "SYNTAX" "`while` is missing a do.")]
    [(Foreach e1 (In e2) (Do e3))
     (let ([vs (trace-interp-expr e2 context module-id debug)])
       (if (is-list? vs)
           (interp-foreach e1 vs e3 context module-id debug)
           (cm-error "CONTRACT" "Argument to `foreach` guard must be a list.")))]
    [(Foreach _ _ _) (cm-error "SYNTAX" "`foreach` is improperly formed.")]
    [(Try e1 (Catch e2) (With e3))
     (interp-try-catch e1 e2 e3 context module-id debug)]
    [(Try _ _ _) (cm-error "SYNTAX" "Improperly formed `try`.")]
    [(Match e1 e2) (interp-match 
                     (trace-interp-expr e1 context module-id debug)
                     e2 context context module-id debug)]
    [(Def e1 (Assign e2)) 
      (match (interp-def-list e1 e2)
             [(Def e1-2 (Assign e2-2)) 
              (let ([v2 (trace-interp-expr e2-2 context module-id debug)])
              (let-values ([(guard-types label)
                            (interp-left-hand-expr e1-2 context module-id debug)])
                (interp-def guard-types label v2 context module-id #f debug)))])]
    [(Def _ _) (cm-error "SYNTAX" "Improperly formed `def`.")]
    [(Set e1 (Assign e2)) 
     (let-values ([(guard-types label) 
                   (interp-left-hand-expr e1 context module-id debug)])
     (interp-def 
       guard-types label
       (trace-interp-expr e2 context module-id debug)
       context module-id #t debug))]
    [(Set _ _) (cm-error "SYNTAX" "Improperly formed `set`.")]
    [(Defun (? var-container? var) e1 (Assign e2))
     (let ([name (get-var-label var context module-id debug)])
     (match (interp-lambda-list e1 e2)
       [(Lambda e1-2 (Assign e2-2))
        (interp-def '("dynamic") name
          (interp-lambda e1-2 e2-2 name context module-id debug)
          context module-id #f debug)
        ])
      )]
    [(Defun _ _ _) (cm-error "SYNTAX" "Improperly formed `defun`.")]
    [(Let e1 (Assign e2) (In e3)) (interp-let e1 e2 e3 context module-id debug)]
    [(Let _ _ _) (cm-error "SYNTAX" "Improperly formed `let`.")]
    [(Lambda e1 (Assign e2)) 
      (match (interp-lambda-list e1 e2)
             [(Lambda e1-2 (Assign e2-2)) 
              (interp-lambda e1-2 e2-2 '() context module-id debug)])]
    [(Lambda _ _) (cm-error "SYNTAX" "`lambda` is missing an assignment.")]
    [(Typedef e1 (Assign e2)) (interp-typedef e1 e2 context module-id debug)]
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

(define (interp-prim2 op v1 v2 module-id debug)
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
        ['mod (if (zero? v2) (cm-error "CONTRACT" "`mod` by zero undefined.")
         (apply-if-type '("int") modulo "mod" v1 v2))
         ]
        ['exp (apply-if-type '("int" "float") expt "exp" v1 v2)]))


(define (interp-prim1 op v context module-id debug)
  (match op
        ['print (interp-print v)]
        ['appnull (interp-apply v null)]
        ['error 
         (match v
                ;; TODO use cm error struct and use id
                [(list id msg) (cm-error id msg)]
                [msg #:when (string? msg) (cm-error "GENERIC" msg)]
                [_ (cm-error "CONTRACT" "Invalid argument(s) to error.")])]
        ['eval #:when (string? v) (run v)] 
        ['eval (cm-error "CONTRACT" "`eval` requires a string argument.")] 
        ['evalxp #:when (string? v) (run-expr v)] 
        ['evalxp (cm-error "CONTRACT" "`evalxp` requires a string argument.")] 
        ['ls 
         (assert-contract (list "string" "list") v "ls")
         (match v
           ["" (map path->string 
                     (try-with-error "GENERIC" "ls: Could not load directory \".\"." 
                                     directory-list (list ".")))]
           [v1 (map path->string 
                     (try-with-error "GENERIC" (format "ls: Could not load directory \"~a\"." v1)
                     directory-list (list v1)))])]
        ['cd 
         (assert-contract (list "string") v "cd")
         (match v
               ["" (path->string 
                     (try-with-error "SYSTEM" "cd: Could not load directory \".\"."
                                     current-directory '()))]
               [v1 
                 (try-with-error "SYSTEM" (format "cd: Could not load directory \"~a\"." v1)
                                      current-directory (list v1)) (Prim0 'void)])]
        ['mkdir 
         (assert-contract (list "string") v "mkdir")
         (match v
               ["" (cm-error "CONTRACT" "Missing argument to `mkdir`.")]
               [v1 
                 (try-with-error "SYSTEM" (format "mkdir: Could not make directory \"~a\"." v1)
                                    make-directory (list v1)) (Prim0 'void)])]
        ['rm 
         (assert-contract (list "string") v "rm")
         (match v
           ["" (cm-error "CONTRACT" "Missing argument to rm")]
           [v1 
             (try-with-error "SYSTEM" (format "rm: Could not delete file or directory \"~a\"." v1)
                                 delete-directory/files (list v1)) (Prim0 'void)])]
        ['getlinesf 
         (assert-contract (list "string") v "getlinesf")
         (match v
           ["" (cm-error "CONTRACT" "Missing argument to `getlinesf`.")]
           [v1 (try-with-error "SYSTEM" (format "getlinesf: Could not load file \"~a\"." v1)
                                   file->lines (list v1))])]
        ['system 
         (assert-contract (list "string") v "system")
         (match v
           [_ (racket-to-bool (system v))])]
        ['sysres
         (assert-contract (list "string") v "sysres")
         (match v
           [_ (with-output-to-string (lambda () (system v)))])]
        ['file_exists? 
         (assert-contract (list "string") v "file_exists?")
         (match v
           ["" (cm-error "CONTRACT" "Missing argument to `file_exists?`")]
           [v1 (racket-to-bool (file-exists? v1))])]
        ['dir_exists? 
         (assert-contract (list "string") v "dir_exists?")
         (match v
           ["" (cm-error "CONTRACT" "Missing argument to `dir_exists?`")]
           [v1 (racket-to-bool (directory-exists? v1))])]
        ['make_hash 
         (match v
           ['() (CmHash (hash) "immutable" '())]
           [(list "mutable") (CmHash (make-hash) "mutable" '())]
           [(list "immutable") (CmHash (hash) "immutable" '())]
           [(list "immutable" f) #:when (is-fun? f) (CmHash (hash) "immutable" f)]
           [(list "mutable" f) #:when (is-fun? f) (CmHash (make-hash) "mutable" f)]
           [(CmHash _ "immutable" _) v]
           ;; deep copy of mutable hash
           [(CmHash h "mutable" handler) (CmHash (hash-copy h) "mutable" handler)]
           [_ (cm-error "CONTRACT" "Invalid arg(s) to `make_hash`.")]
           )]
        ['hash? (racket-to-bool (is-hash? v))]
        ['mutable_hash? (racket-to-bool (is-mutable-hash? v))]
        ;; get-global-var-data will return false if v not yet defined
        ['defined? (interp-defined? v context module-id)]
        ['hash_keys (interp-hash-keys v)]
        ['hash_values (interp-hash-values v)]
        ['hash_to_list (interp-hash-to-list v)]
        ['read_string (interp-read-string v)]
        ['write_string (interp-write-string v)]
        ['write_string_raw (interp-write-string-raw v)]
        ['gensym (interp-gensym v)]
        ['regex (interp-regex v)]
        ['load 
         (match v
           [(list (? string? arg1) (? string? arg2))
            (process-import arg2 arg1 module-id)
            (Prim0 'void)]
           [_ (cm-error "CONTRACT" "Invalid arguments to `load`.")])]
        ['random (interp-random v)]
        ['char_to_int 
         (assert-contract (list "string") v "char_to_int")
         (match (string->list v)
           [(list c) (char->integer c)]
           [_ (cm-error "CONTRACT" "Argument to `char_to_int` must be a single char")])]
        ['int_to_char 
         (assert-contract (list "int") v "int_to_char")
         (with-handlers* 
           ([exn:fail? 
              (lambda (exn) 
                (cm-error "CONTRACT"
                 (format "`int_to_char`: invalid value: ~a" v)))])
           (string (integer->char v)))]
        ['exit 
         ;; exit with 1 if error code is screwy
         (let ([i (match v
                    [(? is-int?) #:when (and (>= v 0) (<= v 255)) v]
                    [_ 1])])
           (displayln i)
           (exit i))]
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
        ['eof? (racket-to-bool (is-eof? v))] 
        ['fun? (racket-to-bool (is-fun? v))] 
        ['not (racket-to-bool
            (apply-if-type-1 '("bool") not-op "not" v))]

        ))

(define (interp-prim0 op)
  (match op
         ['void (Prim0 'void)]
         ['eof (Prim0 'eof)]
         ['read_line (interp-read-line)]
         ))

;;
;; non-standard interp cases
;;

(define (interp-if e1 e2 e3 context module-id debug)
  (if (bool-to-racket (trace-interp-expr e1 context module-id debug))
    (trace-interp-expr e2 context module-id debug)
    (trace-interp-expr e3 context module-id debug)))

(define (interp-while e1 e2 context module-id debug) 
  (cond
    [(bool-to-racket (trace-interp-expr e1 context module-id debug)) 
     (trace-interp-expr e2 context module-id debug)
     (interp-while e1 e2 context module-id debug)]
    [else (Prim0 'void)]))

(define (interp-foreach e1 vs e2 context module-id debug)
  (match vs
    ['() (Prim0 'void)]
    [(cons v t)
      (match (match-expr e1 v context context module-id debug)
            [#f (cm-error "MATCH" "Could not match `foreach` guard expr.")]
            [c2 (trace-interp-expr e2 c2 module-id debug)
                (interp-foreach e1 t e2 context module-id debug)]
            )]
    ))

(define (interp-try-catch e1 e2 e3 context module-id debug)
  (match e2
    [(? var-container? var) 
       (with-handlers* ([exn:fail? (lambda err 
           (match err [(list (exn:fail err-msg _))
               (let ([err-struct 
                   (CmStruct "Error" (list (get-id-from-message err-msg) err-msg))])
          (trace-interp-expr 
            e3 
            (set-local-var 
              (get-var-label var context module-id debug) err-struct context)
            module-id debug))]))])
                       (trace-interp-expr e1 context module-id debug))]
    [_ (cm-error "SYNTAX" "Missing var for `try-catch`.")]))

;; match context is the internal context used inside the match, context is just the usual
;; local context
;;
;; value, expr hash, hash -> value
(define (interp-match v e match-context context module-id debug)
  (match e 
         [(Case ce1 ce2 ce3)
          (match ce2
               [(Yields ce2-1)
                (match ce1
                       [(Prim2 'when ce1-1 ce1-2)
                        (let ([match-context-2 (match-expr ce1-1 v (hash) context module-id debug)])
                          (let ([merged-context (if match-context-2 
                                                  (hash-union match-context match-context-2
                                                    #:combine/key (lambda (k v1 v2) v2))
                                                  #f)])
                        (if (and merged-context 
                                 (bool-to-racket 
                                   (trace-interp-expr ce1-2 merged-context module-id debug)))
                              (trace-interp-expr ce2-1 merged-context module-id debug)
                          (interp-match v ce3 match-context context module-id debug)
                          )))
                        ]
                       [_ 
                        (let ([match-context-2 (match-expr ce1 v (hash) context module-id debug)])
                          ;; hashes evaluate to #t in racket
                        (if match-context-2
                          ;; merge the local contexts so that match-context 2 vals replace
                          ;; match context vals
                          (let ([merged-context (hash-union match-context match-context-2
                                                    #:combine/key (lambda (k v1 v2) v2))])
                              (trace-interp-expr ce2-1 merged-context module-id debug))
                          (interp-match v ce3 match-context context module-id debug)
                          ))

                         ]
                       )
                ]
               [_ (cm-error "SYNTAX" "Missing yields for match case.")])
          ]
         [(Prim0 'end) (cm-error "GENERIC" (format "Matching failed for ~a." (string-coerce v)))]
         [_ (cm-error "SYNTAX" "Invalid match syntax.")]))

;; ast, value, hash, string, debug -> hash | #f
(define (match-expr e v match-context context module-id debug)
  (match e
         [(Int i) (if (equal? i v) match-context #f)]
         [(Float f) (if (equal? f v) match-context #f)]
         [(Bool i1) (match v [(Bool i2) (if (equal? i1 i2) match-context #f)] [_ #f])]
         [(Null) (if (null? v) match-context #f)]
         [(Prim0 'void) (match v [(Prim0 'void) match-context] [_ #f])]
         [(String s) (if (string=? s v) match-context #f)]
         [(Prefix2 '? (? var-container? var) expr) 
          (let* ([func (trace-interp-expr expr context module-id debug)]
                 [res (interp-apply func v)])
            (unless (is-bool? res) 
              (cm-error "CONTRACT"
                (format "`?`: Function must return a bool value,\ngot: ~a" 
                  (string-coerce res))))
            (if (bool-to-racket res) 
              (match-expr var v match-context context module-id debug)
              #f)
            )]
         ;; implied dynamic var
         [(? var-container? var) 
          (match (get-var-label var context module-id debug)
            ;; wildcard, always match
            ["_" match-context]
            [label (match-var label (list "dynamic") v match-context)])]
         [(Prim1 op (? var-container? var))
          #:when (member op guard-types)
          (match (get-var-label var context module-id debug)
            ["_" (cm-error "CONTRACT" "Only plain wildcard is acceptable in match expr.")]
            [label 
              (match-var label (list (symbol->string op)) v match-context)])]
         [(Prefix2 'types types-expr (? var-container? var)) 
          (match (get-var-label var context module-id debug)
            ["_" (cm-error "CONTRACT" "Only plain wildcard is acceptable in match expr.")]
            [label 
              (match-var
                label
                (check-types-list (trace-interp-expr types-expr context module-id debug))
                v match-context)])]
         [(Prefix2 'struct (? var-container? var) lst) 
          #:when (expr-is-list? lst)
          (match (get-var-label var context module-id debug)
            ["_" (cm-error "CONTRACT" "Only plain wildcard is acceptable in match expr.")]
            [label 
              (match v
                [(CmStruct label2 lst2) 
                 #:when (equal? label label2)
                 (match-expr lst lst2 match-context context module-id debug)]
                [_ #f]
            )])]

         ;; a, ...
         [(Prim2 'cons (? var-container? var) (Prim0 '...))
          ;; just return everything
          (match-var 
            (get-var-label var context module-id debug) 
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
                        (get-var-label var context module-id debug) 
                        (list "dynamic") res match-context)])
                 (if (equal? match-context #f) #f
                   (match-expr 
                     es remaining match-context
                     context module-id debug)))]))]
         [(Prim2 'cons _ (Prim2 'cons (Prim0 '...) _))
          (cm-error "SYNTAX" "Improperly formed ellipsis in match expr.")]

         [(Prim2 'cons e1 e2)
          (match v
                 [(cons v1 v2)
                    (let ([mc2 (match-expr e1 v1 match-context context module-id debug)])
                        (if (not (equal? mc2 #f)) 
                          (match-expr e2 v2 mc2 context module-id debug) #f))]
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

(define (interp-case e1 e2 e3 context module-id debug)
  (match e2
         [(Yields e2-2)
          (match e3
                 [(Case e3-1 e3-2 e3-3) 
                  (if (bool-to-racket 
                        (trace-interp-expr e1 context module-id debug)) 
                    (trace-interp-expr e2-2 context module-id debug)
                    (interp-case e3-1 e3-2 e3-3 context module-id debug))]
                 [(Else e3-3)
                  (if (bool-to-racket (trace-interp-expr e1 context module-id debug))
                    (trace-interp-expr e2-2 context module-id debug)
                    (trace-interp-expr e3-3 context module-id debug))]
                 [_ (cm-error "SYNTAX" "Case must end in else or another case.")]
                 )]
         [_ (cm-error "SYNTAX" "Case is missing yields.")]))

(define (interp-var var context module-id)
  ;; check local context first
  (let ([res (get-local-var-data var context)])
  (if (not (false? res)) res 
  ;; then check global
  (match (get-global-var-data var module-id)
         [#f (cm-error "UNDEFINED" (format "Var ~a has not yet been defined." var))]
         [res res]))))


;; gets the label from a left-side expr (such as `int x` in (def int x := 3)),
;; checks it against the given value,
;; then returns the guard type(s) and label. If the guard is improperly formed, then
;; the procedure yields (values #f #f).
;;
;; If the guard is a null expr then (values '() '()) is returned
;;
;; guard-expr, context, string, debug ->
;;      (values (string | fun) list, (string | null)) | (values bool, bool)
(define (interp-left-hand-expr e context module-id debug)
  (match e
      [(Null) (values '() '())]
      ;; function guard
      [(Prefix2 '? (? var-container? var) expr) 
       (let ([func (trace-interp-expr expr context module-id debug)]
             [label (get-var-label var context module-id debug)])
         (unless (is-fun? func) 
           (cm-error "CONTRACT" 
             (format "Var `~a`: Argument to `?` must be a function." label)))
         (values (list func) label))]
      [(Prim1 op (? var-container? var)) #:when (member op guard-types)
       (values (list (symbol->string op)) 
               (get-var-label var context module-id debug))]
      [(Prefix2 'types types-expr (? var-container? var))
       (let ([types-list 
               (check-types-list (trace-interp-expr types-expr context module-id debug))]) 
           (values types-list (get-var-label var context module-id debug)))]
      [(Prefix2 'struct (? var-container? s-var) (? var-container? var))
       (let ([guard-type 
               (get-struct-type-string (get-var-label s-var context module-id debug))])
           (values (list guard-type) (get-var-label var context module-id debug)))]
      ;; implied dynamic case
      [(? var-container? var) 
       (values (list "dynamic") 
         (get-var-label var context module-id debug))]
      [_ (values #f #f)]))

(define (interp-def guard-types label v context module-id update? debug)
  ;(let-values ([(guard-types label) 
                  ;(interp-left-hand-expr e1 context module-id debug)])
 (match label
      [#f #:when update? 
        (cm-error "SYNTAX" "Unknown Item on left hand of `set`.")] 
      [#f (cm-error "SYNTAX" "Unknown Item on left hand of `def`.")]
      ['() (cm-error "SYNTAX" "`def` is missing a variable.")]
      ;; sets var in global context hash and returns value to caller
      [_
        (assign-type-check guard-types v label)
        (if update? 
         (update-global-var! label v module-id)
         (set-global-var! label v (var-name-private? label) module-id))
       v]))
         

(define (interp-let e1 e2 e3 context module-id debug)
  (let-values ([(v2) (trace-interp-expr e2 context module-id debug)] 
               [(guard-types label)
                (interp-left-hand-expr e1 context module-id debug)])
    (match label
         [#f (cm-error "SYNTAX" "Unknown Item on left hand of let.")]
         ['() (cm-error "SYNTAX" "Let is missing a variable.")]
         [_ 
            (assign-type-check guard-types v2 label)
            (trace-interp-expr 
              e3 (set-local-var label v2 context) module-id debug)])))

;; name is the name of the function and is either a string 
;; or '() for anonymous functions
(define (interp-lambda e1 e2 name context module-id debug)
  (let-values ([(guard-types label) 
                 (interp-left-hand-expr e1 context module-id debug)])
    (match label
           [#f (cm-error "SYNTAX" "Unknown Item on left hand of `lambda`")]
           ['() (Fun name '() '() context e2 module-id debug)]
           [_ (Fun name label guard-types context e2 module-id debug)]
    )))

(define (interp-apply v1 v2)
  (let ([trace
          (lambda (name id debug)
            (push-elem-to-trace-stack! 
              (TraceElem "fun" (if (null? name) "lambda" name)
                    id (debug->linenum debug))))])
  (match v1
         ;; no arg lambda
         [(Fun name '() '() fcontext fexpr fmodule-id fdebug)
          ;; trace
          (trace name fmodule-id fdebug)

          (trace-interp-expr fexpr fcontext fmodule-id fdebug)]
         [(Fun name var types fcontext fexpr fmodule-id fdebug)
          ;; trace
          (trace name fmodule-id fdebug)

          ;; check val against guards
          (assign-type-check types v2 var)
          (trace-interp-expr fexpr (set-local-var var v2 fcontext) fmodule-id fdebug)]
         [_ (cm-error "CONTRACT" "Attempted to apply onto a non function.")])))


(define (interp-appl e1 e2 context module-id debug)
  (match (trace-interp-expr e2 context module-id debug)
    [l1 #:when (list? l1) 
      (let aux ([lst l1] [res (trace-interp-expr e1 context module-id debug)])
        (match lst
               ['() res]
               [(cons h t) (aux t (interp-apply res h))]

               ))]
    [_ (cm-error "CONTRACT" "Arguments to appl must be a list.")]

          ))

(define (interp-typedef e1 e2 context module-id debug)
 (let ([lst2 
         (match (ast-cons-to-racket e2)
           [(? list? res) res] 
           [_ (cm-error 
                "SYNTAX" 
                "Invalid schema for `typedef`. Schema must be a list.")])]
       [id 
         (match e1
           [(? var-container? var)
            (get-var-label var context module-id debug)]
           [_ (cm-error "SYNTAX" "Missing Label for `typedef`.")]
           )])
  (let verify-schema ([lst lst2] [acc '()] [acc/labels '()])
   (match lst
      ;; set schema if no errors were found
      ['() (set-type! id (reverse acc) (var-name-private? id) module-id) (Prim0 'void)]
      [(cons h t)
       (let-values 
         ([(guard-types label) (interp-left-hand-expr h context module-id debug)])
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

(define (interp-struct e1 e2 context module-id debug)
  (match e1
         [(? var-container? var)
          (let ([label (get-var-label var context module-id debug)])
          (match (trace-interp-expr e2 context module-id debug)
                 ;; struct args must be a list (null for no args, still technically a list)
                 [res #:when (list? res) 
                      (match (get-type-data label module-id)
                             [#f (cm-error "UNDEFINED" (format "Type \"~a\" has not been declared." label))]
                             [schema
                            (match (valid-against-schema? label schema res)
                                   [#t (CmStruct label res)]
                                   [#f (cm-error "CONTRACT" 
                (format (string-append "Could not validate struct against type schema:"
                                       "\nstruct:\n~a ~a\nschema:\n~a")
                        label (string-coerce res) (struct-schema->string (get-type-data label module-id))))])])]
                 [_ (cm-error "SYNTAX" "Arguments to `struct` must be a list or null.")]))]
         [_ (cm-error "SYNTAX" "Missing label for `struct`.")]))

(define (interp-struct? e1 e2 context module-id debug)
  (match e1
   [(? var-container? var) 
    (let ([label (get-var-label var context module-id debug)])
      (match label
        ;; wildcard accepts any struct
        ["_" 
         (racket-to-bool 
           (is-struct? (trace-interp-expr e2 context module-id debug)))]
        [_ 
           (racket-to-bool 
             (is-struct-type? 
               label
               (trace-interp-expr e2 context module-id debug)))]))]
   [_ (cm-error "SYNTAX" "Missing label for struct question.")]))

(define (interp-index e1 e2 context module-id debug)
  (match (trace-interp-expr e1 context module-id debug)
         [vs #:when (list? vs) 
             (match (trace-interp-expr e2 context module-id debug)
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
             (match (trace-interp-expr e2 context module-id debug)
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
            (interp-hash-ref h (trace-interp-expr e2 context module-id debug))]
         [(CmStruct label lst)
            (let ([index
                    (match e2 
                      [(? var-container? var)
                       (get-var-label var context module-id debug)]
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

(define (interp-hash-set v1 v2 v3)
  (match v1
        [(CmHash h "immutable" handler) (CmHash (hash-set h v2 v3) "immutable" handler)]
        [(CmHash h "mutable" handler) (hash-set! h v2 v3) (Prim0 'void)]
        [_ (cm-error "CONTRACT" "First argument to `hash_set` must be a hash.")]
        ))

(define (interp-hash-ref v1 v2)
  (match v1
         [(CmHash h _ '()) (hash-ref h v2 
                    (lambda () (cm-error "HASHREF" 
                        (format "Could not find key ~a in hash." (string-coerce v2)))))]
         [(CmHash h _ handler) (hash-ref h v2
                    (lambda () (interp-apply handler v2)))]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_ref`.")]))

(define (interp-hash-has-key? v1 v2)
  (match v1
         [(CmHash h _ _) (racket-to-bool (hash-has-key? h v2))]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_has_key?`.")]))

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

(define (interp-hash-ref-check v1 v2 v3)
  (match v1
         [(CmHash h _ _)
            (if (is-fun? v3)
                (hash-ref h v2
                    (lambda () (interp-apply v3 v2)))
                (cm-error "CONTRACT" "Third arg to `hash_ref_check` must be a function."))]
         [_ (cm-error "CONTRACT" "Missing hash for `hash_ref_check`.")]))

(define (interp-print v)
 (begin (displayln v) 
        v))

;; reads until i chars or eof
(define (interp-read-string v1)
  (assert-contract (list "int") v1 "read_string")
  (unless (>= v1 0) 
    (cm-error "CONTRACT" 
        (format "Argument to `read_string` must be greater than zero.\nGot: ~a" v1)))
  (read-string v1))

(define (interp-peek-string v1 v2)
  (assert-contract (list "int") v1 "peek_string")
  (assert-contract (list "int") v2 "peek_string")
  (peek-string v1 v2))

(define (interp-cp v1 v2)
  (assert-contract (list "string") v1 "cp")
  (assert-contract (list "string") v2 "cp")
  (match (cons v1 v2) 
       [(cons v1 v2) #:when (or (string=? v1 "") (string=? v2 "")) 
                     (cm-error "CONTRACT" "Missing argument to `cp`.")]
       [(cons v1 v2)
        (with-handlers* 
         ([exn:fail? (lambda (exn) 
          (cm-error "SYSTEM" (format "Could not copy \"~a\" to \"~a\"" v1 v2)))])
         (copy-directory/files v1 v2))
        (Prim0 'void)]))

(define (interp-appendstrf v1 v2)
  (assert-contract (list "string") v1 "appendstrf")
  (assert-contract (list "string") v2 "appendstrf")
  (match (cons v1 v2) 
    [(cons v1 v2) #:when (or (string=? v1 "") (string=? v2 "")) 
                  (cm-error "CONTRACT" "Missing argument to `appendstrf`.")]
    [(cons v1 v2)
     (with-handlers* 
       ([exn:fail? (lambda (exn) 
         (cm-error "SYSTEM" (format "Could not append to file \"~a\"" v2)))])
       (display-to-file v1 v2 #:exists 'append))
     (Prim0 'void)]))

(define (interp-writestrf v1 v2)
  (assert-contract (list "string") v1 "writestrf")
  (assert-contract (list "string") v2 "writestrf")
  (match (cons v1 v2) 
   [(cons v1 v2) #:when (or (string=? v1 "") (string=? v2 "")) 
                 (cm-error "CONTRACT" "Missing argument to `writestrf`.")]
   [(cons v1 v2)
    (with-handlers* 
      ([exn:fail? (lambda (exn) 
        (cm-error "SYSTEM" (format "Could not write to file \"~a\"" v2)))])
      (display-to-file v1 v2 #:exists 'replace))
    (Prim0 'void)]))

(define (interp-read-line) (read-line))

(define (interp-write-string v1)
  (assert-contract (list "string") v1 "write_string")
  (display v1) (Prim0 'void))

;; not sure what to do with this, not in standard documentation for now
(define (interp-write-string-raw v1)
  (assert-contract (list "string") v1 "write_string_raw")
  (display v1) (Prim0 'void))

(define last-gensym-id 0)
(define (interp-gensym v1)
  (unless (is-string? v1) 
    (cm-error "CONTRACT" "The prefix for `gensym` must be a string."))
  (let ([id last-gensym-id])
    (set! last-gensym-id (add1 id))
    (string-append v1 (number->string id))
    ))

(define (interp-defined? v1 context current-module-id)
  (match v1
         [(list (? is-string? label) (? is-string? id))
          (racket-to-bool 
            (or
              (get-local-var-data label context)
              (get-global-var-data label id)))]
         [(? is-string? label) 
          (racket-to-bool
            (or
              (get-local-var-data label context)
              (get-global-var-data label current-module-id)))]))

(define (interp-schemaof e1 context module-id debug)
  (match e1
    [(? var-container? var)
     (get-struct-schema-labels
       (get-var-label var context module-id debug) module-id)]
    [_ (cm-error "CONTRACT" "Argument to `schemaof` must be a variable.")]
    ))

(define (interp-regex v1)
  (assert-contract (list "list") v1 "regex")
   ;; the second argument to `regex` takes a list of the form (type, regex;),
   ;; where type = "quote" | "px" | "rx",
   ;; and regex is the actual regex.
   ;;
   ;; "quote" means the regex will be formed using `regexp-quote`
   ;; in racket
  (let ([pattern 
          (match v1 
             [(list type pattern args ...)
              (assert-contract (list "list") pattern 
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
                )])])
  (match v1
         [(list "regexp-match" _ str)
          (assert-contract (list "string") str "regex -> regexp-match")
          (match (regexp-match pattern str)
                 [(? boolean? res) (racket-to-bool res)]
                 [res res])
          ]
         [(list "regexp-match*" _ str)
          (assert-contract (list "string") str "regex -> regexp-match*")
          (match (regexp-match* pattern str #:match-select values)
                 [res res])
          ]
         [(list "regexp-match?" _ str)
          (assert-contract (list "string") str "regex -> regexp-match?")
          (racket-to-bool (regexp-match? pattern str))]
         [(list "regexp-split" _ str)
          (assert-contract (list "string") str "regex -> regexp-split")
          (match (regexp-split pattern str)
                 [res res])
          ]
         [(list "regexp-replace" _ str insert)
          (assert-contract (list "string") str "regex -> regexp-replace")
          (assert-contract (list "string") insert "regex -> regexp-replace")
          (match (regexp-replace pattern str insert)
                 [res res])
          ]
         [(list "regexp-replace*" _ str insert)
          (assert-contract (list "string") str "regex -> regexp-replace")
          (assert-contract (list "string") insert "regex -> regexp-replace")
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
