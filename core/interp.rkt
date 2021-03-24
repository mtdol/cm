#lang racket
(require racket/hash racket/lazy-require
         cm/core/ast cm/core/error cm/core/types cm/core/context
         cm/core/parse-stat cm/core/lex cm/core/interp-utils cm/core/modules)
(lazy-require [cm/core/main (run run-expr)])
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
                ;; each statement in a chain of statements is consed together
                ;; into a list
                [(EOP) (cons (prepare-for-output (interp-prep e i)) '())]
                [_ (cons (prepare-for-output (interp-prep e i)) (interp st))])]
        ;; if we get an EOP only, just return void
        [(EOP) (prepare-for-output (interp-expr (Prim0 'void) (hash)))]
        ;; expr only case, no list, just a value returned
        [_ (prepare-for-output (interp-expr ast (hash)))]))

(define (interp-expr ast context)
  (match ast
        [(Prefix2 'struct e1 e2) (interp-struct e1 e2 context)]
        [(Prefix2 'struct? e1 e2) (interp-struct? e1 e2 context)]
        [(Prefix2 'appl e1 e2) (interp-appl e1 e2 context)]
        [(Prefix2 'index e1 e2) (interp-index e1 e2 context)]
        [(Prim2 'index2 e1 e2) (interp-index e2 e1 context)]
        [(Prefix2 'writestrf e1 e2) (match 
                (cons (string-coerce
                        (interp-expr e1 context)) 
                      (string-coerce 
                        (interp-expr e2 context)))
                    [(cons v1 v2) #:when (or (string=? v1 "") (string=? v2 "")) 
                                  (cm-error "CONTRACT" "Missing argument to writestrf.")]
                    [(cons v1 v2) (with-handlers* 
                            ([exn:fail? (lambda (exn) 
                                (cm-error "SYSTEM" (format "Could not write to file \"~a\"" v2)))])
                       (display-to-file v1 v2 #:exists 'replace)) (Prim0 'void)])]
        [(Prefix2 'appendstrf e1 e2) (match 
                (cons (string-coerce
                        (interp-expr e1 context)) 
                      (string-coerce 
                        (interp-expr e2 context)))
                    [(cons v1 v2) #:when (or (string=? v1 "") (string=? v2 "")) 
                                  (cm-error "CONTRACT" "Missing argument to appendstrf.")]
                    [(cons v1 v2) (with-handlers* 
                            ([exn:fail? (lambda (exn) 
                                (cm-error "SYSTEM" (format "Could not append to file \"~a\"" v2)))])
                       (display-to-file v1 v2 #:exists 'append)) (Prim0 'void)])]
        [(Prefix2 'cp e1 e2) (match 
                (cons (string-coerce
                        (interp-expr e1 context)) 
                      (string-coerce 
                        (interp-expr e2 context)))
                    [(cons v1 v2) #:when (or (string=? v1 "") (string=? v2 "")) 
                                  (cm-error "CONTRACT" "Missing argument to cp.")]
                    [(cons v1 v2) (with-handlers* 
                            ([exn:fail? (lambda (exn) 
                                (cm-error "SYSTEM" (format "Could not copy \"~a\" to \"~a\"" v1 v2)))])
                       (copy-directory/files v1 v2)) (Prim0 'void)])]
        [(Prefix2 'mv e1 e2) (interp-expr (Prefix2 'cp e1 e2) context)
                             (interp-expr (Prim1 'rm e1) context)]
        [(Prefix2 'hash_ref e1 e2) (interp-hash-ref 
                                     (interp-expr e1 context)
                                     (interp-expr e2 context))]
        [(Prefix2 'hash_has_key? e1 e2) (interp-hash-has-key? 
                                     (interp-expr e1 context)
                                     (interp-expr e2 context))]
        [(Prefix2 'peek_string e1 e2) (interp-peek-string 
                                     (interp-expr e1 context)
                                     (interp-expr e2 context))]
        [(Prefix3 'hash_set e1 e2 e3) 
         (interp-hash-set 
           (interp-expr e1 context)
           (interp-expr e2 context)
           (interp-expr e3 context)
           )]
        [(Prefix3 'hash_ref_check e1 e2 e3) 
         (interp-hash-ref-check 
           (interp-expr e1 context)
           (interp-expr e2 context)
           (interp-expr e3 context)
           )]
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
        [(Foreach e1 (In e2) (Do e3))
         (let ([vs (interp-expr e2 context)])
           (if (is-list? vs)
               (interp-foreach e1 vs e3 context)
               (cm-error "CONTRACT" "Argument to foreach guard must be a list.")))]
        [(Foreach _ _ _) (cm-error "SYNTAX" "Foreach is improperly formed.")]
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
        ['mod (if (zero? v2) (cm-error "CONTRACT" "Mod by zero undefined.")
         (apply-if-type '("int") modulo "mod" v1 v2))
         ]
        ['exp (apply-if-type '("int" "float") expt "exp" v1 v2)]))
 
(define (interp-prim1 op v)
  (match op
        ['print (interp-print v)]
        ['apply1 (interp-apply null v)]
        ['error 
         (match v
                ;; TODO use cm error struct and use id
                [(list id msg) (cm-error-no-linenum id msg)]
                [msg #:when (string? msg) (cm-error-no-linenum "GENERIC" msg)]
                [_ (cm-error "CONTRACT" "Invalid argument(s) to error.")])]
        ['eval #:when (string? v) (run v)] 
        ['eval (cm-error "CONTRACT" "eval requires a string argument.")] 
        ['evalxp #:when (string? v) (run-expr v)] 
        ['evalxp (cm-error "CONTRACT" "evalxp requires a string argument.")] 
        ['load 
         (match v 
                [s #:when (string? s) (process-import-string s) (Prim0 'void)]
                [_ (cm-error "CONTRACT" "Argument to load must be a file.")])
             ]
        ['ls 
         (match (string-coerce v)
                    ["" (map path->string 
                             (try-with-error "GENERIC" "ls: Could not load directory \".\"." 
                                             directory-list (list ".")))]
                    [v1 (map path->string 
                             (try-with-error "GENERIC" (format "ls: Could not load directory \"~a\"." v1)
                             directory-list (list v1)))])]
        ['cd (match (string-coerce v)
                    ["" (path->string 
                          (try-with-error "SYSTEM" "cd: Could not load directory \".\"."
                                          current-directory '()))]
                    [v1 
                      (try-with-error "SYSTEM" (format "cd: Could not load directory \"~a\"." v1)
                                      current-directory (list v1)) (Prim0 'void)])]
        ['mkdir (match (string-coerce v)
                    ["" (cm-error "CONTRACT" "Missing argument to mkdir.")]
                    [v1 
                      (try-with-error "SYSTEM" (format "cd: Could not make directory \"~a\"." v1)
                                      make-directory (list v1)) (Prim0 'void)])]
        ['rm (match (string-coerce v)
                    ["" (cm-error "CONTRACT" "Missing argument to rm")]
                    [v1 
                      (try-with-error "SYSTEM" (format "cd: Could not delete file or directory \"~a\"." v1)
                                      delete-directory/files (list v1)) (Prim0 'void)])]
        ['getlinesf (match (string-coerce v)
                    ["" (cm-error "CONTRACT" "Missing argument to getlinesf.")]
                    [v1 (try-with-error "SYSTEM" (format "getlinesf: Could not load file \"~a\"." v1)
                                        file->lines (list v1))])]
        ['system (racket-to-bool (system (string-coerce v)))]
        ['sysres (with-output-to-string (lambda () (system (string-coerce v))))]
        ['file_exists? 
         (match (string-coerce v)
                    ["" (cm-error "CONTRACT" "Missing argument to file_exists?")]
                    [v1 (racket-to-bool (file-exists? (string-coerce v1)))])]
        ['dir_exists? 
         (match (string-coerce v)
                    ["" (cm-error "CONTRACT" "Missing argument to dir_exists?")]
                    [v1 (racket-to-bool (directory-exists? (string-coerce v1)))])]
        ['make_hash 
         (match v
           ['() (CmHash (hash) "immutable" '())]
           ["mutable" (CmHash (make-hash) "mutable" '())]
           ["immutable" (CmHash (hash) "immutable" '())]
           [(list "immutable" f) #:when (is-fun? f) (CmHash (hash) "immutable" f)]
           [(list "mutable" f) #:when (is-fun? f) (CmHash (make-hash) "mutable" f)]
           [(CmHash _ "immutable" _) v]
           ;; deep copy of mutable hash
           [(CmHash h "mutable" handler) (CmHash (hash-copy h) "mutable" handler)]
           [_ (cm-error "CONTRACT" "Invalid arg(s) to make_hash.")]
           )]
        ['hash? (racket-to-bool (is-hash? v))]
        ['mutable_hash? (racket-to-bool (is-mutable-hash? v))]
        ['hash_keys (interp-hash-keys v)]
        ['hash_values (interp-hash-values v)]
        ['hash_to_list (interp-hash-to-list v)]
        ['read_string (interp-read-string v)]
        ['write_string (interp-write-string v)]
        ['write_string_raw (interp-write-string-raw v)]
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

(define (interp-foreach e1 vs e2 context)
  (match vs
    ['() (Prim0 'void)]
    [(cons v t)
      (match (match-expr e1 v context context)
            [#f (cm-error "MATCH" "Could not match foreach guard expr.")]
            [c2 (interp-expr e2 c2) (interp-foreach e1 t e2 context)]
            )]
    ))

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
                   ;; no arg lambda
                   [null
                     (Fun '() (list) context e3)]
                   [_ (cm-error "SYNTAX" "Unknown Item on left hand of lambda")]
                   )]
         [_ (cm-error "SYNTAX" "Lambda is missing an assignment.")]))

(define (interp-apply v1 v2)
  (match v2
         ;; no arg lambda
         [(Fun '() '() fcontext fexpr) 
          (interp-expr fexpr fcontext)] 
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
                             [#f (cm-error "UNDEFINED" (format "Type ~a has not been declared." v))]
                             [schema
                            (match (valid-against-schema? v schema res)
                                   [#t (CmStruct v res)]
                                   [#f (cm-error "CONTRACT" 
                (format (string-append "Could not validate struct against type schema:"
                                       "\nstruct:\n~a ~a\nschema:\n~a")
                        v (string-coerce res) (struct-schema->string (get-type-data v))))])])]
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
         [h #:when (is-hash? h) (interp-hash-ref h (interp-expr e2 context))]
         [s (cm-error "CONTRACT" (format "Invalid operand for index: ~a" (string-coerce s)))]
         ))

(define (interp-hash-set v1 v2 v3)
  (match v1
        [(CmHash h "immutable" handler) (CmHash (hash-set h v2 v3) "immutable" handler)]
        [(CmHash h "mutable" handler) (hash-set! h v2 v3) (Prim0 'void)]
        [_ (cm-error "CONTRACT" "First argument to hash_set must be a hash.")]
        ))

(define (interp-hash-ref v1 v2)
  (match v1
         [(CmHash h _ '()) (hash-ref h v2 
                    (lambda () (cm-error "HASHREF" 
                        (format "Could not find key ~a in hash." (string-coerce v2)))))]
         [(CmHash h _ handler) (hash-ref h v2
                    (lambda () (interp-apply v2 handler)))]
         [_ (cm-error "CONTRACT" "Missing hash for hash_ref.")]))

(define (interp-hash-has-key? v1 v2)
  (match v1
         [(CmHash h _ _) (racket-to-bool (hash-has-key? h v2))]
         [_ (cm-error "CONTRACT" "Missing hash for hash_has_key?.")]))

(define (interp-hash-keys v1)
  (match v1
         [(CmHash h _ _) (hash-keys h)]
         [_ (cm-error "CONTRACT" "Missing hash for hash_keys.")]))

(define (interp-hash-values v1)
  (match v1
         [(CmHash h _ _) (hash-values h)]
         [_ (cm-error "CONTRACT" "Missing hash for hash_values.")]))

(define (interp-hash-to-list v1)
  (match v1
         [(CmHash h _ _) (hash->list h)]
         [_ (cm-error "CONTRACT" "Missing hash for hash_to_list.")]))

(define (interp-hash-ref-check v1 v2 v3)
  (match v1
         [(CmHash h _ _)
            (if (is-fun? v3)
                (hash-ref h v2
                    (lambda () (interp-apply v2 v3)))
                (cm-error "CONTRACT" "Third arg to hash_ref_check must be a function."))]
         [_ (cm-error "CONTRACT" "Missing hash for hash_ref_check.")]))

(define (interp-print v)
 (begin (displayln (value->displayable-string v)) 
        v))

;; reads until i chars or eof
(define (interp-read-string v1)
  (if (is-int? v1)
    (read-string v1)
    (cm-error "CONTRACT" "Invalid arg(s) to read_string")))

(define (interp-peek-string v1 v2)
  (if (and (is-int? v1) (is-int? v2))
    (peek-string v1 v2)
    (cm-error "CONTRACT" "Invalid arg(s) to peek_string")))

(define (interp-read-line) (read-line))

(define (interp-write-string v1)
  (display (value->displayable-string v1)) (Prim0 'void))

(define (interp-write-string-raw v1)
  (display (string-coerce v1)) (Prim0 'void))
