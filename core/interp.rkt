#lang racket
(require cm/core/ast cm/core/error cm/core/types)
(provide interp)
(define error-id 3)

;; Matthew Dolinka
;; cm interpreter


(define (interp ast) (string-coerce (interp-aux ast)))

(define (interp-aux ast)
  (match ast
        [(Stat i e st) (interp-statement i e st)]
        [(EOP) (void)]
        [(Prim2 op e1 e2) (interp-prim2 op (interp-aux e1) (interp-aux e2))]
        [(Prim1 op e) (interp-prim1 op (interp-aux e))]
        [(If e1 e2) (interp-if e1 e2)]
        [(Cond e) 
         (match e
                [(Case _ _ _) (interp-aux e)]
                [_ (cm-error error-id "Cond is missing a case.")])]
        [(Case e1 e2 e3) 
         (if (bool-to-racket (interp-aux e1)) (interp-aux e2) 
           (match e3 
                  [(Case _ _ _) (interp-aux e3)]
                  [(Else _) (interp-aux e3)]
                  [(End) (cm-error error-id "Cond matching failed. Hit end.")]
                  [_ (cm-error error-id "Cond is missing its components.")]))]
        [(Yields e) (interp-aux e)]
        [(Else e) (interp-aux e)]
        [(Print e) (interp-print e)]
        [(Error e) (error (string-coerce (interp-aux e)))] 
        [(Eval s) (eval-string (string-coerce (interp-aux s)))] 
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

(define (interp-prim2 op v1 v2)
  (match op
        ['cons (cons v1 v2)]
        ['cat (string-append (string-coerce v1)
                           (string-coerce v2))]
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
        ['not (racket-to-bool (not (bool-to-racket v)))]))

;;
;; sub cases
;;

;; todo: string statements together
(define (interp-statement linenum e st)
  (match e
         [(Noop) (match st [(EOP) (void)] [_ (interp-aux st)])]
         [_ 
           (match st 
                  [(EOP) (cm-error-with-line-handler linenum interp-aux e)]
                  [_ (cm-error-with-line-handler linenum interp-aux e) (interp-aux st)])]))

(define (interp-if e1 e2)
  (match e2
         [(Then e3 e4) 
          (match e4
                 [(Else e5)
                     (if (bool-to-racket (interp-aux e1)) (interp-aux e3) (interp-aux e5))]
                 [_ (cm-error error-id "Then clause is missing an else.")])]
         [_ (cm-error error-id "If clause is missing a then.")]))

;(define (interp-cond e1 e2)
    ;(match e2
           ;[(Else e3)
                ;(let ([v1 (interp e1)] [v2 (interp e3)])
                    ;(let aux ([args (check-argument-dimensions
                                      ;(check-list-arguments v1 "cond") 2 2 "cond")]) 
                           ;(match args
                                  ;['() v2]
                                  ;[(cons h t) #:when (bool-to-racket (car h)) (cadr h)]
                                  ;[(cons h t) (aux t)])))]
           ;[_ (cm-error error-id "Cond clause is missing an else.")]))

(define (interp-print e)
  (let ([v (interp-aux e)])
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


;; converts a pair to a list if it wasn't already
;(define (pair-to-list p)
  ;(match p
        ;['() '()] ;; already a list
        ;[(cons h t) (cons h (pair-to-list t))]
        ;[h (cons h '())]))

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
