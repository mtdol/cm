#lang racket
(require cm/core/error cm/core/types cm/core/ast
         cm/core/operators cm/core/parse-utils
         cm/core/pre-parse)
(provide parse-expr half-parse-expr)
(define error-id "PARSE")

;; for use in eval of ast nodes
(define ns (module->namespace 'cm/core/ast))


;; Matthew Dolinka
;; cm parser


(define (parse-expr tokens) 
  (parse-to-ast (tokens-to-prefix-form (pre-parse-expr tokens))))

(define (half-parse-expr tokens)
  (tokens-to-prefix-form (pre-parse-expr tokens)))

;; (1 + 2 * 3) -> + 1 * 2 3
;; returned expresions are parenthesis free
(define (tokens-to-prefix-form tokens) 
    (match tokens
           ['() '()]
           ;; fundamental value
           [(list h) #:when (not (is-operator? h)) (list h)] 
           ;; unwrap all outer parens from token list, convert to s-expr and
           ;; then rewrap with one layer of parens
           [tokens (tokens-to-prefix-form-aux (unwrap-expr tokens) '() 0 0)])) 

(define (tokens-to-prefix-form-aux tokens acc preced pcount) 
    (match tokens
         ;; list looks like "3 (5 + 8)"
         ;; we must recurse and deal with the subexprs
        ['() #:when (= preced max-precedences) 
         ;; we will collect subexprs and prefixify them while walking down the list
            (let aux ([tokens (reverse acc)] [acc '()] [pcount 0])
                (match tokens
                       ['() (reverse acc)]
                       [(cons ")" t) #:when (= 1 pcount)
                            (append (tokens-to-prefix-form (reverse (cons ")" acc))) 
                                    (aux t '() 0))]
                       [(cons "(" t) #:when (= 0 pcount)
                            (append (reverse acc) 
                                    (aux t '("(") 1))]
                       [(cons "(" t) (aux t (cons "(" acc) (add1 pcount))]
                       [(cons ")" t) (aux t (cons ")" acc) (sub1 pcount))]
                       [(cons h t) (aux t (cons h acc) pcount)]))]
        ;; try again since we didn't find an op of current precedence
        ['() (tokens-to-prefix-form-aux (reverse acc) '() (add1 preced) 0)]
        ;; increment pcount
        [(cons "(" t) (tokens-to-prefix-form-aux t (cons "(" acc) preced (add1 pcount))]
        ;; decrement pcount
        [(cons ")" t) (tokens-to-prefix-form-aux t (cons ")" acc) preced (sub1 pcount))]
        ;; infix op found matching precedence
        ;; reverse acc because it is wrong order
        [(cons h t) #:when (and (zero? pcount) (is-operator? h) (= (op-to-precedence h) preced)
                                (string=? (op-to-position h) "prefix"))
                    (append (tokens-to-prefix-form (reverse acc))
                            (list h) (tokens-to-prefix-form t))]
        ;; prefix op found matching precedence
        [(cons h t) #:when (and (zero? pcount) (is-operator? h) (= (op-to-precedence h) preced)
                                (string=? (op-to-position h) "infix"))
            (append (list h) (tokens-to-prefix-form (reverse acc))
                          (tokens-to-prefix-form t))]
        ;; fundamental value
        [(list h) #:when (and (null? acc) (not (is-operator? h))) (list h)] 
        ;; general case
        [(cons h t) (tokens-to-prefix-form-aux t (cons h acc) preced pcount)]))

;; used in parse-to-ast to check for expressions with too many operands.
(define expr-tail '())

;; Takes in a list of tokes in prefix order and produces an ast.
;; Does not accept expressions with parenthesis.
(define (parse-to-ast tokens)
  (match tokens
         ['() (Prim0 'void)]
         [(cons h '()) (parse-value h)]
         [(cons h t) #:when (is-operator? h) 
                     (match (parse-op h t) 
                            [ast #:when (not (null? expr-tail))
            (cm-error error-id 
                      "Invalid Expression. Probably missing an operator.")]
                            [ast ast])]
         [_ (cm-error error-id "Invalid Expression. Probably missing an operator.")]))

(define (parse-op op tail)
  (let ([nodes (acc-operands tail '() (op-to-arity op) op)]
        [name1 (op-to-node-name op)]
        [name2 (op-to-extra-node-name op)])
  (match name2
         ['() (apply (eval name1 ns) nodes)]
         ;; Prim case
         [_ (apply (eval name1 ns) (cons name2 nodes))])))
        

(define (acc-operands tokens acc arity op)
  (match tokens
     ['() (cond
                ;; set the tail for later use
                [(zero? arity) (set! expr-tail '()) (reverse acc)]
                [else (cm-error error-id (format "Operand(s) missing for ~a." op))])]
     [(cons h t) #:when (zero? arity)
            (set! expr-tail (cons h t)) (reverse acc)]
     [(cons h t) #:when (is-operator? h)
                 (let ([res (parse-op h t)])
            (acc-operands expr-tail (cons res acc) (sub1 arity) op))]
     [(cons h t)
            (acc-operands t (cons (parse-value h) acc) (sub1 arity) op)]))

(define (parse-value token)
  (cond 
        [(is-int-token? token) (Int (string->number token))]
        [(is-float-token? token) (Float (string->number token))]
        [(is-string-token? token) (String (substring token 1 (sub1 (string-length token))))]
        [(string=? "null" token) (Null)]
        [(string=? "true" token) (Bool 1)]
        [(string=? "false" token) (Bool 0)]
        [(string=? token "end") (Prim0 'end)]
        [(string=? token "void") (Prim0 'void)]
        [(string=? token "eof") (Prim0 'eof)]
        [(string=? token "system_type") (String (symbol->string (system-type)))]
        [(string=? token "read_line") (Prim0 'read_line)]
        [(is-var-token? token) (Var token)]
        [(is-operator? token)  (cm-error error-id (format "Operand(s) missing for ~a." token))]
        [else (cm-error error-id (format "Invalid variable name: ~a." token))]))
