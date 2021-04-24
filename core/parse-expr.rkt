#lang racket
(require cm/core/error cm/core/types cm/core/ast
         cm/core/operators cm/core/parse-utils
         cm/core/pre-parse cm/core/context
         cm/core/tokens)
(provide parse-expr half-parse-expr)
(define error-id "SYNTAX")

;; for use in eval of ast nodes
(define ns (module->namespace 'cm/core/ast))


;; Matthew Dolinka
;; cm parser

(define current-module-id "0")

(define (parse-expr tokens module-id)
  (set! current-module-id module-id)
  (parse-to-ast (tokens-to-prefix-form (pre-parse-expr tokens module-id))))

(define (half-parse-expr tokens module-id)
  (set! current-module-id module-id)
  (tokens-to-prefix-form (pre-parse-expr tokens module-id)))

;; all precedences that will be parsed in anti-pemdas order
;; anti-pemdas means parsing from left to right,
;; pemdas is right to left
;(define anti-pemdas (set 1))
(define anti-pemdas (set 0 1 7))

;; (1 + 2 * 3) -> + 1 * 2 3
;; returned expresions are parenthesis free
(define (tokens-to-prefix-form tokens) 
    (match tokens
           ['() '()]
           ;; fundamental value
           [(list h) #:when (not (is-operator? (tok h))) (list h)] 
           ;; unwrap all outer parens from token list, convert to s-expr and
           ;; then rewrap with one layer of parens
           [tokens (tokens-to-prefix-form-aux (unwrap-expr tokens) '() 0 0 
                                              ;; is precedence zero anti-pemdas?
                                              (set-member? anti-pemdas 0))])) 

(define (tokens-to-prefix-form-aux tokens acc preced pcount anti-pemdas?) 
    (match tokens
         ;; list looks like "3 (5 + 8)"
         ;; we must recurse and deal with the subexprs
        ['() #:when (= preced max-precedences) 
         ;; we will collect subexprs and prefixify them while walking down the list
            (let aux ([tokens (if anti-pemdas? (reverse acc) acc)] [acc '()] [pcount 0])
                (match tokens
                       ['() (reverse acc)]
                       [(cons (? (tok=? ")") h1) t) #:when (= 1 pcount)
                            (append (tokens-to-prefix-form (reverse (cons h1 acc))) 
                                    (aux t '() 0))]
                       [(cons (? (tok=? "(") h1) t) #:when (= 0 pcount)
                            (append (reverse acc) 
                                    (aux t (list h1) 1))]
                       [(cons (? (tok=? "(") h1) t) (aux t (cons h1 acc) (add1 pcount))]
                       [(cons (? (tok=? ")") h1) t) (aux t (cons h1 acc) (sub1 pcount))]
                       [(cons h t) (aux t (cons h acc) pcount)]))]
        ;; try again since we didn't find an op of current precedence
        ['() #:when (set-member? anti-pemdas (add1 preced))
         (tokens-to-prefix-form-aux (if anti-pemdas? (reverse acc) acc) '() (add1 preced) 0 #t)]
        ;; next precedence is pemdas order
        ['() 
         (tokens-to-prefix-form-aux (if anti-pemdas? acc (reverse acc)) '() (add1 preced) 0 #f)]
        ;; increment pcount
        [(cons (? (tok=? "(") h1) t) 
         (tokens-to-prefix-form-aux t (cons h1 acc) preced 
           (if anti-pemdas? (add1 pcount) (sub1 pcount)) anti-pemdas?)]
        ;; decrement pcount
        [(cons (? (tok=? ")") h1) t) (tokens-to-prefix-form-aux t (cons h1 acc) preced 
                        (if anti-pemdas? (sub1 pcount) (add1 pcount)) anti-pemdas?)]
        ;; prefix op found matching precedence
        [(cons h t) #:when (and (zero? pcount) (is-operator? (tok h)) (= (op-to-precedence (tok h)) preced)
                                (string=? (op-to-position (tok h)) "prefix"))

            (if anti-pemdas? 
             (append (tokens-to-prefix-form (reverse acc))
                            (list h) (tokens-to-prefix-form t))
             (cm-error-linenum 
               current-module-id (get-current-linenum) error-id "Cannot pemdas parse prefix op.")
             )]
        ;; infix op found matching precedence
        [(cons h t) #:when (and (zero? pcount) (is-operator? (tok h)) (= (op-to-precedence (tok h)) preced)
                                (string=? (op-to-position (tok h)) "infix"))
            (if anti-pemdas? 
             (append (list h) (tokens-to-prefix-form (reverse acc))
                          (tokens-to-prefix-form t))
             (append (list h) (tokens-to-prefix-form (reverse t)) (tokens-to-prefix-form acc)))]
        ;; fundamental value
        [(list h) #:when (and (null? acc) (not (is-operator? (tok h)))) (list h)] 
        ;; general case
        [(cons h t) (tokens-to-prefix-form-aux t (cons h acc) preced pcount anti-pemdas?)]))

;; used in parse-to-ast to check for expressions with too many operands.
(define expr-tail '())

;; Takes in a list of tokens in prefix order and produces an ast.
;; Does not accept expressions with parenthesis.
;;
;; token list -> expr
(define (parse-to-ast tokens)
  (match tokens
         ['() (Prim0 'void)]
         [(cons h '()) (parse-value (tok h))]
         [(cons h t) 
          #:when (is-operator? (tok h)) 
           (match (parse-op (tok h) t) 
                 [ast #:when (not (null? expr-tail))
                    (cm-error-linenum 
                      current-module-id (get-current-linenum) error-id 
                      "Invalid Expression. Probably missing an operator.")]
                 [ast ast])]
         [_ (cm-error-linenum 
              current-module-id
              (get-current-linenum) 
              error-id "Invalid Expression. Probably missing an operator.")]))

;; string, token list -> expr
(define (parse-op op tail)
  (let ([nodes (acc-operands tail '() (op-to-arity op) op)]
        [name1 (op-to-node-name op)]
        [name2 (op-to-extra-node-name op)])
  (match name2
         ['() (apply (eval name1 ns) nodes)]
         ;; Prim case
         [_ (apply (eval name1 ns) (cons name2 nodes))])))
        

;; token list, expr list, integer, string -> expr list
(define (acc-operands tokens acc arity op)
  (match tokens
     ['() (cond
                ;; set the tail for later use
                [(zero? arity) (set! expr-tail '()) (reverse acc)]
                [else (cm-error-linenum 
                        current-module-id
                        (get-current-linenum) 
                        error-id (format "Operand(s) missing for ~a." op))])]
     [(cons h t) #:when (zero? arity)
            (set! expr-tail (cons h t)) (reverse acc)]
     [(cons h t) #:when (is-operator? (tok h))
                 (let ([res (parse-op (tok h) t)])
            (acc-operands expr-tail (cons res acc) (sub1 arity) op))]
     [(cons h t)
            (acc-operands t (cons (parse-value (tok h)) acc) (sub1 arity) op)]))

;; string -> expr
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
        [(string=? token "...") (Prim0 '...)]
        [(is-var-token? token) (Var token)]
        [(is-operator? token)  
         (cm-error-linenum 
           current-module-id
            (get-current-linenum) 
            error-id (format "Operand(s) missing for ~a." token))]
        [else (cm-error-linenum 
                current-module-id
                (get-current-linenum) 
                error-id (format "Invalid variable name: ~a." token))]))
