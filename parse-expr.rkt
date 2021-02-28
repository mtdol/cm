#lang racket
(require "error.rkt" "types.rkt" "ast.rkt"
         "operators.rkt" "parse-auxiliaries.rkt"
         "pre-parse.rkt")
(provide (all-defined-out))
(define error-id 2)

;; Matthew Dolinka
;; cm parser


(define (parse-expr tokens) 
  ;(parse-to-ast (tokens-to-prefix-form (pre-parse-expr tokens))))
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
        ;; probably something like (1 2), which is invalid
        ;; we should never get here
        ['() #:when (= preced max-precedences) 
            (cm-error error-id "Could not find operator.")]
        ;; try again since we didn't find an op of current precedence
        ['() (tokens-to-prefix-form-aux (reverse acc) '() (add1 preced) 0)]
        ;; increment pcount
        [(cons "(" t) (tokens-to-prefix-form-aux t (cons "(" acc) preced (add1 pcount))]
        ;; decrement pcount
        [(cons ")" t) (tokens-to-prefix-form-aux t (cons ")" acc) preced (sub1 pcount))]
        ;; infix op found matching precedence
        ;; reverse acc because it is wrong order
        [(cons h t) #:when (and (zero? pcount) (= (op-to-precedence h) preced)
                                (string=? (op-to-position h) "prefix"))
            (append (tokens-to-prefix-form (reverse acc)) (list h) (tokens-to-prefix-form t))]
        ;; prefix op found matching precedence
        [(cons h t) #:when (and (zero? pcount) (= (op-to-precedence h) preced)
                                (string=? (op-to-position h) "infix"))
            (append (list h) (tokens-to-prefix-form (reverse acc))
                          (tokens-to-prefix-form t))]
        ;; general case
        [(cons h t) (tokens-to-prefix-form-aux t (cons h acc) preced pcount)]))


;; Takes in a list of tokes in prefix order and produces an ast.
;; Does not accept expressions with parenthesis.
(define (parse-to-ast tokens)
  (match tokens
         ['() (Noop)]
         [(cons h '()) (parse-value h)]
         [(cons h t) #:when (is-operator? h) (parse-op t)]
         [_ (cm-error error-id "Invalid Expression. Probably missing an operator.")]))

(define (parse-op tokens) tokens)

(define (acc-operands tokens acc arity)
  (match tokens
     ['() (cond
                [(zero? arity) (reverse acc)]
                [else (cm-error error-id "Operands missing.")])]
     [(cons h t) #:when (is-operator? h)
            (acc-operands t (cons (parse-op h) acc) (sub1 arity))]
     [(cons h t)
            (acc-operands t (cons (parse-value h) acc) (sub1 arity))]))

(define (parse-value token)
  (cond 
        [(is-int-token? token) (Int (string->number token))]
        [(is-float-token? token) (Float (string->number token))]
        [(is-string-token? token) (String (substring token 1 (sub1 (string-length token))))]
        [(string=? "null" token) (Null)]
        [(is-var-token? token) (Var token)]
        [else (cm-error error-id (format "Invalid variable name: ~a" token))]))
