#lang racket
(require cm/core/error cm/core/operators cm/core/parse-utils cm/core/context)
(provide pre-parse-expr pre-parse)
(define error-id "PARSE")

;; this file is for performing adjustments to the token list, so it
;; will be parsable by the main parser

;; prepares the main token list for parsing
(define (pre-parse tokens) 
  (parse-macro-tokens tokens 0))

(define (parse-macro-tokens tokens depth)
  (match tokens
         [_ #:when (> depth 5000) 
            (cm-error error-id "Too many macro applications. Possible infinite recursion detected.")]
         ['() '()]
         [(cons "{" (cons h2 t)) 
          (if (string=? h2 "}") 
            (cm-error error-id "Missing label for macro.")
            (parse-macro-contents t h2 '(()) 0 depth))]
         [(cons h t) (cons h (parse-macro-tokens t depth))]
   ))

;; once inside a macro body, parses its contents into an arguments list
;;
;; string list, string, string list list, int, int -> string list
(define (parse-macro-contents tokens label args bcount depth)
  (match tokens 
         ['() (cm-error error-id "Missing closing brace for macro application.")]
         ;; shifting to next argument
         [(cons "case" t) #:when (zero? bcount) (parse-macro-contents t label 
                            (cons '() (cons (reverse (car args)) (cdr args)))
                            bcount depth)]
         [(cons "}" t) 
          #:when (= 0 bcount)
          ;; DEBUG
          ;(displayln (reverse (cons (reverse (car args)) (cdr args))))
          ;;DEBUG
          ;(displayln (apply-macro label 
                ;(reverse (cons (reverse (car args)) (cdr args)))))
          (append 
            ;; re-parse after we apply the macro
            (parse-macro-tokens 
              (apply-macro label 
                (reverse (cons (reverse (car args)) (cdr args))))
              (add1 depth))
            (parse-macro-tokens t depth))]
         [(cons "}" t) (parse-macro-contents t label 
                            (cons (cons "}" (car args)) (cdr args)) (sub1 bcount) depth)]
         [(cons "{" t) (parse-macro-contents t label 
                            (cons (cons "{" (car args)) (cdr args)) (add1 bcount) depth)]
         [(cons h t) (parse-macro-contents t label 
                            (cons (cons h (car args)) (cdr args)) bcount depth)]))

;; prepares tokens for parsing of an expr
(define (pre-parse-expr tokens)
  (replace-unary-plus-minus
  (check-balanced-parens 
        tokens)))

;; replaces unary minus with :uni_minus and unary plus with :uni_plus
;; additionally, throws an exception when a infix operator is treated as
;; a prefix operator
(define (replace-unary-plus-minus tokens)
  (let aux ([tokens tokens] [first? #t])
    (match tokens
           [(cons h t) #:when 
                    (and first? (or (string=? "plus" h) (string=? "minus" h)))
                (aux (cons (string-append ":uni_" h) t) #f)]
           ;; infix op being treated as prefix is error
           [(cons h t) #:when 
                    (and first? (and (is-operator? h)
                                     (string=? (op-to-position h) "infix")))
                (cm-error error-id (format "Missing operand(s) for ~a." h))]
           [(cons h1 (cons h2 t)) #:when 
                      (and (or (string=? "(" h1) 
                               (and (is-operator? h1) (not (zero? (op-to-arity h1))))) 
                           (and (is-operator? h2) (string=? (op-to-position h2) "infix")))
                    (cond [(or (string=? "plus" h2) (string=? "minus" h2))
                        ;; call aux on uni since it is also an operator
                        (cons h1 (aux (cons (string-append ":uni_" h2) t) #f))]
                          [else (cm-error error-id (format "Missing operand(s) for ~a." h1))])]
           [(cons h t) (cons h (aux t #f))]
           ['() '()])))
