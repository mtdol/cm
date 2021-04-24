#lang racket
(require cm/core/error cm/core/operators cm/core/parse-utils
         cm/core/context cm/core/macros 
         cm/core/tokens)
(provide pre-parse-expr pre-parse)

;; Matthew Dolinka
;;

;; this file is for performing adjustments to the token list, so it
;; will be parsable by the main parser

;; prepares the main token list for parsing
(define (pre-parse tokens module-id) 
  (set-current-linenum! 1)
  (parse-macro-tokens tokens module-id 0))

(define (parse-macro-tokens tokens module-id depth)
  (match tokens
         [_ #:when (> depth 5000) 
            (cm-error-linenum 
              module-id
              (get-current-linenum) 
              "MACRO" 
              "Too many macro applications. Possible infinite recursion detected.")]
         ['() '()]
         [(cons (? (tok=? "{") h1) (cons (? (tok=? "}")) t)) 
          (set-check-current-linenum! (line h1))
          (cm-error-linenum 
            module-id
            (get-current-linenum) "MACRO" "Missing label for macro.")]
         [(cons (? (tok=? "{") h1) (cons h2 t)) 
          (set-check-current-linenum! (line h1))
          (parse-macro-contents t h2 '(()) module-id 0 depth)]
         [(cons h t) (cons h (parse-macro-tokens t module-id depth))]
   ))

;; once inside a macro body, parses its contents into an arguments list
;;
;; token list, string, token list list, string, int, int -> token list
(define (parse-macro-contents tokens label args module-id bcount depth)
  (match tokens 
         ['() (cm-error-linenum 
                module-id
                (get-current-linenum)
                "MACRO" "Missing closing brace for macro application.")]
         ;; shifting to next argument
         [(cons (? (tok=? "case")) t) #:when (zero? bcount) (parse-macro-contents t label 
                            (cons '() (cons (reverse (car args)) (cdr args)))
                            module-id bcount depth)]
         [(cons (? (tok=? "}")) t)
          #:when (= 0 bcount)
          (let 
            ([args 
                 (if (equal? args '(()))
                   '(())
                     ;; eager evaluate args
                     (foldl (lambda (arg acc) 
                              (cons 
                                (parse-macro-tokens arg module-id (add1 depth))
                                acc))
                            '()
                            (cons (reverse (car args)) (cdr args))))])
          (let-values 
            ([(res/tokens res/module-id)
              (apply-macro label args module-id)])
                ;; re-parse after we apply the macro
              (append (parse-macro-tokens res/tokens res/module-id (add1 depth))
                      (parse-macro-tokens t module-id depth))))]
         [(cons (? (tok=? "}") h1) t) (parse-macro-contents t label 
                            (cons (cons h1 (car args)) (cdr args))
                            module-id (sub1 bcount) depth)]
         [(cons (? (tok=? "{") h1) t) (parse-macro-contents t label 
                            (cons (cons h1 (car args)) (cdr args))
                            module-id (add1 bcount) depth)]
         [(cons h t) (parse-macro-contents t label 
                            (cons (cons h (car args)) (cdr args))
                            module-id bcount depth)]))

(define current-module-id "0")

;; prepares tokens for parsing of an expr
(define (pre-parse-expr tokens module-id)
  (set! current-module-id module-id)
  (replace-unary-plus-minus
    (check-balanced-parens 
        tokens module-id)))

;; replaces unary minus with :uni_minus and unary plus with :uni_plus
;; additionally, throws an exception when a infix operator is treated as
;; a prefix operator
(define (replace-unary-plus-minus tokens)
  (let aux ([tokens tokens] [first? #t])
    (match tokens
           [(cons h t) #:when 
                    (and first? (or (string=? "plus" (tok h))
                                    (string=? "minus" (tok h))))
                (aux (cons (make-token (string-append ":uni_" (tok h)) (line h)) t) #f)]
           ;; infix op being treated as prefix is error
           [(cons h t) #:when 
                    (and first? (and (is-operator? (tok h))
                                     (string=? (op-to-position (tok h)) "infix")))
                (cm-error-linenum 
                  current-module-id
                  (get-current-linenum) 
                  "SYNTAX" (format "Missing operand(s) for ~a." (tok h)))]
           [(cons h1 (cons h2 t)) #:when 
                      (and (or (string=? "(" (tok h1)) 
                               (and (is-operator? (tok h1)) 
                                    (not (zero? (op-to-arity (tok h1)))))) 
                           (and (is-operator? (tok h2)) (string=? (op-to-position (tok h2)) "infix")))
                    (cond [(or (string=? "plus" (tok h2)) (string=? "minus" (tok h2)))
                        ;; call aux on uni since it is also an operator
                        (cons h1 (aux (cons (make-token (string-append ":uni_" (tok h2)) (line h2)) t) #f))]
                          [else (cm-error-linenum 
                                  current-module-id
                                  (get-current-linenum) 
                                  "SYNTAX" (format "Missing operand(s) for ~a." (tok h1)))])]
           [(cons h t) (cons h (aux t #f))]
           ['() '()])))
