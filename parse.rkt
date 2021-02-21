#lang racket
(provide parse-expr)
(require "lex.rkt" "ast.rkt")

;; Matthew Dolinka
;; cm parser

;; Auxiliaries

;; given the character after a left paren, returns the tail of the list featuring
;; the matching paren
(define (get-matching-paren lst) (get-matching-paren-aux lst 0))
(define (get-matching-paren-aux lst pcount)
    (match lst
            ['() #f] ;; no matching paren
            [(cons "(" t) (get-matching-paren-aux t (add1 pcount))]
            [(cons ")" t) #:when (zero? pcount) t]
            [(cons ")" t) (get-matching-paren-aux t (sub1 pcount))]
            [(cons h t) (get-matching-paren-aux t pcount)]))

;; returns list if parens are balanced, else throws exception
(define (check-balanced-parens lst) (if (balanced-parens? lst) lst (error "Missing Paren(s).")))
;; determines that every paren (0 or more) in an expr has a matching friend
(define (balanced-parens? lst) (balanced-parens-aux? lst 0))
(define (balanced-parens-aux? lst pcount)
    (match lst
            ['() (zero? pcount)] ;; if pcount is zero, then parens are balanced
            [(cons "(" t) (balanced-parens-aux? t (add1 pcount))]
            [(cons ")" t) (balanced-parens-aux? t (sub1 pcount))]
            [(cons h t) (balanced-parens-aux? t pcount)]))




(define (is-keyword? v)
  (set-member? reserved-keywords v))
(define (is-operator? v)
  (set-member? operators v))
(define (is-string? v) 
         (and (string? v) (string=? (substring v 0 1) "\"")))
(define (is-int? v)
  (and (string->number v) (exact-integer? (string->number v))))
(define (is-float? v)
  (and (string->number v) (flonum? (string->number v))))
(define (is-variable? v) 
  (and (not (is-keyword? v)) (regexp-match? #rx"^[a-zA-Z_][a-zA-Z1-9_]*[?]*[']*$" v)))
;; is the given token a fundamental value?
(define (is-value? v)
      (or (is-variable? v)
          (is-int? v)
          (is-float? v)
          (is-string? v)))


;; converts lam x,y = (x+y) into lam x = lam y = (x + y)
(define (replace-assign-commas lst)
  (match lst
        ['() '()]
        [(cons "lambda" (cons h1 (cons "cons" t)))
            (cons "lambda" (cons h1 (cons ":lambda_assign"
                (replace-assign-commas (cons "lambda" t)))))]
        [(cons "def" (cons h1 (cons "cons" t)))
            (cons "def" (cons h1 (cons ":def_assign"
                (replace-assign-commas (cons "def" t)))))]
        [(cons h t) (cons h (replace-assign-commas t))]))

;; walks down the list, conses, and returns the list back with the first
;; instance of target changed to replacement
(define (replace-first lst target replacement)
  (match lst
         ['() '()]
         [(cons h t) #:when (string=? h target)
                (cons replacement t)]
         [(cons h t) (cons h (replace-first t target replacement))]))

;; converts def x equal 2 into def x :assign 2
(define (replace-assign-equal lst)
  (match lst
        ['() '()]
        [(cons "def" (cons h1 (cons "equal" t)))
            (cons "def" (cons h1 (cons ":def_assign"
                (replace-assign-equal t))))]
        [(cons "let" (cons h1 (cons "equal" t)))
            (cons "let" (cons h1 (cons ":let_assign"
                (replace-assign-equal t))))]
        [(cons "values" t) 
            (cons "values" 
                (replace-assign-equal (replace-first t "equal" ":values_assign")))]
        [(cons "lambda" (cons h1 (cons "equal" t)))
            (cons "lambda" (cons h1 (cons ":lambda_assign"
                (replace-assign-equal t))))]
        [(cons h t) (cons h (replace-assign-equal t))]))

;; inserts a right paren as late as possible  
(define (insert-late-paren lst) (insert-late-paren-aux lst 0))
(define (insert-late-paren-aux lst pcount)
    (match lst 
           ['() (cons ")" '())]
           [(cons "(" t) (cons "(" (insert-late-paren-aux t (add1 pcount)))] 
           [(cons ")" t) #:when (not (zero? pcount)) (cons ")" (insert-late-paren-aux t (sub1 pcount)))] 
           [(cons ")" t) (cons ")" (cons ")" t))] 
           [(cons h t) (cons h (insert-late-paren-aux t pcount))]))

;; replaces some op components like then and else with parenthesis
(define (replace-op-components tokens) 
  (replace-op-components2 (replace-op-components1 tokens)))
(define (replace-op-components1 tokens)
  (match tokens
         ['() '()]
         [(cons "if" t) (cons "if" (cons "(" (replace-op-components1 t)))]
         [(cons "match" t) (cons "match" (cons "(" (replace-op-components1 t)))]
         [(cons "index" t) (cons "index" (cons "(" (replace-op-components1 t)))]
         [(cons "format" t) (cons "format" (cons "(" (replace-op-components1 t)))]
         [(cons "values" t) (cons "values" (cons "(" (replace-op-components1 t)))]
         [(cons "then" t)
            (cons ")" (cons "(" (replace-op-components1 t)))]
         [(cons "else" t)
            (cons ")" (cons "else" (replace-op-components1 t)))]
         [(cons "in" t)
            (cons ")" (cons "in" (replace-op-components1 t)))]
         [(cons "with" t)
            (cons ")" (cons "with" (replace-op-components1 t)))]
         [(cons ":let_assign" t)
            (cons ":let_assign" (cons "(" (replace-op-components1 t)))]
         [(cons ":values_assign" t)
            (cons ")" (cons "(" (replace-op-components1 t)))]
         [(cons h t) (cons h (replace-op-components1 t))]))

(define (replace-op-components2 tokens)
  (match tokens
         ['() '()]
         [(cons "else" t)
            (cons "(" (replace-op-components2 (insert-late-paren t)))]
         [(cons "in" t)
            (cons "(" (replace-op-components2 (insert-late-paren t)))]
         [(cons "with" t)
            (cons "(" (replace-op-components2 (insert-late-paren t)))]
         [(cons ":def_assign" t)
            (cons ":def_assign" (cons "(" (replace-op-components2 (insert-late-paren t))))]
         [(cons ":lambda_assign" t)
            (cons ":lambda_assign" (cons "(" (replace-op-components2 (insert-late-paren t))))]
         [(cons "print" t)
            (cons "print" (cons "(" (replace-op-components2 (insert-late-paren t))))]
         [(cons "eval" t)
            (cons "eval" (cons "(" (replace-op-components2 (insert-late-paren t))))]
         [(cons "error" t)
            (cons "error" (cons "(" (replace-op-components2 (insert-late-paren t))))]
         [(cons h t) (cons h (replace-op-components2 t))]))

;; acumulates the next expression in the token list and returns the accumlator
;; and the tail (catch with let-values)
(define (accumulate-expression tokens) (accumulate-expression-aux tokens 0))
(define (accumulate-expression-aux tokens pcount)
  (match tokens
         ['() (error "An expected expression was missing.")]
         [(cons "(" t) (let-values ;; lp case
                ([(res tail) (accumulate-expression-aux t (add1 pcount))])
                    (values (cons "(" res) tail))]
         [(cons ")" t) #:when (= pcount 1) ;; rp end case
                    (values (list ")") t)]
         [(cons ")" t) (let-values ;; general rp case
                ([(res tail) (accumulate-expression-aux t (sub1 pcount))])
                    (values (cons ")" res) tail))]
         [(cons h t) #:when (and (zero? pcount) (not (is-value? h))) ;; probably another operator
                     (error "Attempted to pass a non-value as a Value.")]
         [(cons h t) #:when (zero? pcount) (values (list h) t)] ;; end case
         [(cons h t) (let-values ;; general case
                ([(res tail) (accumulate-expression-aux t pcount)])
                    (values (cons h res) tail))]))


;; accumulates exactly num expressions and returns list values, with the tail at the end
;; (accumulate-tokens '("(" "a" ")" "b" "(" "(" "c" ")" "d ")" "e" "(" "f" ")") 3) 
;; will return: '("(" "a" ")"), '("b"), '("(" "(" "c" ")" "d" ")"), '("e" "(" "f" ")")
;; so when num = 3 then acc tokens returns a list with num + 1 elements, where
;; the last element is the tail
(define (accumulate-num-expressions tokens num)
  (apply values (reverse (let aux ([res '()][tokens tokens][num num]) {
    if (zero? num) (cons tokens res) 
      (let-values ([(res2 tail) (accumulate-expression tokens)]) {
            aux (cons res2 res) tail (sub1 num)})}))))
    
;; begin parsing code

(define (parse-expr tokens)
    (parse-expr-aux
        (replace-op-components
          (replace-assign-equal 
            (replace-assign-commas
                (check-balanced-parens tokens))))))
   
;; removes extra parens from the given expression
;; ie: ((3 + 1)) -> 3 + 1
(define (unwrap-expr tokens)
  (match tokens
         [(cons "(" t) #:when 
                  (and (string=? (last tokens) ")")
                      (null? (get-matching-paren t)))
            (unwrap-expr (drop-right t 1))]
         [_ tokens]))

(define (parse-expr-aux tokens) 
        (parse-apply (unwrap-expr tokens) '() 0))

(define (parse-apply tokens acc pcount)
    (match tokens
           ;; when we have found the operator, everything to our left is the left
           ;; operand, everything to our right is the right one

           ;; parencount tells us whether we are currently in a subexpression
           ;; we can only form ast when not in a subexpression
           [(cons "apply" t) #:when (zero? pcount) (Prim1 'apply (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-apply t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-apply t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-cons (reverse acc) '() 0)]
           [(cons h t) (parse-apply t (cons h acc) pcount)]
           [_ (error "Apply parse error.")]))

(define (parse-cons tokens acc pcount)
    (match tokens
           [(cons "cons" t) #:when (zero? pcount) (Prim1 'cons (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-cons t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-cons t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-andor (reverse acc) '() 0)]
           [(cons h t) (parse-cons t (cons h acc) pcount)]
           [_ (error "Cons parse error.")]))

(define (parse-andor tokens acc pcount)
    (match tokens
           [(cons "and" t) #:when (zero? pcount) (Prim1 'and (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "or" t) #:when (zero? pcount) (Prim1 'or (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "cat" t) #:when (zero? pcount) (Prim1 'cat (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-andor t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-andor t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-comp (reverse acc) '() 0)]
           [(cons h t) (parse-andor t (cons h acc) pcount)]
           [_ (error "Andor parse error.")]))

(define (parse-comp tokens acc pcount)
    (match tokens
           [(cons "gt" t) #:when (zero? pcount) (Prim1 'gt (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "lt" t) #:when (zero? pcount) (Prim1 'lt (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "equal" t) #:when (zero? pcount) (Prim1 'equal (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "ge" t) #:when (zero? pcount) (Prim1 'ge (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "le" t) #:when (zero? pcount) (Prim1 'le (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-comp t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-comp t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-addsub (reverse acc) '() 0)]
           [(cons h t) (parse-comp t (cons h acc) pcount)]
           [_ (error "Comp parse error.")]))

(define (parse-addsub tokens acc pcount)
    (match tokens
           [(cons h t) ;; unary case
             #:when (and (zero? pcount) 
                         (or (string=? h "plus") (string=? h "minus"))
                         (or (null? acc)
                             (is-operator? (car acc))))
                    (parse-addsub t (cons h acc) pcount)] 
           [(cons "plus" t) #:when (zero? pcount)
                            (Prim1 'add (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "minus" t) #:when (zero? pcount)
                             (Prim1 'sub (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-addsub t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-addsub t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-multdiv (reverse acc) '() 0)]
           [(cons h t) (parse-addsub t (cons h acc) pcount)]
           [_ (error "Addsub parse error.")]))

(define (parse-multdiv tokens acc pcount)
    (match tokens
           [(cons "mult" t) #:when (zero? pcount) (Prim1 'mult (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "div" t) #:when (zero? pcount) (Prim1 'div (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "mod" t) #:when (zero? pcount) (Prim1 'mod (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-multdiv t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-multdiv t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-exp (reverse acc) '() 0)]
           [(cons h t) (parse-multdiv t (cons h acc) pcount)]
           [_ (error "Multdiv parse error.")]))

(define (parse-exp tokens acc pcount)
    (match tokens
           [(cons "exp" t) #:when (zero? pcount) (Prim1 'exp (parse-expr-aux (reverse acc)) (parse-expr-aux t))]
           [(cons "(" t) (parse-exp t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-exp t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-bottom (reverse acc) '() 0)]
           [(cons h t) (parse-exp t (cons h acc) pcount)]
           [_ (error "Exp parse error.")]))

(define bottom-ops (set
        "match" "index" "length" "format"
        "let" "values" "if" "def" "lambda" "print" "eval" "string" "float" "int"
        "bool" "int?" "string?" "float?" "bool?" "list?" "pair?" "null?"
        "error" "type" "not" "tail" "head" "plus" "minus"))

(define (parse-bottom tokens acc pcount)
    (match tokens
           [(cons h t) #:when (and (set-member? bottom-ops h) (not (null? acc)))
                       (error "Error: Token(s) placed before expression.")]
           [(cons "minus" t) #:when (zero? pcount) (Prim2 'neg (parse-expr-aux t))]
           [(cons "plus" t) #:when (zero? pcount) (Prim2 'pos (parse-expr-aux t))]
           [(cons "head" t) #:when (zero? pcount) (Prim2 'head (parse-expr-aux t))]
           [(cons "tail" t) #:when (zero? pcount) (Prim2 'tail (parse-expr-aux t))]
           [(cons "not" t) #:when (zero? pcount) (Prim2 'not (parse-expr-aux t))]
           [(cons "type" t) #:when (zero? pcount) (Prim2 'type (parse-expr-aux t))]
           [(cons "int" t) #:when (zero? pcount) (Prim2 'int (parse-expr-aux t))]
           [(cons "float" t) #:when (zero? pcount) (Prim2 'float (parse-expr-aux t))]
           [(cons "string" t) #:when (zero? pcount) (Prim2 'string (parse-expr-aux t))]
           [(cons "bool" t) #:when (zero? pcount) (Prim2 'bool (parse-expr-aux t))]
           [(cons "int?" t) #:when (zero? pcount) (Prim2 'int (parse-expr-aux t))]
           [(cons "float?" t) #:when (zero? pcount) (Prim2 'float (parse-expr-aux t))]
           [(cons "string?" t) #:when (zero? pcount) (Prim2 'string (parse-expr-aux t))]
           [(cons "bool?" t) #:when (zero? pcount) (Prim2 'bool (parse-expr-aux t))]
           [(cons "list?" t) #:when (zero? pcount) (Prim2 'bool (parse-expr-aux t))]
           [(cons "pair?" t) #:when (zero? pcount) (Prim2 'bool (parse-expr-aux t))]
           [(cons "null?" t) #:when (zero? pcount) (Prim2 'bool (parse-expr-aux t))]
           [(cons "length" t) #:when (zero? pcount) (Prim2 'length (parse-expr-aux t))]
           [(cons "print" t) #:when (zero? pcount) (Print (parse-expr-aux t))]
           [(cons "lambda" (cons h (cons ":lambda_assign" t))) #:when (zero? pcount)
                    (Lambda (parse-expr-aux (list h)) (parse-expr-aux t))]
           [(cons "lambda" t) #:when (zero? pcount) 
                (error "Improperly formed lambda expression.")]
           [(cons "def" (cons h (cons ":def_assign" t))) #:when (zero? pcount)
                    (Def (parse-expr-aux (list h)) (parse-expr-aux t))]
           [(cons "def" t) #:when (zero? pcount) 
                (error "Improperly formed def expression.")]
           [(cons "if" t) #:when (zero? pcount) 
                    (let-values 
                      ([(e1 e2 e3) (accumulate-num-expressions t 2)])
                        {If (parse-expr-aux e1)
                            (parse-expr-aux e2) (parse-expr-aux e3)})]
           [(cons "let" (cons h (cons ":let_assign" t))) #:when (zero? pcount) 
                    (let-values 
                      ([(e1 e2) (accumulate-num-expressions t 1)])
                        {Let (parse-expr-aux (list h))
                            (parse-expr-aux e1) (parse-expr-aux e2)})]
           [(cons "let" t) #:when (zero? pcount) 
                (error "Improperly formed let expression.")]
           [(cons "values" t) #:when (zero? pcount) 
                    (let-values 
                      ([(e1 e2 e3) (accumulate-num-expressions t 2)])
                        {Values (parse-expr-aux e1)
                            (parse-expr-aux e2) (parse-expr-aux e3)})]
           [(cons "format" t) #:when (zero? pcount) 
                    (let-values 
                      ([(e1 e2) (accumulate-num-expressions t 1)])
                        {Format (parse-expr-aux e1) (parse-expr-aux e2)})]
           [(cons "match" t) #:when (zero? pcount) 
                    (let-values 
                      ([(e1 e2) (accumulate-num-expressions t 1)])
                        {Match (parse-expr-aux e1) (parse-expr-aux e2)})]
           [(cons "index" t) #:when (zero? pcount) 
                    (let-values 
                      ([(e1 e2) (accumulate-num-expressions t 1)])
                        {Index (parse-expr-aux e1) (parse-expr-aux e2)})]
           [(cons "eval" t) #:when (zero? pcount) (Eval (parse-expr-aux t))]
           [(cons "error" t) #:when (zero? pcount) (Error (parse-expr-aux t))]
           [(cons "(" t) (parse-bottom t (cons "(" acc) (add1 pcount))] 
           [(cons ")" t) (parse-bottom t (cons ")" acc) (sub1 pcount))] 
           ['() (parse-fundamental (reverse acc))]
           [(cons h t) (parse-bottom t (cons h acc) pcount)]
           [_ (error "Bottom level parse error.")]))


(define (parse-fundamental tokens)
    (match tokens
           [(cons h t) #:when (not (null? t)) (error "Invalid Syntax (v v)")]
           [(cons "null" t) (Null)]
           [(cons h t) #:when (is-int? h) (Int (string->number h))]
           [(cons h t) #:when (is-float? h) (Float (string->number h))]
           [(cons h t) #:when (is-string? h) (String (substring h 1 (sub1 (string-length h))))]
           [(cons h t) #:when (is-variable? h) (Var h)]
           [_ (error "Fundamental parse error; values missing?")]))
