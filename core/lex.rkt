#lang racket
(require cm/core/error)
(provide tokenize tokenize-from-file)
(define error-id 1)

;; Matthew Dolinka
;; cm lexer

;; chars to be tokenized regardless of context
(define key-tokens 
  (list "`" "~" "," ";" "=" ">" "<" ">=" "<=" "+" "-" "*" "/" "^" "%" "&" "|" "!" "@"
            "(" ")" "[" "]" "{" "}" ":" "$" "#" "\\#"))

;; turns a string into a list of strings by spliting by whitespace,
;; and also performs other necessary adjustments to the token list
(define (tokenize s)
    (remove-comments 
      (process-dot 
        (convert-symbols 
          (token-split s)))))

(define (tokenize-from-file file) (tokenize (file->string file)))

;; splits the given line into tokens, removing empty tokens
;; ensures that 'key chars' are seperated as individual tokes regardless
;; of the context they occur in
(define (token-split line)
 (remove* '("") (regexp-split #rx" |\r|\n" (add-spaces line key-tokens))))

;; converts symbols with multiple names into their parsable form
(define (convert-symbols tokens) 
  (match tokens
         ['() '()]
         [(cons "`" t) (cons "head" (convert-symbols t))]
         [(cons "~" t) (cons "tail" (convert-symbols t))]
         [(cons "," t) (cons "cons" (convert-symbols t))]
         [(cons ";" t) (cons "cons" (cons "null" (convert-symbols t)))]
         [(cons "(" (cons ")" t)) (cons "null" (convert-symbols t))]
         [(cons "[" t) (cons "(" (convert-symbols t))]
         [(cons "]" t) (cons ")" (convert-symbols t))]
         [(cons "{" t) (cons "(" (convert-symbols t))]
         [(cons "}" t) (cons ")" (convert-symbols t))]
         [(cons "=" t) (cons "equal" (convert-symbols t))]
         [(cons "equals" t) (cons "equal" (convert-symbols t))]
         [(cons "<" t) (cons "lt" (convert-symbols t))]
         [(cons ">" t) (cons "gt" (convert-symbols t))]
         [(cons ">=" t) (cons "ge" (convert-symbols t))]
         [(cons "<=" t) (cons "le" (convert-symbols t))]
         [(cons "+" t) (cons "plus" (convert-symbols t))]
         [(cons "-" t) (cons "minus" (convert-symbols t))]
         [(cons "*" t) (cons "mult" (convert-symbols t))]
         [(cons "/" t) (cons "div" (convert-symbols t))]
         [(cons "^" t) (cons "exp" (convert-symbols t))]
         [(cons "%" t) (cons "mod" (convert-symbols t))]
         [(cons "&" t) (cons "and" (convert-symbols t))]
         [(cons "|" t) (cons "or" (convert-symbols t))]
         [(cons "!" t) (cons "not" (convert-symbols t))]
         [(cons "true" t) (cons "1" (convert-symbols t))]
         [(cons "false" t) (cons "0" (convert-symbols t))]
         [(cons "$" t) (cons "cat" (convert-symbols t))]
         [(cons "@" t) (cons "print" (convert-symbols t))]
         [(cons "lam" t) (cons "lambda" (convert-symbols t))]
         [(cons ":" t) (cons "apply" (convert-symbols t))]
         [(cons h t) (cons h (convert-symbols t))]))

;; removes contiguous duplicate "dot"s from the token list in order to
;; remove empty statements (noops) from the source code before parsing
(define (remove-empty-statements tokens)
  (match tokens 
         ['() '()]
         [(cons h '()) (cons h '())]
         [(cons h t) (cons h (remove-empty-statements-aux t h))]))
         
(define (remove-empty-statements-aux tokens prev) 
  (match tokens
        ['() '()]
        [(cons h t)
            #:when (and (string=? prev "dot") (string=? h "dot")) 
                (remove-empty-statements-aux t prev)]
        [(cons h t) (cons h (remove-empty-statements-aux t h))]))


;; turns all instances where "." is the statement terminator into "dot"
;; doesn not do this for other usages of dot like "4.2" which is a float
(define (process-dot tokens)
  (match tokens
        ['() '()]
        [(cons h t) #:when (string=? h ".") (cons "dot" (process-dot t))]
        [(cons h t)
            #:when (regexp-match? #rx".*\\.$" h)
               (cons (list-ref (regexp-match #rx"(.*)\\.$" h) 1) (cons "dot" (process-dot t)))]
        [(cons h t) (cons h (process-dot t))]))

;; removes comments
(define (remove-comments tokens) 
  (match tokens
        ['() '()]
        [(cons "#" t) (remove-comments (remove-comments-aux t))]
        [(cons h t) (cons h (remove-comments t))]))

;; when we found a token
(define (remove-comments-aux tokens)
  (match tokens 
        ['() (cm-error error-id "No termination of comment")]
        [(cons "\\#" t) (remove-comments t)]
        [(cons h t) (remove-comments-aux t)]))

;; places spaces between all key characters given in the list lst
(define (add-spaces s lst) 
  (match lst 
        ['() s] 
        [(cons h t) #:when (string=? h "#")
            (add-spaces
              (regexp-replace*
                (regexp "(?<!\\\\)#") s (string-append " " h " ")) t)]
        [(cons h t) #:when (string=? h "\\#")
            (add-spaces
              (regexp-replace*
                (regexp "\\\\#") s (string-append " " "\\\\#" " ")) t)]
        [(cons h t) #:when (string=? h ">")
            (add-spaces
              (regexp-replace*
                (regexp ">(?!=)") s (string-append " " h " ")) t)]
        [(cons h t) #:when (string=? h "<")
            (add-spaces
              (regexp-replace*
                (regexp "<(?!=)") s (string-append " " h " ")) t)]
        [(cons h t) #:when (string=? h "=")
            (add-spaces
              (regexp-replace*
                (regexp "(?<!>)(?<!<)=") s (string-append " " h " ")) t)]
        [(cons h t)
            (add-spaces
              (regexp-replace*
                (regexp-quote h) s (string-append " " h " ")) t)]))
