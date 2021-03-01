#lang racket
(provide tokenize-file tokenize-string)
(define error-id 1)

(require cm/core/error cm/core/reserved-keywords
  parser-tools/lex
    (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR STR))

;;#rx"^[a-zA-Z_][a-zA-Z1-9_]*[?]*[']*$"
(define-lex-abbrevs
  [digit (:/ "0" "9")]

  [lower-letter (:/ "a" "z")]

  [upper-letter (:/ #\A #\Z)]

  [letter (:or lower-letter upper-letter)]

  [alphanumeral (:or letter digit)]

  ;; operators that can be placed after digits or variables
  ;; ie. in "3+2", + would count
  [key-token (:or "`" "~" "," ";" "=" ">" "<" "+" "-"
                   "*" "/" "^" "%" "&" "|" "!" "@"
            "(" ")" "[" "]" "{" "}" ":" "$" "#" ".")]
  ;[keyword (:or (set->list reserved-keywords))]

  [string-char (:~ #\")]

  ;[comment (:: "#" (:* (:~ #\newline)) #\newline)]
  [comment (:: "#" (:* (:~ #\newline)))]
  )


(define cmlex
  (lexer
   [(eof) eof]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space) (cmlex input-port)]
   [#\newline ":newline"]
   ;; skip comment lines
   [comment (cmlex input-port)]
   [(:or "(" ")" "#") lexeme]
   ["." "dot"]
   ["`" "head"]
   ["~" "tail"]
   ["," "cons"]
   [";" (list "cons" "null")]
   ["()" "null"]
   ["[" "("]
   ["]" ")"]
   ["{" "("]
   ["}" ")"]
   ["=" "equal"]
   ["equals" "equal"]
   ["<" "lt"]
   [">" "gt"]
   [">=" "ge"]
   ["<=" "le"]
   ["+" "plus"]
   ["-" "minus"]
   ["*" "mult"]
   ["/" "div"]
   ["^" "exp"]
   ["%" "mod"]
   ["&" "and"]
   ["|" "or"]
   ["!" "not"]
   ["true" "1"]
   ["false" "0"]
   ["$" "cat"]
   ["@" "print"]
   ["lam" "lambda"]
   [":" "apply"]
   [(:+ digit) lexeme]
   [(:: (:+ digit) #\. (:+ digit)) lexeme]
   ;; TODO: allow escape sequence \" in string
   [(:: #\" (:* string-char) #\") lexeme]
   ;; everything else (vars and operators)
   [(:+ (:& (:+ any-char) (:~ key-token) (:~ whitespace))) lexeme]
   ;; custom error behavior
   [any-char (match start-pos [(position linenum colnum _)
                        (cm-error-linenum linenum error-id 
                                (format "Lexing failure around column ~a" colnum))])]
   ))


;; calls the lexer with input port until 'EOF is reached 
;(define (lex ip)
  ;(port-count-lines! ip)
  ;(let one-line ()
    ;(define result
      ;((lambda () (cmlex ip))))
    ;(unless (and (symbol? result) (string=? "EOF" (symbol->string result)))
      ;(printf "~a\n" result)
      ;(one-line))))

(define (tokenize-input ip) 
  (port-count-lines! ip)
    (flatten (port->list cmlex ip)))


(define (tokenize-file name) (tokenize-input (open-input-file name)))
(define (tokenize-string str) (tokenize-input (open-input-string str)))

