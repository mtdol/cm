#lang racket
(provide tokenize-file tokenize-file-abs tokenize-string)
(define error-id "LEX")

(require cm/core/error cm/core/reserved-keywords
  parser-tools/lex
    (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR STR))

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
   ["!lang" "!lang"]
   ["." "dot"]
   ["`" "head"]
   ["~" "tail"]
   ["," "cons"]
   [";" (list "cons" "null")]
   ["()" "null"]
   ["[]" "null"]
   ["{}" "null"]
   ["[" "("]
   ["]" ")"]
   ["{" "("]
   ["}" ")"]
   ["=" "equal"]
   ["!=" "not_equal"]
   ["neq" "not_equal"]
   ["equals" "equal"]
   ["eq" "equal"]
   ["==" "eqq"]
   ["!==" "neqq"]
   ["<" "lt"]
   [">" "gt"]
   [">=" "ge"]
   ["<=" "le"]
   ["+" "plus"]
   ["-" "minus"]
   ["*" "star"]
   ["/" "slash"]
   ["^" "caret"]
   ["%" "mod"]
   ["&" "and"]
   ["||" "or"]
   ["|" "case"]
   ["!" "not"]
   ["$" "cat"]
   ["@" "print"]
   ["lam" "lambda"]
   [":=" "assign"]
   [":" "apply"]
   [":>" "apply1"]
   ["::" "index2"]
   ["->" "yields"]
   [(:+ digit) lexeme]
   [(:: (:+ digit) #\. (:+ digit)) lexeme]
   ;; TODO: allow escape sequence \" in string
   [(:: #\" (:* (:or (:: "\\\"") (:~ #\"))) #\") lexeme]
   [(:: #\" (:* (:~ #\"))) (match start-pos [(position colnum linenum _)
                        (cm-error-linenum linenum error-id 
                                (format "Non-terminated string around column ~a" colnum))])]
   ;[(:: #\" (:* string-char) #\") lexeme]
   ;; everything else (vars and operators)
   [(:+ (:& (:+ any-char) (:~ key-token) (:~ whitespace) (:~ #\"))) lexeme]
   ;; custom error behavior
   [any-char (match start-pos [(position colnum linenum _)
                        (cm-error-linenum linenum error-id 
                                (format "Lexing failure around column ~a" colnum))])]
   ))


(define (tokenize-input ip) 
  (port-count-lines! ip)
    (flatten (port->list cmlex ip)))


;; path relative to current location
(define (tokenize-file name) (tokenize-input (open-input-file name)))
;; absolute file
(define (tokenize-file-abs name) (tokenize-input (open-input-file (expand-user-path (build-path name)))))
(define (tokenize-string str) (tokenize-input (open-input-string str)))

