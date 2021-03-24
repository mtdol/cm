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

  [string-char (:~ #\")]

  [comment (:: "#" (:~ ":") (:* (:~ #\newline)))]
  [macro (:: "#:" (:* (:~ #\newline)))]
  )

(define cm-lang-line (list "load" "\"std_lib::std.cm\"" "dot"))
(define (process-macro name body linenum)
  (match name
         ["lang" 
          (if (string=? body "cm")
            cm-lang-line
            (cm-error-linenum linenum "LEX" "Invalid lang name in macro."))]
         [_ (cm-error-linenum linenum "LEX" "Invalid macro name.")]))

;; macros have two forms:
;; #:name body
;; #:name
;; where body is everything after name with all leading and trailing whitespace removed
(define (read-macro str linenum)
  (match (regexp-match #rx"^\\#\\:([^ ]*) (.*)$" str)
         [(list r1 r2 r3) (process-macro r2 (string-trim r3) linenum)]
         [_ (match (regexp-match #rx"^\\#\\:([^ ]*) *$" str)
            [(list r1 r2) (process-macro r2 "" linenum)]
            [_ (cm-error-linenum linenum "LEX" "Invalid macro syntax.")])
            ]))

(define cmlex
  (lexer
   [(eof) eof]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space) (cmlex input-port)]
   [#\newline ":newline"]
   ;; skip comment lines
   [comment (cmlex input-port)]
   [macro 
    (match start-pos [(position _ linenum _)
            (read-macro lexeme linenum)])]
   [(:or "(" ")" "#") lexeme]
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

