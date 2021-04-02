#lang racket
(require cm/core/pre-lex)
(provide tokenize-file tokenize-string tokenize-import-string)
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

  [macro-label (:: letter (:* (:or alphanumeral "_")))]

  ;; operators that can be placed after digits or variables
  ;; ie. in "3+2", + would count
  [key-token (:or "`" "~" "," ";" "=" ">" "<" "+" "-"
                   "*" "/" "\\" "^" "%" "&" "|" "!" "@"
            "(" ")" "[" "]" "{" "}" ":" "$" "#" ".")]

  [string-char (:~ #\")]

  [comment (:: "--" (:* (:~ #\newline)))]
  [macro (:: "#:" (:* (:~ #\newline)))]


  [import-key-token (:or ",")]
  )


;; lexes the types of strings used in import macro
(define import-list-lex
  (lexer
   [(eof) eof]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space #\newline) (import-list-lex input-port)]
   [(:: "->") "->"]
   [(:: ",") ","]
   [(:: #\" (:* (:or (:: "\\\"") (:~ #\"))) #\") lexeme]
   [(:: #\" (:* (:~ #\"))) (match start-pos [(position colnum linenum _)
                        (cm-error-linenum linenum error-id 
                                (format "Non-terminated string around column ~a." colnum))])]
   ;; everything else (vars and operators)
   [(:+ (:& (:+ any-char) (:~ import-key-token) (:~ whitespace) (:~ #\"))) lexeme]
   ;; custom error behavior
   [any-char (match start-pos [(position colnum linenum _)
                        (cm-error-linenum linenum error-id 
                                (format "Lexing failure around column ~a." colnum))])]
   ))

(define cmlex
  (lexer
   [(eof) eof]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space) (cmlex input-port)]
   [#\newline ":newline"]
   ;; skip comment lines
   [comment (cmlex input-port)]
   ;; escaped macro braces
   ["\\{" "\\{"]
   ["\\}" "\\}"]
   ;; "{label" -> ("{" "label"), regardless of what label is
   [(:: "{" (:* #\space) (:+ (:~ #\space #\{ #\})) (:* #\space) "}") 
    (list "{" (string-trim (substring lexeme 1 (- (string-length lexeme) 1))) "}")]
   ;; "{label " -> ("{" "label")
   [(:: "{" (:* #\space) (:+ (:~ #\space #\newline #\{ #\})) (:or #\newline #\space)) 
    (list "{" (string-trim (substring lexeme 1 (sub1 (string-length lexeme)))))]
   [(:or "(" ")" "{" "}" "#") lexeme]
   ["." "dot"]
   ["//" "dot"]
   ["`" "head"]
   ["~" "tail"]
   ["," "cons"]
   [";" (list "cons" "null")]
   ["()" "null"]
   ["[]" "null"]
   ["[" "("]
   ["]" ")"]
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
   ;; escaped bar used in macros
   ["\\|" "\\|"]
   ["|" "case"]
   ["!" "not"]
   ["$" "cat"]
   ["@" "print"]
   ["lam" "lambda"]
   [":=" "assign"]
   [":" "apply"]
   [":>" "appnull"]
   ["::" "appindex"]
   ["->" "yields"]
   [(:+ digit) lexeme]
   [(:: (:+ digit) #\. (:+ digit)) lexeme]
   [(:: #\" (:* (:or (:: "\\\"") (:~ #\"))) #\") lexeme]
   [(:: #\" (:* (:~ #\"))) (match start-pos [(position colnum linenum _)
                        (cm-error-linenum linenum error-id 
                                (format "Non-terminated string around column ~a." colnum))])]
   ;; everything else (vars and operators)
   [(:+ (:& (:+ any-char) (:~ key-token) (:~ whitespace) (:~ #\"))) lexeme]
   ;; custom error behavior
   [any-char (match start-pos [(position colnum linenum _)
                        (cm-error-linenum linenum error-id 
                                (format "Lexing failure around column ~a." colnum))])]
   ))


(define (tokenize-input ip lex) 
  (port-count-lines! ip)
    (flatten (port->list lex ip)))


(define (tokenize-file name) (tokenize-string 
                (file->string (path->string (path->complete-path name)))))
(define (tokenize-string str) (tokenize-input (open-input-string (pre-lex str)) cmlex))
(define (tokenize-import-string str) 
  (tokenize-input (open-input-string str) import-list-lex))
