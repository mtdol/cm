#lang racket
(require cm/core/pre-lex)
(provide tokenize-string tokenize-import-string)
(define error-id "LEX")

(require cm/core/error cm/core/reserved-keywords
  parser-tools/lex
    (prefix-in : parser-tools/lex-sre))

;; Matthew Dolinka
;;

(define current-module-id "0")

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
                   "*" "/" "\\" "^" "%" "&" "|" "@"
            "(" ")" "[" "]" "{" "}" ":" "$" "#")]

  [comment (:: "--" (:* (:~ #\newline)))]

  ;; based of standard racket string style
  [str (:: "\"" (:* string-element) "\"")]
  [string-element (:or (:~ "\"" "\\")
                        string-escape)]
  [string-escape (:: #\\ any-char)]


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
                        (cm-error-linenum current-module-id linenum error-id 
                                (format "Non-terminated string around column ~a." colnum))])]
   ;; everything else (vars and operators)
   [(:+ (:& (:+ any-char) (:~ import-key-token) (:~ whitespace) (:~ #\"))) lexeme]
   ;; custom error behavior
   [any-char (match start-pos [(position colnum linenum _)
                        (cm-error-linenum current-module-id linenum error-id 
                                (format "Lexing failure around column ~a." colnum))])]
   ))

(define cmlex
  (lexer-src-pos
   [(eof) (return-without-pos eof)]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space #\newline) (return-without-pos (cmlex input-port))]
   [comment (return-without-pos (cmlex input-port))]
   ;; escaped macro braces
   ["\\{" "\\{"]
   ["\\}" "\\}"]
   ;; "{label" -> ("{" "label"), regardless of what label is
   [(:: "{" (:* #\space) (:+ (:~ #\space #\{ #\})) (:* #\space) "}") 
    (return-without-pos 
      (list 
        (make-position-token "{" start-pos end-pos) 
        (make-position-token 
          (string-trim (substring lexeme 1 (- (string-length lexeme) 1)))
          start-pos end-pos) 
        (make-position-token "}" start-pos end-pos)))]
   ;; "{label " -> ("{" "label")
   [(:: "{" (:* #\space) (:+ (:~ #\space #\newline #\{ #\})) (:or #\newline #\space)) 
    (return-without-pos 
      (list 
        (make-position-token "{" start-pos end-pos) 
        (make-position-token 
          (string-trim (substring lexeme 1 (sub1 (string-length lexeme))))
          start-pos end-pos)))]
   [(:or "(" ")" "{" "}" "#" "..." "//") lexeme]
   ["`" "head"]
   ["~" "tail"]
   ["," "cons"]
   [";" (return-without-pos 
          (list 
            (make-position-token "cons" start-pos end-pos) 
            (make-position-token "null" start-pos end-pos)))]
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
   [str 
     (match start-pos 
       [(position _ linenum _)
        (set-current-linenum! linenum)
        (process-string lexeme)])]
   [(:: #\" (:* (:~ #\"))) (match start-pos [(position colnum linenum _)
                        (cm-error-linenum current-module-id linenum error-id 
                                (format "Non-terminated string around column ~a." colnum))])]
   ;; everything else (vars and operators)
   [(:+ (:& (:+ any-char) (:~ key-token) (:~ whitespace) (:~ #\"))) lexeme]
   ;; custom error behavior
   [any-char (match start-pos [(position colnum linenum _)
                        (cm-error-linenum current-module-id linenum error-id 
                                (format "Lexing failure around column ~a." colnum))])]
   ))



;; Converts \n to newline and so on.
;; Raises an exception if an invalid escape pattern is found
;;      (like \{ or something.)
(define (convert-string-components str)
  (let 
    ([str2 
       (foldl (lambda (elem acc)
                (string-replace acc (car elem) (cdr elem)))
              str
              '(
                ("\\n" . "\n")
                ("\\r" . "\r")
                ("\\t" . "\t")
                ("\\\"" . "\"")
                ))])
    ;; now look for illegal uses of backslash
    (match (regexp-match #rx"(\\\\.)" str2)
      [(list _ r1) 
       (cm-error-linenum 
          (get-current-linenum) current-module-id "LEX"
          (format "Illegal escape in string: ~a" r1))]
      [#f str2])))

;; process lexed string
(define (process-string str)
  ;; split by \\ and apply escapes
  (let ([strs (map convert-string-components (string-split str "\\\\"))])
    ;; stitch back together with \
    (foldl (lambda (elem acc)
             (string-append acc "\\" elem))
           (car strs) (cdr strs))))


(define (tokenize-input ip lex) 
  (port-count-lines! ip)
    (flatten (port->list lex ip)))

(define (tokenize-string str module-id) 
  (set! current-module-id module-id)
  (tokenize-input (open-input-string (pre-lex str module-id)) cmlex))
;; for use with the simple lexer
(define (tokenize-import-string str) 
  (tokenize-input (open-input-string str) import-list-lex))
