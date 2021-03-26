#lang racket
(require racket/lazy-require cm/core/error cm/core/context)
(lazy-require (cm/core/lex [tokenize-string]))
(provide pre-lex)

(define (pre-lex str) (parse-defs (map string-trim (string-split str #rx"\n")) 1))

;; parses macro definitions
(define (parse-defs lines linenum)
  (match lines
         ['() ""]
         ;; beginning of multi-line macro 
         [(cons s t) #:when 
            (and (>= (string-length s) 3) (string=? (substring s 0 3) "#:<"))
            (match (regexp-match #rx"^\\#\\:\\<([^ ]+)$" s)
                   [(list _ r2) (string-append 
                                  "\n"
                                  (parse-multi-line-defs t (add1 linenum) r2 ""))]
                   [#f (cm-error-linenum linenum "LEX" "Improperly formed multi-line macro def.")])
            ]
         ;; generic macro
         [(cons s t) #:when 
            (and (>= (string-length s) 2) (string=? (substring s 0 2) "#:"))
            (string-append (read-macro s linenum) "\n" (parse-defs t (add1 linenum)))]
         [(cons s '()) s]
         [(cons s t) (string-append s "\n" (parse-defs t (add1 linenum)))]))

(define (parse-multi-line-defs lines linenum name body)
  (match lines
         ['() (cm-error-linenum linenum "LEX" (format "No termination of multi line macro: ~a" name))]
         [(cons s t) #:when (string=? ">:#" s) 
                     (string-append (process-macro name (string-trim body) linenum)
                             "\n"
                             (parse-defs t (add1 linenum)))]
         [(cons s t) #:when 
            (and (>= (string-length s) 2) (string=? (substring s 0 2) "#:"))
            (cm-error-linenum linenum "LEX" "Cannot place macro inside multi-line macro def.")]
         [(cons s t) (string-append 
                        "\n" 
                        (parse-multi-line-defs t (add1 linenum) name 
                                               (string-append body s "\n")))]))

(define cm-lang-line "load \"std_lib::std.cm\" .")
(define (process-macro name body linenum)
  (match name
         ["lang" 
          (if (string=? body "cm")
            cm-lang-line
            (cm-error-linenum linenum "LEX" "Invalid lang name in macro."))]
         [name #:when (regexp-match? #rx"^def\\:.*" name)
               (match (regexp-match #rx"^def\\:([^{}]+){([a-zA-Z0-9_\\|]*)\\}$" name)
                      [(list _ r2 r3)
                       ;; for error linenums
                       (set-current-linenum! linenum)
                       (set-macro! r2 (remove-bars (tokenize-string r3)) (tokenize-string body)) ""]
                      [_ (cm-error-linenum linenum "LEX" "Invalid macro def name.")]

                 )]
         [name #:when (regexp-match? #rx"^def\\+\\:.*" name)
               (match (regexp-match #rx"^def\\+\\:([^{}]+){([a-zA-Z0-9_\\|]*)\\}$" name)
                      [(list _ r2 r3) 
                       (set-current-linenum! linenum)
                       (append-to-macro! r2 (remove-bars (tokenize-string r3)) (tokenize-string body)) ""]
                      [_ (cm-error-linenum linenum "LEX" "Invalid macro def name.")]

                 )]
         [_ (cm-error-linenum linenum "LEX" "Invalid macro name.")]))

;; removes "case" from a list, so it can be stored in the macro context
(define (remove-bars ts)
  (filter (lambda (elem) (not (string=? elem "case"))) ts))

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
