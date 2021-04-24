#lang racket
(require racket/lazy-require 
         cm/core/error cm/core/context cm/core/modules
         cm/core/tokens)
(lazy-require (cm/core/lex [tokenize-string tokenize-import-string]))
(provide pre-lex unwrap-string unwrap-string-if-necessary)

;; Matthew Dolinka
;;

(define (pre-lex str module-id) 
  (parse-defs 
    (map string-trim (string-split str #rx"\n")) 
    module-id 1))

;; parses macro definitions
(define (parse-defs lines module-id linenum)
  (match lines
         ['() ""]
         ;; beginning of multi-line macro 
         [(cons s t) #:when 
            (and (>= (string-length s) 3) (string=? (substring s 0 3) "#:<"))
            (match (regexp-match #rx"^\\#\\:\\<([^ ]+)$" s)
                   [(list _ r2) (string-append 
                                  "\n"
                                  (parse-multi-line-defs 
                                    t module-id (add1 linenum) r2 ""))]
                   [#f (cm-error-linenum module-id linenum "LEX" "Improperly formed multi-line macro def.")])
            ]
         ;; generic macro
         [(cons s t) #:when 
            (and (>= (string-length s) 2) (string=? (substring s 0 2) "#:"))
            (string-append (read-macro s module-id linenum) "\n" 
                           (parse-defs t module-id (add1 linenum)))]
         [(cons s '()) s]
         [(cons s t) (string-append s "\n" (parse-defs t module-id (add1 linenum)))]))

(define (parse-multi-line-defs lines module-id linenum name body)
  (match lines
         ['() (cm-error-linenum module-id linenum "LEX" (format "No termination of multi line macro: ~a" name))]
         [(cons s t) #:when (string=? ">:#" s) 
                     (string-append (process-macro name (string-trim body) module-id linenum)
                             "\n"
                             (parse-defs t module-id (add1 linenum)))]
         [(cons s t) #:when 
            (and (>= (string-length s) 2) (string=? (substring s 0 2) "#:"))
            (cm-error-linenum module-id linenum "LEX" "Cannot place macro inside multi-line macro def.")]
         [(cons s t) (string-append 
                        "\n" 
                        (parse-multi-line-defs t module-id (add1 linenum) name 
                                               (string-append body s "\n")))]))

;; all the files to import for standard cm lang
(define cm-lang-imports '("std_lib::std.cm"))
(define (process-macro name body module-id linenum)
  (set-current-linenum! linenum)
  (match name
         ["lang" 
          (if (string=? body "cm")
            (begin 
              (map (lambda (elem) (process-import elem "" module-id)) cm-lang-imports)
              "")
            (cm-error-linenum module-id linenum "LEX" "Invalid lang name in macro."))]
         [name #:when (regexp-match? #rx"^def\\:.*" name)
               (match (regexp-match #rx"^def\\:([^{}]+){([a-zA-Z0-9_\\|]*)\\}$" name)
                      [(list _ r2 r3)
                       (set-macro! 
                         r2 
                         (map tok (remove-bars (tokenize-string r3 module-id)))
                         (map token->nl-token (tokenize-string body module-id))
                         (var-name-private? r2)
                         module-id)
                       ""]
                      [_ (cm-error-linenum module-id linenum "LEX" "Invalid macro def name.")]

                 )]
         [name #:when (regexp-match? #rx"^def\\+\\:.*" name)
               (match (regexp-match #rx"^def\\+\\:([^{}]+){([a-zA-Z0-9_\\|]*)\\}$" name)
                      [(list _ r2 r3) 
                       (append-to-macro! 
                         r2 
                         (map tok
                            (remove-bars (tokenize-string r3 module-id)))
                         (map token->nl-token (tokenize-string body module-id))
                         module-id)
                       ""]
                      [_ (cm-error-linenum module-id linenum "LEX" "Invalid macro def name.")]

                 )]
         [name #:when (regexp-match? #rx"^import$" name)
               (map 
                 (lambda (elem) 
                   (process-import elem "" module-id)) 
                 (file-list-string->list body module-id))
               ""]
         [name #:when (regexp-match? #rx"^import:.+" name)
               (match (regexp-match #rx"^import:(.+)$" name)
                  [(list _ r1)
                   (map 
                 (lambda (elem) 
                   (process-import elem r1 module-id)) 
                 (file-list-string->list body module-id))]) 
               ""]

         [name #:when (regexp-match? #rx"^lazy_import$" name)
               (process-lazy-import-macro body "" module-id) ""]
         [name #:when (regexp-match? #rx"^lazy_import:.+" name)
               (match (regexp-match #rx"^lazy_import:(.+)$" name)
                  [(list _ r1)
                    (process-lazy-import-macro body r1 module-id)])
               ""]
         [_ (cm-error-linenum module-id linenum "LEX" "Invalid macro name.")]))

(define (process-lazy-import-macro body prefix module-id)
   (let-values ([(file-str args) (lazy-import-string->list body module-id)])
   (map 
     (lambda (elem)
       (match elem
              [(cons type item)
               (process-lazy-import file-str type item prefix module-id)])
       ) 
     args)))


;; removes "case" from a list, so it can be stored in the macro context
(define (remove-bars ts)
  (filter (lambda (elem) (not (string=? (tok elem) "case"))) ts))

;; unwraps the given string and returns it else false
;;
;; string -> string | bool
(define (unwrap-string str)
  (match (regexp-match #rx"^\\\"(.*)\\\"$" str) 
           [(list _ r1) r1] 
           [_ #f]))
;; unwraps if is a string, else just returns str
(define (unwrap-string-if-necessary str) 
  (match (unwrap-string str)
         [#f str]
         [res res]))

;; takes in a string representing a list of filenames and returns them
;;
;; string -> string list
(define (file-list-string->list str module-id) 
  (map (lambda (elem) 
         (match (unwrap-string elem) 
           [#f (cm-error-linenum 
                 module-id
                 (get-current-linenum)
                 "LEX" "File list arguments must be strings wrapped in quotes.")]
           [res res] 
           ))
       ;; remove the "," from the string
    (filter (lambda (elem) (not (string=? elem ","))) (tokenize-import-string str))))

;; returns a lazy require string as a list of strings where the first element is the file name
;; that we are importing from
;;
;; string -> values (string, (string . string) list)
(define (lazy-import-string->list str module-id)
  (let ([tokens (tokenize-import-string str)])
    (unless (and (> (length tokens) 2) (equal? (car (cdr tokens)) "->"))
      (cm-error-linenum
        module-id
        (get-current-linenum) "LEX" "Improperly formed lazy_import."))
      (values 
        (match (unwrap-string (car tokens)) 
           [#f (cm-error-linenum 
                 module-id
                 (get-current-linenum) 
                 "LEX" "File arguments to lazy_import must be strings wrapped in quotes.")]
           [res res] 
           )
        ;; mode 0 = type name, 1 = cons
        (let aux ([tokens (cddr tokens)] [mode 0])
          (match tokens
                 ['() '()]
                 [(cons "," t) #:when (= 1 mode) (aux t 0)]
                 [(cons type (cons var t)) 
                  #:when (= 0 mode)
                  (cons (cons type (unwrap-string-if-necessary var)) (aux t 1))]
                 [_ (cm-error-linenum 
                      module-id
                      (get-current-linenum) "LEX" "Improperly formed lazy_import.")])))))

;; macros have two forms:
;; #:name body
;; #:name
;; where body is everything after name with all leading and trailing whitespace removed
(define (read-macro str module-id linenum)
  (match (regexp-match #rx"^\\#\\:([^ ]*) (.*)$" str)
         [(list r1 r2 r3) (process-macro r2 (string-trim r3) module-id linenum)]
         [_ (match (regexp-match #rx"^\\#\\:([^ ]*) *$" str)
            [(list r1 r2) (process-macro r2 "" module-id linenum)]
            [_ (cm-error-linenum module-id linenum "LEX" "Invalid macro syntax.")])
            ]))
