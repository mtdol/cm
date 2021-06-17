#lang racket
(require cm/core/lex cm/core/ast cm/core/parse-expr 
         cm/core/parse-stat cm/core/interp cm/core/types
         cm/core/context cm/core/modules cm/core/error
         cm/core/pre-parse cm/core/tokens
         )
(provide run run-file run-file/main run-expr run-tokenize-string
         run-parse run-parse-expr run-prefix-form
         display-output display-expr-output silent
         set-verbose-error-level!
         current-module-id get-current-module-id set-current-module-id!)

;; Matthew Dolinka
;;

(define current-module-id "0")
(define (get-current-module-id) (string-copy current-module-id))
(define (set-current-module-id! id) (set! current-module-id id))

;; reads over each list element and prints its tostring
;; value list | value -> void
(define (display-output input)
    (match input
           ['() (void)]
           [(cons h t) (displayln (string-coerce h)) (display-output t)]
           ;; if output is void
           [_ (void)]
           ))

(define (set-verbose-error-level!) (set-error-level! VERBOSE_ERR_LEVEL))

;; coerces an expr to a string and prints
(define (display-expr-output input)
    (displayln (string-coerce input)))

;; list -> void
(define (silent input) input (void))

;; runs a statement
;; string -> value list
(define (run input) 
  (interp 
    (parse-stat 
      (tokenize-string 
        input current-module-id)
      current-module-id)
    current-module-id))

;; string -> value list
(define (run-file file)
  (define (run-file file) 
    (let ([file (module-string->filename file)])
    (unless (file-exists? file) 
      (cm-error "GENERAL" (format "File does not exist: \"~a\"" file)))
    (let* ([id (file-name->module-id file)])
      (set-current-module-id! id)
      (interp 
        (parse-stat 
          (tokenize-string (file->string id) id)
          id)
        id))))
  (run-file file))

;; runs the main function if it exists, then returns void
;;
;; string, string list -> value list
(define (run-file/main file args)
  (let ([module-id (file-name->module-id (module-string->filename file))])
    ;; run `file`, discard results
    (run-file file)
    ;; run `main` if it exists with `args`
    (interp-expr 
      (run-parse-expr 
        (format "if defined? \"main\" \"~a\"
                and fun? main then main:args else void" module-id))
      (set-local-var "args" args (hash)) (hash) current-module-id '())
    (void)))

;; runs an expr (no terminator)
(define (run-expr input) 
  (interp 
    (parse-expr 
      (pre-parse (tokenize-string input current-module-id) current-module-id)
      current-module-id)
    current-module-id))

(define (run-tokenize-string input) 
  (map tok (tokenize-string input current-module-id)))

(define (run-parse input) 
  (parse-stat (tokenize-string input current-module-id) current-module-id))
(define (run-parse-expr input)
  (parse-expr (tokenize-string input current-module-id) current-module-id))

(define (run-prefix-form input) 
  (map tok 
       (half-parse-expr 
         (tokenize-string input current-module-id) current-module-id)))
