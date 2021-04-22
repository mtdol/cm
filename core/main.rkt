#lang racket
(require cm/core/lex cm/core/ast cm/core/parse-expr 
         cm/core/parse-stat cm/core/interp cm/core/types
         cm/core/context cm/core/modules cm/core/error
         cm/core/pre-parse cm/core/tokens
         )
(provide run run-file run-expr run-tokenize-string
         run-parse run-parse-expr run-prefix-form
         display-output display-expr-output silent
         set-verbose-error-level!
         current-module-id get-current-module-id set-current-module-id!)

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
  (unless (file-exists? file) 
    (cm-error "GENERAL" (format "File does not exist: \"~a\"" file)))
  (let* ([id (file-name->module-id file)]
         [res 
          (begin 
            (set-current-module-id! id)
            (interp 
              (parse-stat 
                (tokenize-string (file->string id) id)
                id)
              id))])
    ;; reset the module-id
    (set-current-module-id! id)
    res
      ))

;; runs an expr (no dot)
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
