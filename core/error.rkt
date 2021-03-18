#lang racket
(provide cm-error cm-error-with-line-handler cm-error-linenum current-linenum
         get-id-from-message cm-error-no-linenum try-with-error)

(define error-types
    (set (list
        "GENERIC" "LEX" "PARSE" "INTERP" "IMPORT" "CONTRACT" "SYNTAX"
        "UNDEFINED"
        )))

;; line-number belonging to the current statement
(define current-linenum 0)

;; general error function
(define (cm-error id message)
  (cm-error-linenum current-linenum id message))

;; doesn't provide a linenum on the error
(define (cm-error-no-linenum id message)
  (error (string-append
                 "NL:"
                 id
                 ": "
                 message)))

;; sends error with line number included
(define (cm-error-linenum linenum id message) 
  (error (string-append
                 (number->string linenum)
                 ":"
                 id
                 ": "
                 message)))

;; sets current linenum and execs the proc
(define (cm-error-with-line-handler linenum proc args) 
  (set! current-linenum linenum)
    (apply proc args))


;; throws an error if the given type if an exception is raised
(define (try-with-error type message proc args) 
  (with-handlers* ([exn:fail? (lambda (exn) (cm-error type message))])
    (apply proc args)))

(define (get-id-from-message msg) 
  ;; regexp for lines like "linenum:id: msg"
  (match (regexp-match #rx"^(?:[0-9]+|NL)\\:(.+?)\\:.*$" msg)
         [(list _ id) id]
         ;; should never happen
         [_ (cm-error "GENERIC" "Couldn't get id from error message.")]))
