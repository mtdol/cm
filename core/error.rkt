#lang racket
(provide cm-error cm-error-with-line-handler cm-error-linenum current-linenum)

(define error-types
    (make-hash (list
        (cons 1     "Lex Error")
        (cons 2     "Parse Error")
        (cons 3     "Interp Error")
        (cons 4     "Type Error")
        (cons 5     "Import Error")
        )))

(define (id-to-error-string id) 
        (hash-ref error-types id (lambda () "Unknown Error")))

;; line-number belonging to the current statement
(define current-linenum 0)

;; general error function
(define (cm-error id message)
  (cm-error-linenum current-linenum id message))


;; sends error with line number included
(define (cm-error-linenum linenum id message) 
  (error (string-append
                 (number->string linenum)
                 ":"
                 (id-to-error-string id)
                 ": "
                 message)))

;; sets current linenum and execs the proc
(define (cm-error-with-line-handler linenum proc args) 
  (set! current-linenum linenum)
    (apply proc args))
