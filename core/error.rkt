#lang racket
(provide cm-error cm-error-with-line-handler cm-error-linenum)

(define error-types
    (make-hash (list
        (cons 1     "Lex Error")
        (cons 2     "Parse Error")
        (cons 3     "Interp Error")
        (cons 4     "Type Error")
        )))

(define (id-to-error-string id) 
        (hash-ref error-types id (lambda () "Unknown Error")))

;; general error function
(define (cm-error id message)
  (error (string-append 
                 (id-to-error-string id)
                 ": "
                 message)))


;; sends error with line number included
(define (cm-error-linenum linenum id message) 
  (error (string-append
                 (number->string linenum)
                 ":"
                 (id-to-error-string id)
                 ": "
                 message)))

;; calls the given function with a handler that will intercept any error
;; messages recieved and raise them again with a line number included.
(define (cm-error-with-line-handler linenum func arg) 
  (with-handlers* ([exn:fail?
            (lambda (e) 
                (match e 
                  [(exn:fail m _)
                   (error (string-append (number->string linenum) ":" m))]))])
    (func arg)))
