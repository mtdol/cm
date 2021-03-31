#lang racket
(require racket/lazy-require)
(lazy-require (cm/core/context [get-current-module-id])
              (cm/core/types [value->displayable-string]))
(provide cm-error cm-error-linenum cm-error-with-line-handler get-current-linenum
         set-current-linenum! get-trace-data set-trace-data!
         get-id-from-message try-with-error)

(define error-types
    (set (list
        "GENERIC" "LEX" "SYNTAX" "MACRO" "IMPORT" "CONTRACT" 
        "UNDEFINED" "SYSTEM" "HASHREF" 
        )))

;; line-number belonging to the current statement
(define current-linenum 0)
(define (get-current-linenum) 0)
(define (set-current-linenum! linenum) (set! current-linenum linenum))

(define trace-data '())
(define (get-trace-data) trace-data)
(define (set-trace-data! v) (set! trace-data v))

(define (get-trace-message)
  (match trace-data
         ['() 
          (string-append
            "module: "
            (get-current-module-id)
            "\n"
            "linenum: "
            (number->string current-linenum)
            )]))

;; general error function
(define (cm-error id message)
  (error (string-append
    id ": "
    (value->displayable-string message) "\n\n"
    (get-trace-message))))


(define (cm-error-linenum linenum id message)
  (set-current-linenum! linenum)
  (cm-error id message))

;; sets current linenum and execs the proc
(define (cm-error-with-line-handler linenum proc args) 
  (set! current-linenum linenum)
    (apply proc args))


;; throws an error of the given type if an exception is raised
(define (try-with-error type message proc args) 
  (with-handlers* ([exn:fail? (lambda (exn) (cm-error type message))])
    (apply proc args)))

(define (get-id-from-message msg) 
  ;; regexp for lines like "linenum:id: msg"
  (match (regexp-match #rx"^(.+?)\\:.*$" msg)
         [(list _ id) id]
         ;; should never happen
         [_ (cm-error "GENERIC" "Couldn't get id from error message.")]))
