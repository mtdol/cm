#lang racket
(require cm/core/ast)
(provide cm-error cm-error-linenum 
         get-current-linenum set-current-linenum! set-check-current-linenum!
         get-trace-stack push-elem-to-trace-stack! pop-elem-from-trace-stack!
         reset-trace-stack! ast-node-to-trace-elem run-and-reset-stack
         trace-stack->string
         TraceElem
         DebugData
         debug->linenum
         VERBOSE_ERR_LEVEL LIGHT_ERR_LEVEL 
         get-error-level set-error-level!
         get-id-from-message try-with-error)

;; Matthew Dolinka
;;

(define VERBOSE_ERR_LEVEL 1)
(define LIGHT_ERR_LEVEL 0)
(define error-level LIGHT_ERR_LEVEL)
(define (get-error-level) error-level)
(define (set-error-level! level) (set! error-level level))

(define error-types
    (set (list
        "GENERIC" "LEX" "SYNTAX" "MACRO" "IMPORT" "CONTRACT" 
        "UNDEFINED" "SYSTEM" "HASHREF" 
        )))

;; line-number belonging to the current statement
(define current-linenum 0)
(define (get-current-linenum) current-linenum)
(define (set-current-linenum! linenum) (set! current-linenum linenum))
;; only sets the new linenum if > 0
(define (set-check-current-linenum! linenum)
  (when (> linenum 0)
    (set! current-linenum linenum)))

;; (TraceElem "var" "x" "/home/user/cm/std_lib/std.cm" 32)
;; (TraceElem "fun" "add1" "/home/user/cm/std_lib/std.cm" 32)
;; (TraceElem "statement" "" "/home/user/cm/std_lib/std.cm" 32)
;;
;; "var" | "statement", string, string, int
(struct TraceElem (type label module-id linenum) #:transparent)

;; object used to hold debug information during interp
(struct DebugData (linenum))
(define (debug->linenum debug) 
  (match debug 
    [(DebugData linenum) linenum]))

(define trace-stack '())
(define max-trace-stack-size 100)
(define (get-trace-stack) trace-stack)
(define (push-elem-to-trace-stack! elem) 
  (if (>= (length trace-stack) max-trace-stack-size)
    (set! trace-stack (cons elem (drop-right trace-stack 1)))
    (set! trace-stack (cons elem trace-stack))))

;; pops a element from the trace stack and returns void
;; if no element is present, then does nothing
(define (pop-elem-from-trace-stack!)
  (unless (null? trace-stack)
    (set! trace-stack (cdr trace-stack))))
(define (reset-trace-stack!) (set! trace-stack '()))

;; takes in a val, resets stack and returns val
(define (run-and-reset-stack v) (reset-trace-stack!) v)

(define (ast-node-to-trace-elem ast module-id debug) 
  ;; get the label of the struct
  (let ([type (symbol->string (prefab-struct-key ast))]
        [linenum (match debug [(DebugData linenum) linenum])])
    (match ast
        [(Prim0 op) 
         (TraceElem (symbol->string op) "" module-id linenum)]
        [(Prim1 op _) 
         (TraceElem (symbol->string op) "" module-id linenum)]
        [(Prim2 op _ _) 
         (TraceElem (symbol->string op) "" module-id linenum)]
        [(Prefix2 op _ _) 
         (TraceElem (symbol->string op) "" module-id linenum)]
        [(Prefix3 op _ _ _) 
         (TraceElem (symbol->string op) "" module-id linenum)]
        [(Var id) 
         (TraceElem type id module-id linenum)]
        [_ (TraceElem type "" module-id linenum)]
           )))


;; formats `until` elements from the trace stack, starting from the freshest elements.
;; If until is #f, then all elements will be used
;;
;; if `trim`, then a statement will only be included if it is the first element
;; in the trace-stack
;;
;; int | bool -> string
(define (trace-stack->string until)
  (let ([until (if until until (sub1 (length trace-stack)))])
  (let aux ([elems trace-stack] [i 0] [last #f] [last-count 0])
       (match elems
              [(cons h t) #:when (equal? h last) (aux t i last (add1 last-count))]
              [(cons h t)
               #:when (and (not (zero? last-count)) (not (equal? h last)))
               (string-append 
                 (format "  {Repeats ~a time(s)...}\n" last-count)
                 (aux (cons h t) i #f 0))]
              ['() #:when (not (zero? last-count)) 
               (string-append 
                 (format "  {Repeats ~a time(s)...}" last-count))]
              [_ #:when (= until i) "  ..."]
              ['() ""]
              [(cons (TraceElem "statement" label module-id linenum) t)
               (string-append 
                 (format "  Statement:: Module: \"~a\":~a\n"
                                module-id linenum)
                 (aux t (add1 i) (car elems) 0))]
              [(cons (TraceElem type label module-id linenum) t)
               (string-append 
                 (if (string=? "" type)
                   (format "  Module: \"~a\":~a ~a\n"
                                  module-id linenum  type)
                   (format "  Module: \"~a\":~a ~a ~a\n"
                                  module-id linenum type label))
                   (aux t (add1 i) (car elems) 0))]
              ))))

(define (get-trace-message)
  (match trace-stack
         [_ #:when (= error-level VERBOSE_ERR_LEVEL)
            (trace-stack->string 40)]
         [_ 
            (trace-stack->string 20)]
         ))

;; general error function,
;; performs stack trace
(define (cm-error id message)
  (let ([trace-message (get-trace-message)])
    (reset-trace-stack!)
    (error 
      (format "~a: ~a\n\ncontext:\n~a"
        id message trace-message))))


;; simple error function,
;; no stack trace, but prints line number and module-id
(define (cm-error-linenum module-id linenum id message)
  (error 
    (format "~a: ~a\n\nModule: \"~a\":~a"
            id message module-id linenum)))


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
