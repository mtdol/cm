#lang racket
(require cm/core/cm readline/readline)

(define help "`#exit` exit\n`#e` exit\n`#help` display help\n`#run` run mode\n`#runxp` run expression mode\n`#parse` parse mode\n`#parsexp` parse expression mode\n`#token` tokenize mode\n\n")
(define exit-text "exiting\n")

;; the cm function we will be calling
(define mode cm-run-list)

(define (print-results lst)
  (match lst
         ['() (void)]
         [(cons h t) (displayln h) (print-results t)]
         [h (displayln h)]))


(define (repl) 
  (match (request)
         ["#exit" exit-text]
         ["#e" exit-text]
         ["#token" (list (displayln "tokenizing mode")
                         (set! mode cm-tokenize)
                         (add-history "#token")
                         (repl))]
         ["#run" (list (displayln "run mode")
                         (set! mode cm-run-list)
                         (add-history "#run")
                         (repl))]
         ["#runxp" (list (displayln "run expression mode")
                         (set! mode cm-run-expr)
                         (add-history "#runxp")
                         (repl))]
         ["#parse" (list (displayln "parse mode")
                         (set! mode cm-parse)
                         (add-history "#parse")
                         (repl))]
         ["#parsexp" (list (displayln "parse expression mode")
                         (set! mode cm-parse-expr)
                         (add-history "#parsexp")
                         (repl))]
         ["#prefix" (list (displayln "prefix expression mode")
                         (set! mode cm-prefix-form)
                         (add-history "#prefix")
                         (repl))]
         ["#help" (list (display help) (add-history "#help") (repl))]
         [s (list (add-history s)
                  ;; run and print results
                  (print-results (mode s))
                  ;(displayln (mode s))
                  (repl)
                  )]))

;; runs f and exits before values from f can be printed to console
(define (cleanse f)
  (match (cons (f) "")
         [_ (exit)]))

(define (request) 
       (readline "> "))
        

(define (repl-failsafe)
  (with-handlers* (
      [exn:fail? (lambda (e) (cons (displayln (match e [(exn:fail m _) m])) (repl-failsafe)))]
                  ) (repl)))

;; import standard lib
(cm-run-file-silent "std_lib/std.cm")

(display "Welcome to the cm repl!\nType `#exit` or `#e` to exit. Type `#help` for help.\n\n")
(cleanse repl-failsafe)
