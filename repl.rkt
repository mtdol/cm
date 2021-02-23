#lang racket
(require "cm.rkt" readline/readline)

(define help "`:exit` or `:e` to exit.\n")
(define exit-text "exiting\n")

(define (repl) 
  (match (request)
         [":exit" exit-text]
         [":e" exit-text]
         [":help" (list (display help) (repl-failsafe))]
         [s (list (add-history s)
                  (displayln (run s))
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

(display "Welcome to the cm repl!\nType `:exit` or `:e` to exit. Type `:help` for help.\n\n")
(cleanse repl-failsafe)
