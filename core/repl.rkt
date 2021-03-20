#lang racket
(require cm/core/main)

(define-for-syntax WINDOWS? (equal? (system-type) 'windows))

;; macros for windows versus linux
(define-syntax (match-opsys stx)
  (syntax-case stx ()
    ((_ windows-expr other-expr)
    (if WINDOWS?
         #'windows-expr
         #'other-expr
        )
        )))

(match-opsys
    (begin 
      (define (request) (display "> ") (string-replace (read-line) "\r" ""))
      (define (add-history item) (void)))
    (begin 
      (require readline/readline)
      (define (request) (readline "> "))))


;; adds a dot to the end of the string if not present
(define (add-dot str)
  (if (regexp-match? #rx"^.*(\\.|dot) *$" str)
    str
    (string-append str ".")))

;; the cm function we will be calling
(define mode (compose display-output run add-dot))


(define help (string-append "`#exit` exit\n"
                            "`#e` exit\n"
                            "`#help` display help\n" 
                            "`#run` run mode\n" 
                            "`#runxp` run expression mode\n"
                            "`#parse` parse mode\n"
                            "`#parsexp` parse expression mode\n"
                            "`#prefix` tokens to prefix form mode\n"
                            "`#tostring` tokens to ast to string mode\n"
                            "`#token` tokenize mode\n"
                            "\n"))

(define exit-text "exiting\n")


(define (repl) 
  (match (request)
         ["#exit" exit-text]
         ["#e" exit-text]
         [s #:when (eof-object? s) (displayln "")]
         ["#token" (list (displayln "tokenizing mode")
                         (set! mode (compose displayln run-tokenize-string))
                         (add-history "#token")
                         (repl))]
         ["#run" (list (displayln "run mode")
                         (set! mode (compose display-output run add-dot))
                         (add-history "#run")
                         (repl))]
         ["#runxp" (list (displayln "run expression mode")
                         (set! mode (compose display-expr-output run-expr))
                         (add-history "#runxp")
                         (repl))]
         ["#parse" (list (displayln "parse mode")
                         (set! mode (compose displayln run-parse))
                         (add-history "#parse")
                         (repl))]
         ["#parsexp" (list (displayln "parse expression mode")
                         (set! mode (compose displayln run-parse-expr))
                         (add-history "#parsexp")
                         (repl))]
         ["#prefix" (list (displayln "prefix expression mode")
                         (set! mode (compose displayln run-prefix-form))
                         (add-history "#prefix")
                         (repl))]
         ["#tostring" (list (displayln "ast to string mode")
                         (set! mode (compose displayln run-ast-to-string))
                         (add-history "#tostring")
                         (repl))]
         ["#help" (list (display help) (add-history "#help") (repl))]
         [s (list (add-history s)
                  ;; run the current mode
                  (mode s)
                  (repl)
                  )]))

;; runs f and exits before values from f can be printed to console
(define (cleanse f)
  (match (cons (f) "")
         [_ (exit)]))


(define (repl-failsafe)
  (with-handlers* (
      [exn:break? (lambda (e) (begin (displayln (match e [(exn:break m _ _) m])) (repl-failsafe)))]
      [exn:fail? (lambda (e) (begin (displayln (match e [(exn:fail m _) m])) (repl-failsafe)))]
                  ) (repl)))

;; import standard libs
(define (run-lang-line!) (begin (run-expr "!lang cm") (void)))

(run-lang-line!)
(display "Welcome to the cm repl!\nType `#exit` or `#e` to exit. Type `#help` for help.\n\n")
(cleanse repl-failsafe)
