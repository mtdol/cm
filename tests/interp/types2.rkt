#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test


(check-equal? (interp (parse-expr (tokenize-string 
        "string match 0 | types (\"int\", \"bool\";) a -> bool a end")))
"false")
(check-equal? (interp (parse-expr (tokenize-string 
        "string match true | types (\"int\", \"bool\";) a -> bool a end")))
"true")
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "string match 3.7 | types (\"int\", \"bool\";) a -> bool a end")))))

(check-equal? (interp (parse-expr (tokenize-string 
        "string match true, 4.6 | types (\"int\", \"bool\";) a, types (\"float\";) b -> b, a end")))
"(4.6, true)")
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string 
    "string match true, 4 | types (\"int\", \"bool\";) a, types (\"float\";) b -> b, a end")))))
)
