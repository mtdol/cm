#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (interp (parse-expr (tokenize-string "try 3 + 4 catch e with 8")))
7)

(check-equal? (interp (parse-expr (tokenize-string "try 3 + 4.0 catch e with 8")))
8)

(check-equal? (interp (parse-expr (tokenize-string "try 3 + 4.0 catch e with match e | struct Error id,msg; -> id end")))
"CONTRACT")

(check-equal? (interp (parse-expr (tokenize-string "try error \"my error\" catch e with match e | struct Error id,msg; -> msg end")))
"NL:GENERIC: my error")

(check-equal? (interp (parse-expr (tokenize-string "try error \"MY_ID\",\"my error\"; catch e with match e | struct Error id,msg; -> id end")))
"MY_ID")

(check-equal? (interp (parse-expr (tokenize-string "try error \"MY_ID\",\"my error\"; catch e with match e | struct Error id,msg; -> msg end")))
"NL:MY_ID: my error")

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "try 3 + 4.0 catch 6")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "try 3 + 4.0 catch e")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "try 3 + 4.0 catch 9e with 8")))))

)
