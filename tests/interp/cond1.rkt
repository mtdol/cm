#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (interp (parse-expr (tokenize-string "cond | true -> 2 else 3")))
"2")

(check-equal? (interp (parse-expr (tokenize-string "| true -> 2 else 3")))
"2")

(check-equal? (interp (parse-expr (tokenize-string "| 4 -> 2 else 3")))
"2")

(check-equal? (interp (parse-expr (tokenize-string "| 4 -> 1 + 1 else 3")))
"2")

(check-equal? (interp (parse-expr (tokenize-string "| bool 0 -> 1 + 1 else 2 + 1")))
"3")

(check-equal? (interp (parse-expr (tokenize-string "| 1 + 2 = 3 -> 2 else 3")))
"2")

(check-equal? (interp (parse-expr (tokenize-string "| 1 + 2 = 2 -> 2 else 3")))
"3")

(check-equal? (interp (parse-expr (tokenize-string "| bool 0 -> 2 else 3")))
"3")

(check-equal? (interp (parse-expr (tokenize-string "cond | true -> 2 | 3 -> 4 else 5")))
"2")

(check-equal? (interp (parse-expr (tokenize-string "| true -> 2 | 3 -> 4 else 5")))
"2")

(check-equal? (interp (parse-expr (tokenize-string "| false -> 2 | 3 -> 4 else 5")))
"4")

(check-equal? (interp (parse-expr (tokenize-string "| false -> 2 | 1 + 2 = 1 -> 4 else 5")))
"5")

(check-equal? (interp (parse-expr (tokenize-string "1 + | bool 0 -> 3 else 4")))
"5")

(check-equal? (interp (parse-expr (tokenize-string "1 + (| bool 0 -> 3 else 4)")))
"5")

(check-equal? (interp (parse-expr (tokenize-string "(| bool 0 -> 2 else 3) + 5")))
"8")

(check-equal? (interp (parse-expr (tokenize-string "(| bool 1 -> 2 else 3) + 5")))
"7")

(check-equal? (interp (parse-expr (tokenize-string "| bool 0 -> 2 else 3 + 5")))
"8")
)
