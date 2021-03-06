#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test
(check-equal? (interp (parse-expr (tokenize-string "4 - 1")))
3)

(check-equal? (interp (parse-expr (tokenize-string "4 -- 1")))
5)

(check-equal? (interp (parse-expr (tokenize-string "+4 - 1")))
3)

(check-equal? (interp (parse-expr (tokenize-string "+4")))
4)

(check-equal? (interp (parse-expr (tokenize-string "-4")))
-4)

(check-equal? (interp (parse-expr (tokenize-string "4 *- 1")))
-4)

(check-equal? (interp (parse-expr (tokenize-string "-4 *- 1")))
4)

(check-equal? (interp (parse-expr (tokenize-string "-4 *- 2")))
8)

(check-equal? (interp (parse-expr (tokenize-string "2 ^ 3")))
8)

(check-equal? (interp (parse-expr (tokenize-string "4.0 / 2.0")))
2.0)

(check-equal? (interp (parse-expr (tokenize-string "4.0 + 2.0")))
6.0)

(check-equal? (interp (parse-expr (tokenize-string "4 + 2")))
6)

(check-equal? (interp (parse-expr (tokenize-string "string (true and false)")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string (true xor false)")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "string (true or false)")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "string (true xor true)")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string (true or true)")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "string (true and true)")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "string not (true and true)")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string not true")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string not false")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "int 3.2")))
3)

(check-equal? (interp (parse-expr (tokenize-string "int 3.8")))
3)

(check-equal? (interp (parse-expr (tokenize-string "int 3.5")))
3)

(check-equal? (interp (parse-expr (tokenize-string "int 3.5 + 1")))
4)

(check-equal? (interp (parse-expr (tokenize-string "int (3.5 + 1.0)")))
4)

(check-equal? (interp (parse-expr (tokenize-string "int 3")))
3)

(check-equal? (interp (parse-expr (tokenize-string "3 % 2")))
1)

(check-equal? (interp (parse-expr (tokenize-string "string 4")))
"4")

(check-equal? (interp (parse-expr (tokenize-string "string 4.0")))
"4.0")

(check-equal? (interp (parse-expr (tokenize-string "string true")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "2 * (3 + 2)")))
10)

(check-equal? (interp (parse-expr (tokenize-string "(3 + 2) * 2")))
10)

(check-equal? (interp (parse-expr (tokenize-string "string null? ()")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "string null? null")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "string null? 3")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string int? 7")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "string int? \"7\"")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string int? 7.0")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string bool? int? 7.0")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "string bool? int 7.0")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string string? \"int 9.0\"")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "string string? int 9.0")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string list? int 9.0")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string list? (int 9.0;)")))
"true")

(check-equal? (interp (parse-expr (tokenize-string "string (list? int 9.0;)")))
"(false, null)")

(check-equal? (interp (parse-expr (tokenize-string "string (list? int 9.0, int 7.3)")))
"(false, 7)")

(check-equal? (interp (parse-expr (tokenize-string "string `(list? int 9.0, int 7.3)")))
"false")

(check-equal? (interp (parse-expr (tokenize-string "string ~(list? int 9.0, int 7.3)")))
"7")

(check-equal? (interp (parse-expr (tokenize-string "string ~(list? int 9.0, int 7.3, \"t6\")")))
"(7, t6)")

(check-equal? (interp (parse-expr (tokenize-string "string `~(list? int 9.0, int 7.3, \"t6\")")))
"7")
)
