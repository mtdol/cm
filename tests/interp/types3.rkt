#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(interp (parse-expr (tokenize-string 
    "typedef S := types (\"int\", \"float\";) a, types (\"string\", \"dynamic\";) b;")))

(check-equal? (interp (parse-expr (tokenize-string "string struct S (4.2, 7;)")))
"(struct S (4.2, 7, null))")
(check-equal? (interp (parse-expr (tokenize-string "string struct S (4, 7;)")))
"(struct S (4, 7, null))")
(check-equal? (interp (parse-expr (tokenize-string "string struct S (4, true;)")))
"(struct S (4, true, null))")

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "string struct S (true, 7;)")))))



)
