#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(interp (parse-expr (tokenize-string "typedef St2 = int a, int b;")))
(interp (parse-expr (tokenize-string "typedef St1 = int a,  (struct St2 st);")))
(check-equal? (interp (parse-expr (tokenize-string "string def s2 = struct St2 4,5;")))
"(struct St2 (4, 5, null))")
(check-equal? (interp (parse-expr (tokenize-string "string s2")))
"(struct St2 (4, 5, null))")

(check-equal? (interp (parse-expr (tokenize-string "string def s1 = struct St1 7, (struct St2 5,6;);")))
"(struct St1 (7, (struct St2 (5, 6, null)), null))")
(check-equal? (interp (parse-expr (tokenize-string "string s1")))
"(struct St1 (7, (struct St2 (5, 6, null)), null))")

(check-equal? (interp (parse-expr (tokenize-string "string struct? St2 s1")))
"false")
(check-equal? (interp (parse-expr (tokenize-string "string struct? St1 s1")))
"true")
(check-equal? (interp (parse-expr (tokenize-string "string struct? St2 s2")))
"true")
(check-equal? (interp (parse-expr (tokenize-string "string struct? St1 s2")))
"false")
(check-equal? (interp (parse-expr (tokenize-string "string struct? St1 struct St2 4,5;")))
"false")
(check-equal? (interp (parse-expr (tokenize-string "string struct? St1 struct St1 7, (struct St2 4,5;);")))
"true")

(interp (parse-expr (tokenize-string "typedef St = a, int b;")))
(check-equal? (interp (parse-expr (tokenize-string "string struct St 4,5;")))
"(struct St (4, 5, null))")
(check-equal? (interp (parse-expr (tokenize-string "string struct St 4.3,5;")))
"(struct St (4.3, 5, null))")

)
