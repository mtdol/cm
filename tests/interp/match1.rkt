#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "match 5 | 5 -> 2 end")
2)

(check-equal? (run "match 5,2 | 5,2 -> 2 end")
2)

(check-equal? (run "match 5,3 | 5,2 -> 2 | 5,3 -> 7 end")
7)

(check-equal? (run "match 5,3 | 5 -> 2 | 5,3 -> 7 end")
7)

(check-equal? (run "match 5 | 5 -> 2 | 5,3 -> 7 end")
2)

(check-equal? (run "string match 5 | 5,3 -> 2 | 5 -> 7, 3 end")
"(7, 3)")

(check-failure run "match 5 | match 5 + 1 | 4 -> 0 | 6 -> 5 end -> 3 | 5,3 -> 7 end")


(check-equal? (run "match 5 | 5 -> (match 5 + 1 | 4 -> 0 | 6 -> 8 end) | 5,3 -> 7 end")
8)


(check-equal? (run "match 1 | 1 when true -> \"one\" | 1 when not true -> \"other one\" end")
"one")

(check-equal? (run "match 1 | 1 when not true -> \"one\" | 1 when true -> \"other one\" end")
"other one")

(check-equal? (run "match 1 | 1 when not true -> \"one\" | 1 -> \"other one\" end")
"other one")

(check-failure run "match 6 | 5 -> 2 end")

(check-failure run "match 6 end")

(check-failure run "match end")

(check-failure run "match 6 | true -> 2 end")

(check-failure run "match 6 | 5 -> 2 | 3 -> 9 end")

(check-failure run "match 6,3 | 5 -> 2 | 7,3 -> 9 end")

(check-failure run "match 6 | 5,1 -> 2 | 7 -> 9 end")

(check-failure run "match 5 | match 5 + 1 | 4 -> 0 | 6 -> 4 end -> 3 | 5,3 -> 7 end")

(check-failure run "match 5 | (match 5 + 1 | 4 -> 0 | 6 -> 4 end) -> 3 | 5,3 -> 7 end")


(check-failure run "match 6 | 5 -> 2 3 -> 9 end")

(check-failure run "match 6 | 5 when -> 2 | 3 -> 9 end")

(check-failure run "match 6 | when -> 2 | 3 -> 9 end")

(check-failure run "match 6 | when 5 -> 2 | 3 -> 9 end")

(check-failure run "match 6 | when 5 2 -> 2 | 3 -> 9 end")

(check-failure run "match 6 | 5 -> 2 3 -> 9 end")

(check-failure run "match 6 | 5 -> 2 | 3 -> 9")

(check-failure run "match 6 | 5 -> 2 4 -> 9")

(check-failure run "match true | 6 -> 2 end")

(check-failure run "match  | 6 -> 2 end")

(check-failure run "match match | 6 -> 2 end")

(check-failure run "match match 7 | 6 -> 2 end")

(check-failure run "match true 6 -> 2 end")

(check-failure run "match true | -> 2 end")

(check-failure run "match true | 6 2 end")

(check-failure run "match true | 6 else 2 end")

(check-failure run "match true | 6 -> 2")

(check-failure run "match true | 6 2 end")

(check-failure run "match true | 6 -> 2 null")

(check-failure run "match true | 6 -> 2 else 7")

(check-failure run "match 6 -> 2 else 7")

(check-failure run "match -> 2 else 7")

(check-failure run "match -> 2 7")

(check-failure run "match 7")

(check-failure run "match")
