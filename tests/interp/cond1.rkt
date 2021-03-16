#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "cond | true -> 2 else 3")
2)

(check-equal? (run "| true -> 2 else 3")
2)

(check-equal? (run "| 4 -> 2 else 3")
2)

(check-equal? (run "| 4 -> 1 + 1 else 3")
2)

(check-equal? (run "| bool 0 -> 1 + 1 else 2 + 1")
3)

(check-equal? (run "| 1 + 2 = 3 -> 2 else 3")
2)

(check-equal? (run "| 1 + 2 = 2 -> 2 else 3")
3)

(check-equal? (run "| bool 0 -> 2 else 3")
3)

(check-equal? (run "cond | true -> 2 | 3 -> 4 else 5")
2)

(check-equal? (run "| true -> 2 | 3 -> 4 else 5")
2)

(check-equal? (run "| false -> 2 | 3 -> 4 else 5")
4)

(check-equal? (run "| false -> 2 | 1 + 2 = 1 -> 4 else 5")
5)

(check-equal? (run "1 + | bool 0 -> 3 else 4")
5)

(check-equal? (run "1 + (| bool 0 -> 3 else 4)")
5)

(check-equal? (run "(| bool 0 -> 2 else 3) + 5")
8)

(check-equal? (run "(| bool 1 -> 2 else 3) + 5")
7)

(check-equal? (run "| bool 0 -> 2 else 3 + 5")
8)
