#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "#:lang cm")

;; reverse_array

(run-silent "def a := list_to_array:{list 1|2|3}")
(run-silent "def b := list_to_array:{list 1|2|3|4}")

(check-equal? (run "reverse_array : a")
val-void)
(check-equal? (run "reverse_array : b")
val-void)

(check-equal? (run "a::0")
3)
(check-equal? (run "a::1")
2)
(check-equal? (run "a::2")
1)

(check-equal? (run "b::0")
4)
(check-equal? (run "b::1")
3)
(check-equal? (run "b::2")
2)
(check-equal? (run "b::3")
1)

;; array_map

(run-silent "def a := list_to_array:{list 1|2|3}")

(check-equal? (run "array_map : add1 : a")
val-void)

(check-equal? (run "a::0")
2)
(check-equal? (run "a::1")
3)
(check-equal? (run "a::2")
4)

(check-equal? (run "array_map : add1 : (make_array : 0)")
val-void)
