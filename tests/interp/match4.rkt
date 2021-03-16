#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "match 3,3 | a,a -> a end")
3)

(check-equal? (run "match 3,3 | a, int a -> a end")
3)

(check-equal? (run "match 3,3 | int a, a -> a end")
3)

(check-equal? (run "match 3,4 | a, a -> a | a,b -> a + b end")
7)

(check-exn exn:fail? (lambda ()
  (run "match 3,4 | a,a -> a end")))

(check-exn exn:fail? (lambda ()
  (run "match 3,4 | a, int a -> a end")))

(check-exn exn:fail? (lambda ()
  (run "match 3,4 | int a, a -> a end")))


(check-equal? (run "match 3,3 | a,b -> a + b end")
6)


(check-equal? (run "match null | a, a -> a | null -> 5 end")
5)

(check-equal? (run "match void | a, a -> a | void -> 5 end")
5)
