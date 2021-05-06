#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "def x := match 5 | 5 -> 2 end")
2)

(check-equal? (run "x")
2)


(run-silent "def x := lam n := match n | 5 -> 3 end")

(check-equal? (run "x:5")
3)
(check-exn exn:fail? (lambda ()
  (run "x:4")))
(check-exn exn:fail? (lambda ()
  (run "x:(4,2)")))

(run-silent "def x := lam n := match n | 5 -> 3 | 7 -> 1 end")

(check-equal? (run "x:5")
3)
(check-equal? (run "x:7")
1)
(check-exn exn:fail? (lambda ()
  (run "x:3")))
(check-exn exn:fail? (lambda ()
  (run "x:\"6h\"")))

(check-equal? (run "(match 5 | 5 -> 2 end) + 1")
3)

(check-exn exn:fail? (lambda ()
  (run "def x := match 5 | 6 -> 2 end")))


;; variable time

(check-equal? (run "match 5 | x -> 7 end")
7)
(check-equal? (run "match 5 | x -> x+1 end")
6)
(check-exn exn:fail? (lambda ()
  (run "match 5 | x,y -> 2 end")))
(check-exn exn:fail? (lambda ()
  (run "match 5 | x,5 -> 2 end")))
(check-exn exn:fail? (lambda ()
  (run "match 5 | x -> x + 0.6 end")))
(check-exn exn:fail? (lambda ()
  (run "match 5 | float x -> x + 0.6 end")))

(check-equal? (run "match 5.0 | float x -> x + 0.6 end")
5.6)
(check-equal? (run "match 5.0 | float x -> x + 0.6 | 3 -> 2 end")
5.6)
(check-equal? (run "match 5.0 | 3 -> 1 | 5.0 -> 2 end")
2)

(check-equal? (run "string match 5.0,3 | float x,y -> y,x end")
"(3, 5.0)")
(check-equal? (run "string match 5.0,3; | float x,y; -> y,x end")
"(3, 5.0)")
(check-exn exn:fail? (lambda ()
  (run "string match 5.0,3 | float x, bool y -> y,x end")))
(check-equal? (run "string match 5.0,3 | float x, bool y -> y,x | x, y -> \"it worked\" end")
"it worked")

(check-equal? (run "match 5.0,3; | a, b -> a end")
5.0)
(check-equal? (run "string match 5.0,3; | a, b -> b end")
"(3;)")

(check-equal? (run "match 5.0,3; | a, b when a = 5.0 -> a | _ -> 2 end")
5.0)
(check-equal? (run "match 5.0,3; | a, b when a = 5.1 -> a | _ -> 2 end")
2)
(check-equal? (run "match 5.0,3; | a, b when typeof a = \"float\" -> a | _ -> 2 end")
5.0)
(check-equal? (run "match 5.0,3; | a, b when typeof a = \"int\" -> a | _ -> 2 end")
2)

(check-equal? (run "match 5.0,3 | _, b -> b | a, b -> 2 end")
3)

(check-equal? (run "match 5.0,3,4 | a, _, c -> c | a, b -> 2 end")
4)

(check-equal? (run "match 5.0, (3, 6), 4 | a, (b, c), d -> b + c | a, b -> 2 end")
9)



(check-equal? (run "let v := 5 in match 1 | _ -> v end")
5)
(check-equal? (run "let v := 5 in let b := true in match 1 | _ when b -> v end")
5)
