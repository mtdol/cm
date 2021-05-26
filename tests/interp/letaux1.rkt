#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? 
  (run "letaux f 
       | x := 5
       in
         x + 1")
6)

(check-equal? 
  (run "letaux f 
       | x := 100
       in
         | x = 0 -> 0
         else 2 + f:(x-1)")
200)

;; recursive, multiple args
(check-equal? 
  (run "letaux f 
       | int x := 4 
       -- check if works with guard
       | ? y (\\n -> n > 0) := 3 
       in 
         | x = 0 -> 0 - y
         else
           (x - y) + (f : (x - 1) : (y + 1))")
(+ (- 4 3) (- 3 4) (- 2 5) (- 1 6) (- 0 7)))

;; same as before but with different guard
(check-failure
  run "letaux f 
       | int x := 4 
       -- should only work initially, not when y is incremented
       | ? y (\\n -> n % 2 = 1) := 3 
       in 
         | x = 0 -> 0 - y
         else
           (x - y) + (f : (x - 1) : (y + 1))")

(check-failure 
  run "letaux \"f\" | x := 3 in x") 

(check-failure 
  run "letaux f | x -> 3 in x") 

(check-failure 
  run "letaux f := | x := 3 in x") 

(check-failure 
  run "letaux f | x -> 3 else x") 

(check-failure 
  run "letaux f | x -> 3 x") 

(check-failure 
  run "letaux f x -> 3 in x") 

(check-failure 
  run "letaux f 2") 
