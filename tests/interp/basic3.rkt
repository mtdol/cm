#lang racket
(require cm/tests/test-utils rackunit)

;;
;; ++
;;

(check-equal? (run "() ++ ()")
'())

(check-equal? (run "() ++ (1;)")
'(1))

(check-equal? (run "(1,2;) ++ (3;)")
'(1 2 3))

(check-equal? (run "(1,2;) ++ ((3,4;);)")
'(1 2 (3 4)))

(check-equal? (run "(1,(2,3;);) ++ (4,(5,6;);)")
'(1 (2 3) 4 (5 6)))

(check-equal? (run "(1,((2;) ++ (3,4;));) ++ (5;)")
'(1 (2 3 4) 5))

(check-failure run "1 ++ 2")

(check-failure run "1 ++ (2;)")

(check-failure run "(1;) ++ 2")
