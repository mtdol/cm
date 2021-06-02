#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; sort
;;

(check-equal? (run "sort : value : {list 3|2|1}")
'(1 2 3))

(check-equal? (run "sort : value : {list 2|3|1}")
'(1 2 3))

(check-equal? (run "sort : value : {list 2}")
'(2))

(check-equal? (run "sort : value : {list}")
'())

(check-failure run "sort : value : 3")

(check-failure run "sort : value : {list \"a\"|\"b\"}")

(check-equal? (run "sort : char_to_int : {list \"a\"|\"b\"}")
'("a" "b"))

(check-equal? (run "sort : char_to_int : {list \"b\"|\"a\"}")
'("a" "b"))

(check-equal? (run "sort : char_to_int : (string_to_list:\"fred\")")
'("d" "e" "f" "r"))

(check-equal? (run "sort : char_to_int : (string_to_list:\"ice-cream\")")
'("-" "a" "c" "c" "e" "e" "i" "m" "r"))

(check-equal? (run "sort : abs : {list 2|3|1}")
'(1 2 3))

(check-equal? (run "sort : abs : {list -2|3|1}")
'(1 -2 3))

;;
;; shuffle
;;

; not a complete test but just makes sure that we get one of the possible values

(define-syntax-rule (check-shuffle v vs)
  (check 
    (lambda (x xs) (member x xs)) 
    v vs))

(check-shuffle (run "shuffle : {list 1|2}")
'((1 2) (2 1)))

(check-shuffle (run "shuffle : {list 1|2|3}")
'((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))

;;
;; sample
;;

; similar to `shuffle`

(check-failure run "sample : -1 : {list 1|2}")

(check-shuffle (run "sample : 0 : {list 1|2}")
'(()))

(check-shuffle (run "sample : 1 : {list 1|2}")
'((1) (2)))

(check-shuffle (run "sample : 2 : {list 1|2}")
'((1 2) (2 1)))

(check-shuffle (run "sample : 2 : {list 1|2|3}")
'((1 2) (1 3) (2 1) (2 3) (3 1) (3 2)))

;;
;; partition
;;

(check-equal? (run "partition : value : {list 1|2|3|4}")
'((1 2 3 4) ()))

(check-equal? (run "partition : odd? : {list 1|2|3|4}")
'((1 3) (2 4)))

(check-equal? (run "partition : odd? : {list 2|4}")
'(() (2 4)))

(check-equal? (run "partition : odd? : {list 1}")
'((1) ()))

(check-equal? (run "partition : odd? : {list}")
'(() ()))
