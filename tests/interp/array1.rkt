#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "#:lang cm")
(run-silent "def a := make_array : 3")

(check-equal? (run "a")
(vector 0 0 0))


(check-equal? (run "array_length : a")
3)
(check-equal? (run "length a")
3)

(check-equal? (run "array_ref : a : 0")
0)
(check-equal? (run "array_ref : a : 1")
0)
(check-equal? (run "array_ref : a : 2")
0)

(check-equal? (run "a::0")
0)
(check-equal? (run "a::1")
0)
(check-equal? (run "a::2")
0)

(check-failure run "array_ref : a : 3")
(check-failure run "array_ref : a : -1")
(check-failure run "a::3")
(check-failure run "a::-1")
(check-failure run "a::(0,4;)")
(check-failure run "a::(-1,2;)")

(run-silent "array_set : a : 0 : \"a\"")
(run-silent "array_set : a : 1 : \"b\"")
(run-silent "array_set : a : 2 : \"c\"")

(check-equal? (run "a::0")
"a")
(check-equal? (run "a::1")
"b")
(check-equal? (run "a::2")
"c")

(check-equal? (run "a")
(vector "a" "b" "c"))

(check-equal? (run "a::(0,2;)")
(vector "a" "b"))

(check-equal? (run "a::(1,3;)")
(vector "b" "c"))

(check-failure run "array_set : a : 3 : \"d\"")

;;
;; array_to_list, list_to_array
;;

(check-equal? (run "array_to_list : a")
(list "a" "b" "c"))

(check-equal? (run "array_to_list : (make_array : 2)")
(list 0 0))

(check-equal? (run "array_to_list : (make_array : 0)")
(list))

(check-equal? (run "def a2 := list_to_array : {list \"a\"|\"b\"|\"c\"}")
(vector "a" "b" "c"))

(check-equal? (run "list_to_array : {list}")
(vector))

;;
;; equality
;;

(check-equal? (run "list_to_array : {list \"a\"|\"b\"|\"c\"} ==
                      list_to_array : {list \"a\"|\"b\"|\"c\"}")
val-true)

(check-equal? (run "list_to_array : {list \"b\"|\"c\"} ==
                      list_to_array : {list \"a\"|\"b\"|\"c\"}")
val-false)

(check-equal? (run "list_to_array : {list} ==
                      list_to_array : {list \"a\"|\"b\"|\"c\"}")
val-false)

(check-equal? (run "list_to_array : {list} ==
                      list_to_array : {list}")
val-true)
