#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; map
;;

(check-equal? (run "{: map | value | {list 1|2|3}}")
'(1 2 3))

(check-equal? (run "{: map | add1 | {list 1|2|3}}")
'(2 3 4))

(check-equal? (run "{: map | car | {list 1,3|2,4.3|-2,4,1;}}")
'(1 2 -2))

(check-equal? (run "{: map | cdr | {list 1,3|2,4.3|-2,4,1;}}")
'(3 4.3 (4 1)))

(check-equal? (run "let fs := {: map | func.add | {list 1|2|3}} in {: map | (lambda x := x:2) | fs}")
'(3 4 5))

(check-failure run "{: map | value | 3}")

;;
;; filter
;;

(check-equal? (run "{: filter | lambda () := true | {list 1|2|3}}")
'(1 2 3))

(check-equal? (run "{: filter | odd? | {list 1|2|3|4}}")
'(1 3))

(check-equal? (run "{: filter | even? | {list 1|2|3|4}}")
'(2 4))

(check-equal? (run "{: filter | lambda v := odd?:(`v) | {list 1,3;|2,4;|5,1;}}")
'((1 3) (5 1)))

;;
;; min, max
;;

(check-equal? (run "{: min | 3 | 4}")
3)

(check-equal? (run "{: min | 4 | 3}")
3)

(check-equal? (run "{: min | 4.0 | 3.0}")
3.0)

(check-equal? (run "{: max | 3 | 4}")
4)

(check-equal? (run "{: max | 4 | 3}")
4)

(check-equal? (run "{: max | 4.0 | 3.0}")
4.0)

(check-failure run "{: min | 3.2| 3}")

(check-failure run "{: max | 3.2| 3}")

;;
;; minf, maxf
;;

(check-equal? (run "{: minf | value | -3 | 2}")
-3)

(check-equal? (run "{: minf | abs | -3 | 2}")
2)

(check-equal? (run "{: minf | abs | 3 | 2}")
2)

(check-equal? (run "{: minf | abs | -3 | -2}")
-2)

(check-equal? (run "{: maxf | value | -3 | 2}")
2)

(check-equal? (run "{: maxf | abs | -3 | 2}")
-3)

(check-equal? (run "{: maxf | abs | 3 | 2}")
3)

(check-equal? (run "{: maxf | abs | -3 | -2}")
-3)

(check-equal? (run "{: minf | to_int | \"3\"| \"2\"}")
"2")

(check-equal? (run "{: minf | to_int | \"-3\"| \"2\"}")
"-3")

(check-equal? (run "{: maxf | to_int | \"3\"| \"2\"}")
"3")

(check-equal? (run "{: maxf | to_int | \"-3\"| \"2\"}")
"2")

;;
;; reverse
;;

(check-equal? (run "{: reverse | 3;}")
'(3))

(check-equal? (run "{: reverse | ()}")
'())

(check-equal? (run "{: reverse | {list 3|4}}")
'(4 3))

(check-equal? (run "{: reverse | {list \"a\"|4}}")
'(4 "a"))

(check-failure run "{: reverse | 3}")

;;
;; append
;;

(check-equal? (run "{: append | {list {list 1}}}")
'(1))

(check-equal? (run "{: append | {list}}")
'())

(check-equal? (run "{: append | {list {list 1|2} | {list 3|\"4\"}}}")
'(1 2 3 "4"))

(check-failure run "{: append | 3}")

(check-failure run "{: append | {list {list 3} | 4}}")

;;
;; flatten
;;

(check-equal? (run "{: flatten | {list 1}}")
'(1))

(check-equal? (run "{: flatten | {list 1 | 2}}")
'(1 2))

(check-equal? (run "{: flatten | {list}}")
'())

(check-equal? (run "{: flatten | {list {list}}}")
'())

(check-equal? (run "{: flatten | {list {list 1}}}")
'(1))

(check-equal? (run "{: flatten | {list {list 1 | 2} | 3}}")
'(1 2 3))

(check-equal? (run "{: flatten | {list 1 | {list 2 | 3}}}")
'(1 2 3))

(check-equal? (run "{: flatten | {list {list 1} | 2}}")
'(1 2))

(check-equal? (run "{: flatten | {list {list 1 | {list {list 2 | 3}}} | 4}}")
'(1 2 3 4))

;;
;; range
;;

(check-equal? (run "{: range | 3 | 4}")
'(3))

(check-equal? (run "{: range | 3 | 3}")
'())

(check-equal? (run "{: range | 3 | 5}")
'(3 4))

(check-equal? (run "{: range | 3 | 1}")
'(3 2))

(check-equal? (run "{: range | 3 | -2}")
'(3 2 1 0 -1))

(check-failure run "{: range | 3 | -2.2}")

;;
;; foldl, foldr
;;

(check-equal? (run "{: foldl | lambda elem, acc := elem, acc | () | {list 1|2|3}}")
'(3 2 1))

(check-equal? (run "{: foldr | lambda elem, acc := elem, acc | () | {list 1|2|3}}")
'(1 2 3))

(check-equal? (run "{: foldl | lambda elem, acc := elem, acc | () | {list}}")
'())

(check-equal? (run "{: foldl | lambda elem, acc := elem - acc | 0 | {list 1|2|3}}")
(- 3 (- 2 (- 1 0))))

(check-equal? (run "{: foldr | lambda elem, acc := elem - acc | 0 | {list 1|2|3}}")
(- 1 (- 2 (- 3 0))))

(check-failure run "{: foldl | lambda elem := elem | () | {list 1|2|3}}")

;;
;; ormap, andmap
;;

(check-equal? (run "string {: ormap | odd? | {list 1|2|3}}")
"true")

(check-equal? (run "string {: ormap | odd? | {list 1|3}}")
"true")

(check-equal? (run "string {: ormap | odd? | {list}}")
"false")

(check-failure run "{: ormap | odd? | 1}")

(check-failure run "{: ormap | lambda x, y := true | {list 1|2|3}}")

(check-equal? (run "string {: andmap | odd? | {list 1|2|3}}")
"false")

(check-equal? (run "string {: andmap | odd? | {list 1|3}}")
"true")

(check-equal? (run "string {: andmap | odd? | {list}}")
"true")

(check-failure run "{: andmap | odd? | 1}")

(check-failure run "{: andmap | lambda x, y := true | {list 1|2|3}}")

;;
;; member, member?
;;

(check-equal? (run "{: member | 3 | {list 1|2|3}}")
'())

(check-equal? (run "string {: member? | 3 | {list 1|2|3}}")
"true")

(check-equal? (run "string {: member | 4 | {list 1|2|3}}")
"false")

(check-equal? (run "string {: member? | 4 | {list 1|2|3}}")
"false")

(check-equal? (run "{: member | 2 | {list 1|2|3}}")
'(3))

(check-equal? (run "string {: member? | 2 | {list 1|2|3}}")
"true")

(check-equal? (run "string {: member | \"a\" | {list 1|2|3}}")
"false")

(check-equal? (run "string {: member? | \"a\" | {list 1|2|3}}")
"false")

(check-equal? (run "{: member | \"a\" | {list 1|\"a\"|3}}")
'(3))

(check-equal? (run "string {: member? | \"a\" | {list 1|\"a\"|3}}")
"true")

(check-equal? (run "{: member | {list 1|2} | {list 1|{list 1|2}|3}}")
'(3))

(check-equal? (run "string {: member? | {list 1|2} | {list 1|{list 1|2}|3}}")
"true")

(check-failure run "{: member | 3 | 2}")


;;
;; remove
;;

(check-equal? (run "{: remove | 3 | {list 1|2|3}}")
'(1 2))

(check-equal? (run "{: remove | 3 | {list 1|3}}")
'(1))

(check-equal? (run "{: remove | 3 | {list 3|1}}")
'(1))

(check-equal? (run "{: remove | 3 | {list 3|1|3}}")
'(1))

(check-equal? (run "{: remove | 3 | {list 3|1|3|2}}")
'(1 2))

(check-equal? (run "{: remove | 3 | {list 3}}")
'())

(check-equal? (run "{: remove | 3 | {list}}")
'())

(check-equal? (run "{: remove | 3 | {list 2}}")
'(2))

(check-equal? (run "{: remove | 3 | {list 1|2}}")
'(1 2))

(check-equal? (run "{: remove | 3,4 | {list 3,4|1,2}}")
'((1 . 2)))

(run-silent "typedef S := a,b;")

(check-equal? (run "{: remove | struct S {list 1|2} | {list 1,2|\"s\"|struct S {list 1|2}|\"dd\"}}")
'((1 . 2) "s" "dd"))


;;
;; build_list, build_list_range
;;


(check-equal? (run "{: build_list_range | 0 | 1 | value}")
'(0))

(check-equal? (run "{: build_list_range | 0 | 0 | value}")
'())

(check-equal? (run "{: build_list_range | 0 | 3 | value}")
'(0 1 2))

(check-equal? (run "{: build_list | 3 | value}")
'(0 1 2))

(check-equal? (run "{: build_list_range | -2 | 3 | value}")
'(-2 -1 0 1 2))

(check-equal? (run "{: build_list_range | -2 | 3 | add1}")
'(-1 0 1 2 3))

(check-equal? (run "{: build_list_range | 0 | 0 | add1}")
'())

(check-equal? (run "{: build_list | 0 | add1}")
'())

(check-failure run "{: build_list_range | 3.2 | 2.1 | value}")

;;
;; index_where, index_of
;;

(check-equal? (run "{: index_where | {list 0|1|2} | odd?}")
1)

(check-equal? (run "{: index_where | {list 3|1|2} | even?}")
2)

(check-equal? (run "{: index_where | {list 3|1|5} | even?}")
val-false)

(check-equal? (run "{: index_of | {list \"a\"|2|4.3} | 1}")
val-false)

(check-equal? (run "{: index_of | {list \"a\"|2|4.3} | 2}")
1)

(check-equal? (run "{: index_of | {list \"a\"|2|4.3} | \"a\"}")
0)

(check-equal? (run "{: index_of | {list \"a\"|2|4.3} | 4.3}")
2)

;;
;; last
;;

(check-equal? (run "{: last | {list 0|1|2}}")
2)

(check-equal? (run "{: last | {list 0}}")
0)

(check-failure run "{: last | {list}}")

;;
;; list_update, list_set
;;

(check-equal? (run "{: list_update | {list 1|2|3} | {list 1} | add1}")
'(1 3 3))

(check-equal? (run "{: list_update | {list 1|2|3} | {list 0|1} | add1}")
'(2 3 3))

(check-equal? (run "{: list_update | {list 1|2|3} | {list} | add1}")
'(1 2 3))

(check-equal? (run "{: list_update | {list 1|2|3} | {list 4} | add1}")
'(1 2 3))

(check-equal? (run "{: list_update | {list} | {list 0} | add1}")
'())

(check-equal? (run "{: list_set | {list 1|2|3} | {list 1} | 0}")
'(1 0 3))

(check-equal? (run "{: list_set | {list 1|2|3} | {list 0|1} | 0}")
'(0 0 3))

(check-equal? (run "{: list_set | {list 1|2|3} | {list} | 0}")
'(1 2 3))

(check-equal? (run "{: list_set | {list 1|2|3} | {list 4} | 0}")
'(1 2 3))

(check-equal? (run "{: list_set | {list} | {list 0} | 0}")
'())

(check-equal? (run "{: list_set | {list 1|2|3} | {list 1} | \"a\"}")
'(1 "a" 3))

;;
;; count
;;

(check-equal? (run "{: count | {list 1|2} | odd?}")
1)

(check-equal? (run "{: count | {list 1|3} | odd?}")
2)

(check-equal? (run "{: count | {list 2|4} | odd?}")
0)

(check-equal? (run "{: count | {list} | odd?}")
0)

;;
;; duplicates?, remove_duplicates
;;

(check-equal? (run "{: duplicates? | {list 1}}")
val-false)

(check-equal? (run "{: duplicates? | {list 1|2}}")
val-false)

(check-equal? (run "{: duplicates? | {list}}")
val-false)

(check-equal? (run "{: duplicates? | {list 1|1}}")
val-true)

(check-equal? (run "{: duplicates? | {list 1|2|3|2|4}}")
val-true)

(check-equal? (run "{: duplicates? | {list 1|\"a\"|3|\"b\"|\"a\"}}")
val-true)

(check-equal? (run "{: remove_duplicates | {list 1}}")
'(1))

(check-equal? (run "{: remove_duplicates | {list 1|2}}")
'(1 2))

(check-equal? (run "{: remove_duplicates | {list}}")
'())

(check-equal? (run "{: remove_duplicates | {list 1|1}}")
'(1))

(check-equal? (run "{: remove_duplicates | {list 1|2|3|2|4}}")
'(1 2 3 4))

(check-equal? (run "{: remove_duplicates | {list 1|\"a\"|3|\"b\"|\"a\"}}")
'(1 "a" 3 "b"))

(check-equal? (run "{: remove_duplicates | {list 1|\"a\"|3|\"b\"|3|\"a\"}}")
'(1 "a" 3 "b"))
