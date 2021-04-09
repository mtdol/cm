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

(check-equal? (run "let fs := {: map | add | {list 1|2|3}} in {: map | (lambda x := x:2) | fs}")
'(3 4 5))

(check-exn exn:fail? (lambda ()
  (run "{: map | value | 3}")))

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

(check-exn exn:fail? (lambda ()
  (run "{: min | 3.2| 3}")))

(check-exn exn:fail? (lambda ()
  (run "{: max | 3.2| 3}")))

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

(check-exn exn:fail? (lambda ()
  (run "{: reverse | 3}")))

;;
;; append
;;

(check-equal? (run "{: append | {list {list 1}}}")
'(1))

(check-equal? (run "{: append | {list}}")
'())

(check-equal? (run "{: append | {list {list 1|2} | {list 3|\"4\"}}}")
'(1 2 3 "4"))

(check-exn exn:fail? (lambda ()
  (run "{: append | 3}")))

(check-exn exn:fail? (lambda ()
  (run "{: append | {list {list 3} | 4}}")))

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

(check-exn exn:fail? (lambda ()
  (run "{: range | 3 | -2.2}")))

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

(check-exn exn:fail? (lambda ()
  (run "{: foldl | lambda elem := elem | () | {list 1|2|3}}")))

;;
;; ormap, andmap
;;

(check-equal? (run "string {: ormap | odd? | {list 1|2|3}}")
"true")

(check-equal? (run "string {: ormap | odd? | {list 1|3}}")
"true")

(check-equal? (run "string {: ormap | odd? | {list}}")
"false")

(check-exn exn:fail? (lambda ()
  (run "{: ormap | odd? | 1}")))

(check-exn exn:fail? (lambda ()
  (run "{: ormap | lambda x, y := true | {list 1|2|3}}")))

(check-equal? (run "string {: andmap | odd? | {list 1|2|3}}")
"false")

(check-equal? (run "string {: andmap | odd? | {list 1|3}}")
"true")

(check-equal? (run "string {: andmap | odd? | {list}}")
"true")

(check-exn exn:fail? (lambda ()
  (run "{: andmap | odd? | 1}")))

(check-exn exn:fail? (lambda ()
  (run "{: andmap | lambda x, y := true | {list 1|2|3}}")))

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

(check-exn exn:fail? (lambda ()
  (run "{: member | 3 | 2}")))


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

(check-exn exn:fail? (lambda ()
  (run "{: build_list_range | 3.2 | 2.1 | value}")))
