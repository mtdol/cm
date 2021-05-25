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
