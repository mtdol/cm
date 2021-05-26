#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; iterator basics
;;

(check-equal? (run "string :>iter_open?")
"false")

(check-equal? (run "string (iter:add1)")
"eof")

(check-equal? (run ":>iter_get")
0)

;; init
(check-equal? (run "iter_init:2")
2)

(check-equal? (run "string :>iter_open?")
"true")

(check-equal? (run ":>iter_get")
2)

(check-equal? (run "iter:add1")
3)

(check-equal? (run "iter:add1")
4)

(check-equal? (run ":>iter_get")
4)

(check-equal? (run "iter:sub1")
3)

(check-equal? (run ":>iter_get")
3)

(check-equal? (run "iter_set:7")
7)

(check-equal? (run ":>iter_get")
7)

(check-equal? (run "iter:sub1")
6)

(check-equal? (run ":>iter_get")
6)

(check-equal? (run "string :>iter_term")
"eof")

(run-stat-silent 
      "(def x := 0) comma 
       while {iter :>(\\ () ->
            \\| not :>iter_open? -> iter_init:2
        else 
            \\| :>iter_get < 5 -> iter:add1
            else :>iter_term)} do set x := x + :>iter_get//")

(check-equal? (run "x")
(+ 2 3 4 5))

;;
;; iter_range
;;

;; this change should be lost when we run the range iter
(check-equal? (run "iter_set:7")
7)

(run-stat-silent 
  "(def x := 0) comma 
  while {iter {: iter_range | 2 | 6}} do
    set x := x + :>iter_get//")

(check-equal? (run "x")
(+ 2 3 4 5))

(run-stat-silent 
  "(def x := 0) comma 
  while {iter {: iter_range | -2 | 3}} do
    set x := x + :>iter_get//")

(check-equal? (run "x")
(+ -2 -1 0 1 2))

;;
;; gen_undefined
;;

(check-equal? (run "gen_undefined:\"add\":{->module_id \"std_lib::std.cm\"}")
"add0")

;; gens `add2` because there is an add1 func but not an add2 in std.cm
(check-equal? (run "gen_undefined:\"add\":{->module_id \"std_lib::std.cm\"}")
"add2")

(check-equal? (run "gen_undefined:\"a\":{current_module}")
"a3")

(check-equal? (run "gen_undefined:\"a\":{current_module}")
"a4")

(run-silent "def a5 := ()")

(check-equal? (run "gen_undefined:\"a\":{current_module}")
"a6")
