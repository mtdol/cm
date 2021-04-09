#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; list_to_hash  
;;

(check-equal? (run "string def x := {: list_to_hash | {list 1,3|\"a\",7}}")
"immutable hash")

(check-equal? (run "x::1")
3)

(check-equal? (run "x::\"a\"")
7)

(check-equal? (run "string def x := {: list_to_mutable_hash | {list 1,3|\"a\",7}}")
"mutable hash")

(check-equal? (run "x::1")
3)

(check-equal? (run "x::\"a\"")
7)

(run-silent "hash_set x 1 \"v\"")

(check-equal? (run "x::1")
"v")

(check-equal? (run "x::\"a\"")
7)

(check-equal? (run "string def x := {: list_to_hash | {list}}")
"immutable hash")

(check-exn exn:fail? (lambda ()
  (run "x::1")))
