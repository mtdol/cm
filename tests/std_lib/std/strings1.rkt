#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; string basics
;;

(check-equal? (run "substring:\"abc\":0:1")
"a")

(check-equal? (run "substring:\"abc\":0:2")
"ab")

(check-equal? (run "substring:\"abc\":1:3")
"bc")

(check-equal? (run "string_ref:\"abc\":1")
"b")

(check-equal? (run "concatenate:\"abc\":\"de\"")
"abcde")

(check-equal? (run "string_length:\"abc\"")
3)

(check-equal? (run "string_length:\"\"")
0)

(check-equal? (run "string_to_list:\"abc\"")
'("a" "b" "c"))

(check-equal? (run "string_to_list:\"a\"")
'("a"))

(check-equal? (run "string_to_list:\"\"")
'())

(check-equal? (run "list_to_string:{list \"a\"|\"b\"|\"c\"}")
"abc")

(check-equal? (run "list_to_string:{list}")
"")

(check-equal? (run "list_to_string:{list \"ab\"|\"c\"}")
"abc")

(check-equal? (run "let cs := string_to_list:\"abc\" in list_to_string:cs")
"abc")
