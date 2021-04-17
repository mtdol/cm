#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; string_replace 
;;

(check-equal? (run "{: string_replace | \"abcdef\" | \"b\" | \"34\" }")
"a34cdef")

(check-equal? (run "{: string_replace | \"abcdef\" | \"bc\" | \"34\" }")
"a34def")

(check-equal? (run "{: string_replace | \"abcdbcef\" | \"bc\" | \"34\" }")
"a34d34ef")

(check-equal? (run "{: string_replace | \"abcdef\" | \"\" | \"~\" }")
"~a~b~c~d~e~f~")

;;
;; format
;;

(check-equal? (run "{: format | \"ab~1cd~2ef\" | {list \"1\"|\"32\"}}")
"ab1cd32ef")

(check-equal? (run "{: format | \"ab~1c~1d~2ef\" | {list \"1\"|\"32\"}}")
"ab1c1d32ef")

(check-equal? (run "{: format | \"a~2b~1c~1d~2ef\" | {list \"1\"|\"32\"}}")
"a32b1c1d32ef")

(check-equal? (run "{: format | \"a~2b~1c~1d~2ef\" | {list \"1\"}}")
"a~2b1c1d~2ef")

(check-equal? (run "{: format | \"\" | {list \"1\"}}")
"")
