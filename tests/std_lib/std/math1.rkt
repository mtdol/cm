#lang racket
(require cm/tests/test-utils rackunit)

;; import macros
(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; Basic ops
;;

(check-equal? (run "string (number?:3)")
"true")

(check-equal? (run "string (number?:1.2)")
"true")

(check-equal? (run "string (number?:\"3\")")
"false")

(check-equal? (run "string (number?:true)")
"false")

(check-equal? (run "string (number?:(lambda x := x))")
"false")


(check-equal? (run "add:3:4")
7)

(check-exn exn:fail? (lambda ()
  (run "add:3:4.2")))

(check-equal? (run "sub:3:4")
-1)

(check-exn exn:fail? (lambda ()
  (run "sub:3:4.2")))

(check-equal? (run "mult:3:4")
12)

(check-exn exn:fail? (lambda ()
  (run "mult:3:4.2")))

(check-equal? (run "div:6.0:2.0")
3.0)

(check-exn exn:fail? (lambda ()
  (run "div:3:4.2")))

(check-exn exn:fail? (lambda ()
  (run "div:3:4")))

(check-equal? (run "exp:2:3")
8)

(check-equal? (run "exp:2.0:3.0")
8.0)

(check-exn exn:fail? (lambda ()
  (run "exp:3:4.2")))

(check-equal? (run "add1:3")
4)

(check-equal? (run "add1:-3")
-2)

(check-equal? (run "add1:-3.2")
-2.2)

(check-exn exn:fail? (lambda ()
  (run "add1:\"a\"")))

(check-equal? (run "sub1:3")
2)

(check-equal? (run "sub1:-3")
-4)

(check-equal? (run "sub1:-3.2")
-4.2)

(check-exn exn:fail? (lambda ()
  (run "sub1:\"a\"")))

(check-equal? (run "string (zero?:3.2)")
"false")

(check-equal? (run "string (zero?:3)")
"false")

(check-equal? (run "string (zero?:0)")
"true")

(check-equal? (run "string (zero?:0.0)")
"true")

(check-exn exn:fail? (lambda ()
  (run "zero?:\"a\"")))

(check-equal? (run "string (one?:3.2)")
"false")

(check-equal? (run "string (one?:3)")
"false")

(check-equal? (run "string (one?:0)")
"false")

(check-equal? (run "string (one?:1)")
"true")

(check-equal? (run "string (one?:1.0)")
"true")

(check-exn exn:fail? (lambda ()
  (run "one?:\"a\"")))

(check-equal? (run "string (pos?:3.2)")
"true")

(check-equal? (run "string (pos?:3)")
"true")

(check-equal? (run "string (pos?:-3.2)")
"false")

(check-equal? (run "string (pos?:0)")
"false")

(check-exn exn:fail? (lambda ()
  (run "pos?:\"a\"")))

(check-equal? (run "string (neg?:3.2)")
"false")

(check-equal? (run "string (neg?:3)")
"false")

(check-equal? (run "string (neg?:-3.2)")
"true")

(check-equal? (run "string (neg?:0)")
"false")

(check-exn exn:fail? (lambda ()
  (run "neg?:\"a\"")))

(check-equal? (run "string (odd?:2)")
"false")

(check-equal? (run "string (odd?:3)")
"true")

(check-equal? (run "string (odd?:-3)")
"true")

(check-equal? (run "string (odd?:0)")
"false")

(check-exn exn:fail? (lambda ()
  (run "odd?:\"a\"")))

(check-exn exn:fail? (lambda ()
  (run "odd?:3.2")))

(check-equal? (run "string (even?:2)")
"true")

(check-equal? (run "string (even?:3)")
"false")

(check-equal? (run "string (even?:-3)")
"false")

(check-equal? (run "string (even?:0)")
"true")

(check-exn exn:fail? (lambda ()
  (run "odd?:\"a\"")))

(check-exn exn:fail? (lambda ()
  (run "odd?:3.2")))

(check-equal? (run "value:\"s\"")
"s")

(check-equal? (run "type (value:(lambda x := x))")
"fun")

(check-equal? (run "value:3")
3)

(check-equal? (run "value:3.2")
3.2)

(check-equal? (run "string (value:true)")
"true")

(check-equal? (run "to_int:\"1\"")
1)

(check-equal? (run "to_int:1")
1)

(check-equal? (run "to_int:1.1")
1)

(check-equal? (run "to_int:true")
1)

(check-exn exn:fail? (lambda ()
  (run "to_int:\"s\"")))

(check-exn exn:fail? (lambda ()
  (run "to_int:(lambda x := x)")))

(check-equal? (run "to_float:\"1\"")
1.0)

(check-equal? (run "to_float:1")
1.0)

(check-equal? (run "to_float:1.1")
1.1)

(check-equal? (run "to_float:true")
1.0)

(check-exn exn:fail? (lambda ()
  (run "to_float:\"s\"")))

(check-exn exn:fail? (lambda ()
  (run "to_float:(lambda x := x)")))

(check-equal? (run "to_string:\"1\"")
"1")

(check-equal? (run "to_string:\"s\"")
"s")

(check-equal? (run "to_string:1")
"1")

(check-equal? (run "to_string:1.1")
"1.1")

(check-equal? (run "to_string:true")
"true")

(check-equal? (run "string (to_bool:\"1\")")
"true")

(check-equal? (run "string (to_bool:1)")
"true")

(check-equal? (run "string (to_bool:1.1)")
"true")

(check-equal? (run "string (to_bool:0)")
"false")

(check-equal? (run "string (to_bool:0.0)")
"false")

(check-equal? (run "string (to_bool:true)")
"true")

(check-exn exn:fail? (lambda ()
  (run "to_bool:(lambda x := x)")))

(check-equal? (run "car:(3,2)")
3)

(check-equal? (run "car:(3,2;)")
3)

(check-exn exn:fail? (lambda ()
  (run "car:3")))

(check-equal? (run "cdr:(3,2)")
2)

(check-equal? (run "cdr:(3,2;)")
'(2))

(check-exn exn:fail? (lambda ()
  (run "cdr:3")))
