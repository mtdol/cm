#lang racket
(require cm/tests/test-utils rackunit)

;; import macros
(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; Basic ops
;;

(check-equal? (run "number?:3")
val-true)

(check-equal? (run "number?:1.2")
val-true)

(check-equal? (run "number?:\"3\"")
val-false)

(check-equal? (run "number?:true")
val-false)

(check-equal? (run "number?:(lambda x := x)")
val-false)


(check-equal? (run "add:3:4")
7)

(check-failure run "add:3:4.2")

(check-equal? (run "sub:3:4")
-1)

(check-failure run "sub:3:4.2")

(check-equal? (run "mult:3:4")
12)

(check-failure run "mult:3:4.2")

(check-equal? (run "div:6.0:2.0")
3.0)

(check-failure run "div:3:4.2")

(check-failure run "div:3:4")

(check-equal? (run "exp:2:3")
8)

(check-equal? (run "exp:2.0:3.0")
8.0)

(check-failure run "exp:3:4.2")

(check-equal? (run "add1:3")
4)

(check-equal? (run "add1:-3")
-2)

(check-equal? (run "add1:-3.2")
-2.2)

(check-failure run "add1:\"a\"")

(check-equal? (run "sub1:3")
2)

(check-equal? (run "sub1:-3")
-4)

(check-equal? (run "sub1:-3.2")
-4.2)

(check-failure run "sub1:\"a\"")

(check-equal? (run "zero?:3.2")
val-false)

(check-equal? (run "zero?:3")
val-false)

(check-equal? (run "zero?:0")
val-true)

(check-equal? (run "zero?:0.0")
val-true)

(check-failure run "zero?:\"a\"")

(check-equal? (run "one?:3.2")
val-false)

(check-equal? (run "one?:3")
val-false)

(check-equal? (run "one?:0")
val-false)

(check-equal? (run "one?:1")
val-true)

(check-equal? (run "one?:1.0")
val-true)

(check-failure run "one?:\"a\"")

(check-equal? (run "pos?:3.2")
val-true)

(check-equal? (run "pos?:3")
val-true)

(check-equal? (run "pos?:-3.2")
val-false)

(check-equal? (run "pos?:0")
val-false)

(check-failure run "pos?:\"a\"")

(check-equal? (run "neg?:3.2")
val-false)

(check-equal? (run "neg?:3")
val-false)

(check-equal? (run "neg?:-3.2")
val-true)

(check-equal? (run "neg?:0")
val-false)

(check-failure run "neg?:\"a\"")

(check-equal? (run "odd?:2")
val-false)

(check-equal? (run "odd?:3")
val-true)

(check-equal? (run "odd?:-3")
val-true)

(check-equal? (run "odd?:0")
val-false)

(check-failure run "odd?:\"a\"")

(check-failure run "odd?:3.2")

(check-equal? (run "even?:2")
val-true)

(check-equal? (run "even?:3")
val-false)

(check-equal? (run "even?:-3")
val-false)

(check-equal? (run "even?:0")
val-true)

(check-failure run "odd?:\"a\"")

(check-failure run "odd?:3.2")


(check-equal? (run "is_int?:3")
val-true)

(check-equal? (run "is_int?:3.2")
val-false)

(check-equal? (run "is_float?:3.2")
val-true)

(check-equal? (run "is_float?:3")
val-false)

(check-equal? (run "is_string?:\"3.2\"")
val-true)

(check-equal? (run "is_string?:3")
val-false)

(check-equal? (run "is_bool?:false")
val-true)

(check-equal? (run "is_bool?:3")
val-false)

(check-equal? (run "is_list?:()")
val-true)

(check-equal? (run "is_list?:3")
val-false)

(check-equal? (run "is_pair?:(1,2)")
val-true)

(check-equal? (run "is_pair?:3")
val-false)

(check-equal? (run "is_void?:void")
val-true)

(check-equal? (run "is_void?:3")
val-false)

(check-equal? (run "is_eof?:eof")
val-true)

(check-equal? (run "is_eof?:3")
val-false)

(check-equal? (run "is_fun?:add1")
val-true)

(check-equal? (run "is_fun?:3")
val-false)

(run-silent "typedef S := a,b;")

(check-equal? (run "is_struct?:\"S\":(struct S (1,2;))")
val-true)

(check-equal? (run "is_struct?:\"S\":3")
val-false)


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

(check-failure run "to_int:\"s\"")

(check-failure run "to_int:(lambda x := x)")

(check-equal? (run "to_float:\"1\"")
1.0)

(check-equal? (run "to_float:1")
1.0)

(check-equal? (run "to_float:1.1")
1.1)

(check-equal? (run "to_float:true")
1.0)

(check-failure run "to_float:\"s\"")

(check-failure run "to_float:(lambda x := x)")

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

(check-equal? (run "to_bool:\"1\"")
val-true)

(check-equal? (run "to_bool:1")
val-true)

(check-equal? (run "to_bool:1.1")
val-true)

(check-equal? (run "to_bool:0")
val-false)

(check-equal? (run "to_bool:0.0")
val-false)

(check-equal? (run "to_bool:true")
val-true)

(check-failure run "to_bool:(lambda x := x)")

(check-equal? (run "car:(3,2)")
3)

(check-equal? (run "car:(3,2;)")
3)

(check-failure run "car:3")

(check-equal? (run "cdr:(3,2)")
2)

(check-equal? (run "cdr:(3,2;)")
'(2))

(check-failure run "cdr:3")

;;
;; list type funcs
;;

(check-equal? (run "homogeneous_list?:{list}")
val-true)

(check-equal? (run "homogeneous_list?:{list 1}")
val-true)

(check-equal? (run "homogeneous_list?:{list 1|2}")
val-true)

(check-equal? (run "homogeneous_list?:{list 1|\"a\"}")
val-false)


(check-equal? (run "list_of_type?:{list}:\"int\"")
val-true)

(check-equal? (run "list_of_type?:{list 1}:\"int\"")
val-true)

(check-equal? (run "list_of_type?:{list 1|2}:\"int\"")
val-true)

(check-equal? (run "list_of_type?:{list 1|2}:\"string\"")
val-false)

(check-equal? (run "list_of_type?:{list 1|2.1}:\"int\"")
val-false)

(check-equal? (run "list_of_type?:{list 1|2.1}:\"float\"")
val-false)


(check-equal? (run "int_list?:{list}")
val-true)

(check-equal? (run "int_list?:{list 1}")
val-true)

(check-equal? (run "int_list?:{list 1|2}")
val-true)

(check-equal? (run "int_list?:{list 1|2.1}")
val-false)

(check-equal? (run "float_list?:{list}")
val-true)

(check-equal? (run "float_list?:{list 1.0}")
val-true)

(check-equal? (run "float_list?:{list 1.0|2.0}")
val-true)

(check-equal? (run "float_list?:{list 1.0|2}")
val-false)

(check-equal? (run "string_list?:{list}")
val-true)

(check-equal? (run "string_list?:{list \"1.0\"}")
val-true)

(check-equal? (run "string_list?:{list \"1.0\"|\"2.0\"}")
val-true)

(check-equal? (run "string_list?:{list \"a\"|2}")
val-false)

(check-equal? (run "bool_list?:{list}")
val-true)

(check-equal? (run "bool_list?:{list false}")
val-true)

(check-equal? (run "bool_list?:{list true|false}")
val-true)

(check-equal? (run "bool_list?:{list true|2}")
val-false)

(check-equal? (run "fun_list?:{list}")
val-true)

(check-equal? (run "fun_list?:{list add1}")
val-true)

(check-equal? (run "fun_list?:{list add1|sub1}")
val-true)

(check-equal? (run "fun_list?:{list add1|2}")
val-false)
