#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "4 - 1")
3)

(check-equal? (run "4 - - 1")
5)

(check-equal? (run "+4 - 1")
3)

(check-equal? (run "+4")
4)

(check-equal? (run "-4")
-4)

(check-equal? (run "4 *- 1")
-4)

(check-equal? (run "-4 *- 1")
4)

(check-equal? (run "-4 *- 2")
8)

(check-equal? (run "2 ^ 3")
8)

(check-equal? (run "4.0 / 2.0")
2.0)

(check-equal? (run "4.0 + 2.0")
6.0)

(check-equal? (run "4 + 2")
6)

(check-equal? (run "(true and false)")
val-false)

(check-equal? (run "(true xor false)")
val-true)

(check-equal? (run "(true or false)")
val-true)

(check-equal? (run "(true xor true)")
val-false)

(check-equal? (run "(true or true)")
val-true)

(check-equal? (run "(true and true)")
val-true)

(check-equal? (run "not (true and true)")
val-false)

(check-equal? (run "not true")
val-false)

(check-equal? (run "not false")
val-true)

(check-equal? (run "int 3.2")
3)

(check-equal? (run "int 3.8")
3)

(check-equal? (run "int 3.5")
3)

(check-equal? (run "int 3.5 + 1")
4)

(check-equal? (run "int (3.5 + 1.0)")
4)

(check-equal? (run "int 3")
3)

(check-equal? (run "3 % 2")
1)

(check-equal? (run "string 4")
"4")

(check-equal? (run "string 4.0")
"4.0")

(check-equal? (run "string true")
"true")

(check-equal? (run "2 * (3 + 2)")
10)

(check-equal? (run "(3 + 2) * 2")
10)

(check-equal? (run "null? ()")
val-true)

(check-equal? (run "null? null")
val-true)

(check-equal? (run "null? 3")
val-false)

(check-equal? (run "int? 7")
val-true)

(check-equal? (run "int? \"7\"")
val-false)

(check-equal? (run "int? 7.0")
val-false)

(check-equal? (run "bool? int? 7.0")
val-true)

(check-equal? (run "bool? int 7.0")
val-false)

(check-equal? (run "string? \"int 9.0\"")
val-true)

(check-equal? (run "string? int 9.0")
val-false)

(check-equal? (run "list? int 9.0")
val-false)

(check-equal? (run "list? (int 9.0;)")
val-true)

(check-equal? (run "string (list? int 9.0;)")
"(false;)")

(check-equal? (run "string (list? int 9.0, int 7.3)")
"(false, 7)")

(check-equal? (run "string `(list? int 9.0, int 7.3)")
"false")

(check-equal? (run "string ~(list? int 9.0, int 7.3)")
"7")

(check-equal? (run "string ~(list? int 9.0, int 7.3, \"t6\")")
"(7, t6)")

(check-equal? (run "string `~(list? int 9.0, int 7.3, \"t6\")")
"7")
