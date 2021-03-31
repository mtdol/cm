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

(check-equal? (run "string (true and false)")
"false")

(check-equal? (run "string (true xor false)")
"true")

(check-equal? (run "string (true or false)")
"true")

(check-equal? (run "string (true xor true)")
"false")

(check-equal? (run "string (true or true)")
"true")

(check-equal? (run "string (true and true)")
"true")

(check-equal? (run "string not (true and true)")
"false")

(check-equal? (run "string not true")
"false")

(check-equal? (run "string not false")
"true")

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

(check-equal? (run "string null? ()")
"true")

(check-equal? (run "string null? null")
"true")

(check-equal? (run "string null? 3")
"false")

(check-equal? (run "string int? 7")
"true")

(check-equal? (run "string int? \"7\"")
"false")

(check-equal? (run "string int? 7.0")
"false")

(check-equal? (run "string bool? int? 7.0")
"true")

(check-equal? (run "string bool? int 7.0")
"false")

(check-equal? (run "string string? \"int 9.0\"")
"true")

(check-equal? (run "string string? int 9.0")
"false")

(check-equal? (run "string list? int 9.0")
"false")

(check-equal? (run "string list? (int 9.0;)")
"true")

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
