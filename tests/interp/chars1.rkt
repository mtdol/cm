#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "#:lang cm")

(check-equal? (run "int_to_char : 49")
"1")

(check-equal? (run "char_to_int : \"1\"")
49)

;; must be a char, not 2 chars
(check-failure run "char_to_int : \"12\"")

(check-failure run "char_to_int : 3")

(check-failure run "int_to_char : \"3\"")

(check-failure run "int_to_char : -1")

(check-failure run "int_to_char : 100000000000000000000")

;;今
;; chinese char
(check-equal? (run "int_to_char : 20170")
"今")

(check-equal? (run "char_to_int : \"今\"")
20170)
