#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (output run "@ 3")
"3\n")

(check-equal? (output run "write_string \"3\"")
"3")

(check-equal? (output run "write_string \"3\n4\"")
"3\n4")

(check-failure-args output (list run "write_string 3"))

(check-equal? (input run "read_line" "123\n56")
"123")

(check-equal? (input run "read_line" "123\n")
"123")

(check-equal? (input run "read_string 3" "12345")
"123")

(check-equal? (input run "read_string 0" "12345")
"")

(check-failure-args input (list run "read_string -1" "12345"))
