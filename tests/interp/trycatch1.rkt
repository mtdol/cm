#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "#:lang cm")

(check-equal? (run "try 3 + 4 catch e with 8")
7)

(check-equal? (run "try 3 + 4.0 catch e with 8")
8)

(check-equal? (run "try 3 + 4.0 catch e with match e | struct Error (id,msg;) -> id end")
"CONTRACT")

(check-equal? (run "try id.error : \"MY_ID\" : \"my error\" catch e with match e | struct Error (id,msg;) -> id end")
"MY_ID")

(check-failure run "try 3 + 4.0 catch 6")
(check-failure run "try 3 + 4.0 catch e")
(check-failure run "try 3 + 4.0 catch 9e with 8")
