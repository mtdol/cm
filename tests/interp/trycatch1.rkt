#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "try 3 + 4 catch e with 8")
7)

(check-equal? (run "try 3 + 4.0 catch e with 8")
8)

(check-equal? (run "try 3 + 4.0 catch e with match e | struct Error (id,msg;) -> id end")
"CONTRACT")

(check-equal? (run "try error \"my error\" catch e with match e | struct Error (id,msg;) -> msg end")
"NL:GENERIC: my error")

(check-equal? (run "try error \"MY_ID\",\"my error\"; catch e with match e | struct Error (id,msg;) -> id end")
"MY_ID")

(check-equal? (run "try error \"MY_ID\",\"my error\"; catch e with match e | struct Error (id,msg;) -> msg end")
"NL:MY_ID: my error")

(check-exn exn:fail? (lambda ()
  (run "try 3 + 4.0 catch 6")))
(check-exn exn:fail? (lambda ()
  (run "try 3 + 4.0 catch e")))
(check-exn exn:fail? (lambda ()
  (run "try 3 + 4.0 catch 9e with 8")))

