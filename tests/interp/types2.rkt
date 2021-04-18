#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "match 0 | types (\"int\", \"bool\";) a -> bool a end")
val-false)
(check-equal? (run "match true | types (\"int\", \"bool\";) a -> bool a end")
val-true)
(check-exn exn:fail? (lambda ()
  (run "string match 3.7 | types (\"int\", \"bool\";) a -> bool a end")))

(check-equal? (run "string match true, 4.6 | types (\"int\", \"bool\";) a, types (\"float\";) b -> b, a end")
"(4.6, true)")
(check-exn exn:fail? (lambda ()
  (run "string match true, 4 | types (\"int\", \"bool\";) a, types (\"float\";) b -> b, a end")))
