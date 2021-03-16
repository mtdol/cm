#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "typedef S := types (\"int\", \"float\";) a, types (\"string\", \"dynamic\";) b;")

(check-equal? (run "string struct S (4.2, 7;)")
"(struct S (4.2, 7;))")
(check-equal? (run "string struct S (4, 7;)")
"(struct S (4, 7;))")
(check-equal? (run "string struct S (4, true;)")
"(struct S (4, true;))")

(check-exn exn:fail? (lambda ()
  (run "string struct S (true, 7;)")))
