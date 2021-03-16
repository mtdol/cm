#lang racket
(require cm/tests/test-utils rackunit)

(run "typedef St2 := int a, int b;")
(run "typedef St1 := int a,  (struct St2 st);")
(check-equal? (run "string def s2 := struct St2 (4,5;)")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2")
"(struct St2 (4, 5;))")

(check-equal? (run "string def s1 := struct St1 (7, (struct St2 (5,6;));)")
"(struct St1 (7, (struct St2 (5, 6;));))")
(check-equal? (run "string s1")
"(struct St1 (7, (struct St2 (5, 6;));))")

(check-equal? (run "string struct? St2 s1")
"false")
(check-equal? (run "string struct? St1 s1")
"true")
(check-equal? (run "string struct? St2 s2")
"true")
(check-equal? (run "string struct? St1 s2")
"false")
(check-equal? (run "string struct? St1 struct St2 (4,5;)")
"false")
(check-equal? (run "string struct? St1 struct St1 (7, (struct St2 (4,5;));)")
"true")

(run "typedef St := a, int b;")
(check-equal? (run "string struct St (4,5;)")
"(struct St (4, 5;))")
(check-equal? (run "string struct St (4.3,5;)")
"(struct St (4.3, 5;))")

