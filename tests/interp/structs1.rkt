#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "typedef St2 := int a, int b;")
(run-silent "typedef St1 := int a,  (struct St2 st);")
(run-silent "typedef St0 := ()")
(run-silent "typedef St3 := int var \"a\";")

(check-equal? (run "schemaof St2")
'("a" "b"))

(check-equal? (run "schemaof St1")
'("a" "st"))

(check-equal? (run "schemaof St0")
'())

(check-equal? (run "schemaof St3")
'("a"))

;; does not exist
(check-failure run "schemaof St4")

(check-equal? (run "string def s2 := struct St2 (4,5;)")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2")
"(struct St2 (4, 5;))")

(check-equal? (run "string def s1 := struct St1 (7, (struct St2 (5,6;));)")
"(struct St1 (7, (struct St2 (5, 6;));))")
(check-equal? (run "string s1")
"(struct St1 (7, (struct St2 (5, 6;));))")

(check-equal? (run "string def s3 := struct St3 (2;)")
"(struct St3 (2;))")
(check-equal? (run "string s3")
"(struct St3 (2;))")

(check-equal? (run "string def s0 := struct St0 ()")
"(struct St0 ())")
(check-equal? (run "string s0")
"(struct St0 ())")

(check-equal? (run "struct? St2 s1")
val-false)
(check-equal? (run "struct? St1 s1")
val-true)
(check-equal? (run "struct? St2 s2")
val-true)
(check-equal? (run "struct? St1 s2")
val-false)
(check-equal? (run "struct? St1 struct St2 (4,5;)")
val-false)
(check-equal? (run "struct? St1 struct St1 (7, (struct St2 (4,5;));)")
val-true)
(check-equal? (run "struct? St3 s2")
val-false)
(check-equal? (run "struct? St3 s3")
val-true)
(check-equal? (run "struct? St3 s0")
val-false)
(check-equal? (run "struct? St0 s3")
val-false)
(check-equal? (run "struct? St0 s0")
val-true)

(check-equal? (run "struct? _ s1")
val-true)

(check-equal? (run "struct? _ s2")
val-true)

(check-equal? (run "struct? _ s3")
val-true)

(check-equal? (run "struct? _ s0")
val-true)

(check-equal? (run "struct? _ struct St2 (4,5;)")
val-true)

(check-equal? (run "struct? _ 4")
val-false)

(run-silent "typedef St := a, int b;")
(check-equal? (run "string struct St (4,5;)")
"(struct St (4, 5;))")
(check-equal? (run "string struct St (4.3,5;)")
"(struct St (4.3, 5;))")

