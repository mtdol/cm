#lang racket
(require cm/tests/test-utils rackunit)

;; check for things that should fail

(check-failure run "typedef S := 3")

(check-failure run "typedef S := 3;")

(check-failure run "typedef S := a")

(check-failure run "typedef S := int float a;")

(check-failure run "typedef S a;")

(check-failure run "typedef := a;")

;; cannot have a duplicate id in schema
(check-failure run "typedef := int a, float a;")
(check-failure run "typedef := a,b,a;")

(run-silent "typedef S := a;")

(check-failure run "Struct S (3;)")

(check-failure run "struct S := (3;)")

(check-failure run "struct S 3")

(check-failure run "struct s (3;)")

(run-silent "typedef St2 := int a, int b;")
(run-silent "typedef St1 := int a,  (struct St2 st);")

(check-failure run "def struct St2 s2 := struct S 4;")

(check-failure run "def struct St2 s2 := 4")
