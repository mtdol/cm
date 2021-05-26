#lang racket
(require cm/tests/test-utils rackunit)

;; test the `?` operator in struct schemas

(run-silent "defun odd? x := x % 2 = 1")
(run-silent "typedef S := ? a odd?, ? b (\\x -> not (odd?:x));")
;; invalid function for `?`
(run-silent "typedef S2 := ? a (\\_ -> 3);")

(check-equal? (run "schemaof S")
'("a" "b"))

(check-equal? (run "string struct S (1,2;)")
"(struct S (1, 2;))")

(check-failure run "struct S (2,2;)")

(check-failure run "struct S (1,3;)")

(check-failure run "struct S (2,3;)")

(check-failure run "struct S2 (1;)")
