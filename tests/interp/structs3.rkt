#lang racket
(require cm/tests/test-utils rackunit)

;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (run "typedef S := 3")))

(check-exn exn:fail? (lambda ()
  (run "typedef S := 3;")))

(check-exn exn:fail? (lambda ()
  (run "typedef S := a")))

(check-exn exn:fail? (lambda ()
  (run "typedef S := int float a;")))

(check-exn exn:fail? (lambda ()
  (run "typedef S a;")))

(check-exn exn:fail? (lambda ()
  (run "typedef := a;")))


(run-silent "typedef S := a;")

(check-exn exn:fail? (lambda ()
  (run "Struct S (3;)")))

(check-exn exn:fail? (lambda ()
  (run "struct S := (3;)")))

(check-exn exn:fail? (lambda ()
  (run "struct S 3")))

(check-exn exn:fail? (lambda ()
  (run "struct s (3;)")))

(run-silent "typedef St2 := int a, int b;")
(run-silent "typedef St1 := int a,  (struct St2 st);")

(check-exn exn:fail? (lambda ()
  (run "def struct St2 s2 := struct S 4;")))

(check-exn exn:fail? (lambda ()
  (run "def struct St2 s2 := 4")))
