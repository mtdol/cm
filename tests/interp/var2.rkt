#lang racket
(require cm/core/types)
(require cm/tests/test-utils rackunit)

(run-file-silent "files/var2/a.cm")

(check-equal? (run "_a")
3)

(check-equal? (run "var \"%b\"")
5)

;; check long names

;; just long enough
(run-stat-silent 
  (format "def s := \"\". def x := 0. while x < ~a do (set s := s $ \"s\") comma set x := x + 1."
        (get-max-var-length)))

;; 1 too long
(run-stat-silent 
  "def s_long := s $ \"s\".")

(check-equal? (run "def var s := 1")
1)

(check-equal? (run "var s")
1)

(check-exn exn:fail? (lambda ()
  (run "def var s_long := 1")))

;; check typedef
(run-silent 
  "typedef S := int var \"%a\", b, var \"c %\", types (\"int\", \"float\";) var \"d\";")

(check-equal? (run "string struct S (1, 2, 3.2, 4;)")
"(struct S (1, 2, 3.2, 4;))")

(check-exn exn:fail? (lambda ()
  (run "struct S (1.2, 2, 3, 4.2;)")))

(check-exn exn:fail? (lambda ()
  (run "struct S (1, 2, 3, \"4.2\";)")))
