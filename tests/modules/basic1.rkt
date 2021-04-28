#lang racket
(require cm/tests/test-utils rackunit)

(run-file-silent "files/basic1/a.cm")

;; checks that a.cm only has access to the components that it is allowed

(check-equal? (run-stat ":>a_b//")
'(4))

(check-equal? (run-stat ":>a_c//")
'("failure"))

(check-equal? (run-stat "b//")
'(4))

(check-equal? (run-stat "_a//")
'("i'm private"))

(check-exn exn:fail? (lambda ()
  (run-stat "c//")))

(check-exn exn:fail? (lambda ()
  (run-stat "_b//")))

(check-equal? (run-stat "string struct B ()//")
'("(struct B ())"))

(check-exn exn:fail? (lambda ()
  (run-stat "struct _B ()//")))

(check-exn exn:fail? (lambda ()
  (run-stat "struct C ()//")))

(check-equal? (run-stat "{b_txt}//")
'("b macro"))

(check-equal? (run-stat "b_txt//")
'("b macro"))

(check-exn exn:fail? (lambda ()
  (run-stat "{_b_txt}//")))

(check-exn exn:fail? (lambda ()
  (run-stat "{c_txt}//")))


;; this will rerun b.cm and set the current module id to b's path
(run-file-silent "files/basic1/b.cm")

(check-exn exn:fail? (lambda ()
  (run-stat ":>a_b//")))

(check-exn exn:fail? (lambda ()
  (run-stat ":>a_c//")))

(check-exn exn:fail? (lambda ()
  (run-stat "_a//")))

(check-equal? (run-stat "b//")
'(4))

(check-equal? (run-stat "c//")
'(4))

(check-exn exn:fail? (lambda ()
  (run-stat "_c//")))

(check-exn exn:fail? (lambda ()
  (run-stat "struct A ()//")))

(check-equal? (run-stat "string struct B ()//")
'("(struct B ())"))

(check-equal? (run-stat "string struct C ()//")
'("(struct C ())"))

(check-exn exn:fail? (lambda ()
  (run-stat "{a_txt}//")))

(check-equal? (run-stat "{b_txt}//")
'("b macro"))

(check-equal? (run-stat "{_b_txt}//")
'("b macro private"))

(check-equal? (run-stat "{c_txt}//")
'("c macro"))


;; keep this test at the end to avoid damage to the context affecting
;; the other tests
;;
;; this file has an undefined macro, so it should not load
(check-exn exn:fail? (lambda ()
  (run-file-silent "files/basic1/d.cm")))
