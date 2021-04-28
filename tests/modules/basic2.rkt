#lang racket
(require cm/tests/test-utils rackunit)

;; set should not work on an undeclared var
(check-exn exn:fail? (lambda ()
  (run-stat "set v := 5//")))

(run-stat-silent "def v := 5//")

(check-equal? (run-stat "set v := 4//")
'(4))

(check-equal? (run-stat "v//")
'(4))


(run-file-silent "files/basic2/a.cm")

;; check that resources can be shared across files 
(check-equal? (run-stat ":>a_c// :>a_c// :>a_c//")
'(4 5 6))


(run-file-silent "files/basic2/b.cm")

(check-equal? (run-stat ":>b_c// :>b_c//")
'(5 4))


(run-file-silent "files/basic2/a.cm")

(check-equal? (run-stat ":>a_c// :>a_c//")
'(5 6))

(run-stat-silent ":>destroy_ref//")

(check-equal? (run-stat ":>a_c2//")
'(22))


(run-file-silent "files/basic2/b.cm")

(check-equal? (run-stat ":>b_c// :>b_c//")
'(5 4))


;; references should be restored
(run-file-silent "files/basic2/a.cm")

(check-equal? (run-stat ":>a_c// :>a_c//")
'(5 6))
