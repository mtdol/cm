#lang racket
(require cm/tests/test-utils rackunit)

;; import macros
(run-file-silent "files/basic1.cm")

(check-exn exn:fail? (lambda ()
  (run-stat "one.")))

(check-exn exn:fail? (lambda ()
  (run-stat "{one.")))

(check-exn exn:fail? (lambda ()
  (run-stat "one}.")))

(check-exn exn:fail? (lambda ()
  (run-stat "{}.")))

(check-equal? (run-stat "{one}.")
'(1))

(check-equal? (run-stat "{one }.")
'(1))

(check-equal? (run-stat "{one  }.")
'(1))

(check-equal? (run-stat "{one} + {one}.")
'(2))

(check-equal? (run-stat "{two}.")
'(2))

(check-equal? (run-stat "({two}+{two})*({two}+{one}).")
'(12))


(check-equal? (run-stat "{add1 3}.")
'(4))

(check-equal? (run-stat "{+1 3}.")
'(4))

(check-equal? (run-stat "{ +1 3}.")
'(4))

(check-equal? (run-stat "{+1}.")
'(1))

(check-equal? (run-stat "{ +1}.")
'(1))

(check-equal? (run-stat "{+1 }.")
'(1))

(check-equal? (run-stat "{ +1 }.")
'(1))

(check-equal? (run-stat "{add1 {add1 3}}.")
'(5))

(check-equal? (run-stat "{add1 {add1 {one}}}.")
'(3))

(run-stat-silent "{set! x|7}.")

(check-equal? (run-stat "x.")
'(7))

(run-stat-silent "{++ x}.")

(check-equal? (run-stat "x.")
'(8))



(check-equal? (run-stat "{plus x|4}.")
'(12))

(check-equal? (run-stat "{plus {minus x|1}|4}.")
'(11))

(check-equal? (run-stat "{app_op x|-|3}.")
'(5))

(check-equal? (run-stat "{app_op {add1 {app_op x|%|2}}|-|3}.")
'(-2))

(check-equal? (run-stat "1 + {app_op {add1 {app_op x|%|2}}|-|3}.")
'(-1))
