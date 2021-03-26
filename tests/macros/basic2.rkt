#lang racket
(require cm/tests/test-utils rackunit)

;; import macros
(run-file-silent "files/basic2.cm")

(check-exn exn:fail? (lambda ()
  (run-stat "{comp}.")))

(check-equal? (run-stat "{comp 4}.")
'(1))

(check-equal? (run-stat "{vari +|4}.")
'(4))

(check-equal? (run-stat "{vari +|4|5}.")
'(9))

(check-equal? (run-stat "{vari + | 3*2 | {vari *| 4|(7-1)|7} | 7 }.")
'(181))

(check-equal? (run-stat "{vari +|\\| false -> 1 else 2 |5}.")
'(7))


(check-equal? (run-stat "{pass +|4|5}.")
'(9))

(check-equal? (run-stat "{list 3|4|5}.")
'((3 4 5)))

(run-silent "defun sub3 (a, b, c) := a - b - c")

;(check-equal? (run-stat "{: sub3|3|4|5}.")
;'(-6))
