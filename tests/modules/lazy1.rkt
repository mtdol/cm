#lang racket
(require cm/tests/test-utils rackunit)

;; check if import with prefix works
(run-file-silent "files/lazy1/a.cm")
(run-file-silent "files/lazy1/b.cm")

(check-equal? (run-stat "a_v//")
'(7))

(check-equal? (run-stat "{a_txt}//")
'("a macro"))

(check-equal? (run-stat "string struct a_S ()//")
'("(struct a_S ())"))


(check-equal? (run-stat "c_v//")
'(4))

(check-equal? (run-stat "{c_txt}//")
'("c macro"))

(check-equal? (run-stat "string struct c_S ()//")
'("(struct c_S ())"))
