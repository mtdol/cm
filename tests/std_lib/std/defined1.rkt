#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:lang cm")

;;
;; defined?
;;

(run-silent "def x1 := 3")
(run-silent "static x2 := 4")

(check-equal? (run "defined? \"x1\" :>current_module")
val-true)

(check-equal? (run "defined? \"x2\" :>current_module")
val-true)

(check-equal? (run "defined? \"x3\" :>current_module")
val-false)

(check-equal? (run "let x3 := 3 in defined? \"x3\" :>current_module")
val-true)

(check-equal? (run "param x3 := 3 in defined? \"x3\" :>current_module")
val-true)

(check-equal? (run "param x1 := 3 in defined? \"x1\" :>current_module")
val-true)

;;
;; global.defined?
;;

(check-equal? (run "global.defined? \"x1\" :>current_module")
val-true)

(check-equal? (run "global.defined? \"x2\" :>current_module")
val-true)

(check-equal? (run "global.defined? \"x3\" :>current_module")
val-false)

(check-equal? (run "let x3 := 3 in global.defined? \"x3\" :>current_module")
val-false)

(check-equal? (run "param x3 := 3 in global.defined? \"x3\" :>current_module")
val-false)

(check-equal? (run "param x1 := 3 in global.defined? \"x1\" :>current_module")
val-true)

;;
;; local.defined?
;;

(check-equal? (run "local.defined? \"x1\"")
val-false)

(check-equal? (run "local.defined? \"x2\"")
val-false)

(check-equal? (run "local.defined? \"x3\"")
val-false)

(check-equal? (run "let x3 := 3 in local.defined? \"x3\"")
val-true)

(check-equal? (run "param x3 := 3 in local.defined? \"x3\"")
val-false)

(check-equal? (run "param x1 := 3 in local.defined? \"x1\"")
val-false)

(check-equal? (run "let x1 := 3 in local.defined? \"x1\"")
val-true)

;;
;; params.defined?
;;

(check-equal? (run "params.defined? \"x1\"")
val-false)

(check-equal? (run "params.defined? \"x2\"")
val-false)

(check-equal? (run "params.defined? \"x3\"")
val-false)

(check-equal? (run "let x3 := 3 in params.defined? \"x3\"")
val-false)

(check-equal? (run "param x3 := 3 in params.defined? \"x3\"")
val-true)

(check-equal? (run "param x1 := 3 in params.defined? \"x1\"")
val-true)

(check-equal? (run "let x1 := 3 in params.defined? \"x1\"")
val-false)
