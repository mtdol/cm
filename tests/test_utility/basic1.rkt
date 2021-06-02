#lang racket
(require cm/tests/test-utils cm/core/context cm/core/modules
         rackunit)

; module-id of the testing_utility file
(define test-file 
  (file-name->module-id 
    (module-string->filename "std_lib::testing.cm")))

; grabs a (private) variable from the testing_utility module
(define (get-data str)
  (get-global-var-data str test-file))

(run-silent "#:import \"std_lib::std.cm\"")
(run-silent "#:import \"std_lib::testing.cm\"")

(check-equal? (run "assert_equal : 3 : 3")
val-true)

(check-equal? (run "assert_equal : 4 : 3")
val-false)

(check-equal? (run "assert_equal : \"a\" : 3")
val-false)

(check-equal? (run "assert_equal : \"a\" : \"a\"")
val-true)

(check-equal? (run "assert_not_equal : \"a\" : \"a\"")
val-false)

(check-equal? (run "assert_not_equal : 1 : \"a\"")
val-true)

(check-equal? (run "assert_not_equal : 2 : 2")
val-false)

(check-equal? (run "assert_equalf : (\\x,y -> x - 1 = y) : 2 : 2")
val-false)

(check-equal? (run "assert_equalf : (\\x,y -> x - 1 = y) : 3 : 2")
val-true)


(check-equal? (get-data "_num_tests")
9)

(check-equal? (get-data "_num_successful_tests")
4)

(check-equal? (get-data "_num_failed_tests")
5)

;; check failures

(check-equal? (run "assert_failure : (\\x,y -> x - y) : (1,2;)")
val-false)

(check-equal? (run "assert_failure : (\\x,y -> x - y) : (1,2,4;)")
val-true)

(check-equal? (run "assert_failure : (\\x,y -> x - y) : (1,2.0;)")
val-true)

(check-equal? (run "assert_failure_id : \"CONTRACT\" :(\\x,y -> x - y) : (1,2;)")
val-false)

(check-equal? (run "assert_failure_id : \"CONTRACT\" :(\\x,y -> x - y) : (1,2.0;)")
val-true)

; fails with "GENERIC" instead of "CONTRACT"
(check-equal? (run "assert_failure_id : \"CONTRACT\" :
                      (\\() -> error : \"err\") : ()")
val-false)

(check-equal? (get-data "_num_tests")
(+ 9 6))

(check-equal? (get-data "_num_successful_tests")
(+ 4 3))

(check-equal? (get-data "_num_failed_tests")
(+ 5 3))
