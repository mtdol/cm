#lang racket
(require cm/tests/test-utils rackunit)

;;
;; ifdef{}
;;

(run-silent "#:def:s{} true")

(check-equal? (run "{ifdef \"abc\" | 1 | 2}")
2)

(check-equal? (run "{ifdef \"s\" | 1 | 2}")
1)

(check-failure run "{ifdef 1 | 2}")

(check-failure run "{ifdef s | 1 | 2}")

(check-failure run "{ifdef \"s\" | 1 | 2 | 3}")
 

;;
;; string{}
;;
 
(check-equal? (run "{string x}")
"x")

(check-equal? (run "{string}")
"")

(check-equal? (run "{string \"abc\"}")
"\"abc\"")

(check-equal? (run "{string x \"abc\"}")
"x \"abc\"")

(check-equal? (run "{string x+ \"abc\"}")
"x + \"abc\"")

;;
;; current_module{}
;;

(check-equal? (run "{current_module}")
"0")

(run-file-silent "files/built-in1/a.cm")

(run-silent "def new := {current_module}")

(check-equal? (run "new !== \"0\"")
val-true)

(check-not-false 
  (regexp-match? #rx".*files.built-in1.a\\.cm$" (run "new")))

;;
;; ->module_id
;;

(run-silent "def id := {->module_id \"std_lib::std.cm\"}")

(check-not-false 
  (regexp-match? #rx".*std_lib.std\\.cm$" (run "id")))

(check-equal? (run "{->module_id \"files/built-in1/a.cm\"}")
(run "new"))
