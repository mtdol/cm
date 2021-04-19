#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; macros
;;

(check-equal? 
  (run "match system_type 
       | \"windows\" -> {ifdef \"windows\" | true | false} 
       | \"macosx\" -> {ifdef \"macosx\" | true | false}
       | \"unix\" -> {ifdef \"unix\" | true | false}
       end")
val-true)

;;
;; Path stuff
;;

(check-equal? 
  (run "build_path:{list \"a\"|\"\"}")
"a")

(check-equal? 
  (run "build_path:{list \"\"|\"a\"}")
"a")

(check-equal? 
  (run "build_path:{list \" \"|\"a\"}")
(if (equal? (system-type) 'windows)
  " \\\\a"
  " /a"))

(check-equal? 
  (run "build_path:{list \"a\"|\"bcd\"|\"\"|\"ef\"}")
(if (equal? (system-type) 'windows)
  "a\\\\bcd\\\\ef"
  "a/bcd/ef"))

(check-equal? 
  (run "ls_build:\"files/system1\"")
(if (equal? (system-type) 'windows)
  '("files\\\\system1\\\\f.txt")
  '("files/system1/f.txt")))

;; invalid directory
(check-exn exn:fail? (lambda ()
  (run "cd2:\"file/system1\"")))

(check-equal? (run "cd2:\"files/system1\"")
val-void)

(check-equal? 
  (run "ls_build:\"\"")
(if (equal? (system-type) 'windows)
  '("f.txt")
  '("f.txt")))
