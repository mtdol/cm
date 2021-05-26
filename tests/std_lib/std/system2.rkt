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
  " \\a"
  " /a"))

(check-equal? 
  (run "build_path:{list \"f/\"|\"a\"}")
(if (equal? (system-type) 'windows)
  "f/\\a"
  "f/a"))

(check-equal? 
  (run "build_path:{list \"f\\\\\"|\"a\"}")
(if (equal? (system-type) 'windows)
  "f\\a"
  "f\\/a"))

(check-equal? 
  (run "build_path:{list \"a\"|\"bcd\"|\"\"|\"ef\"}")
(if (equal? (system-type) 'windows)
  "a\\bcd\\ef"
  "a/bcd/ef"))

(check-equal? 
  (run "ls_build:\"files/system2\"")
(if (equal? (system-type) 'windows)
  '("files\\system2\\f.txt")
  '("files/system2/f.txt")))

(check-equal? 
  (run "ls_build:\"files/system2/\"")
(if (equal? (system-type) 'windows)
  '("files\\system2\\f.txt")
  '("files/system2/f.txt")))

;; invalid directory
(check-failure run "cd_check:\"file/system2\"")

(check-equal? (run "cd_check:\"files/system2\"")
val-void)

(check-equal? 
  (run "ls_build:\"\"")
(if (equal? (system-type) 'windows)
  '("f.txt")
  '("f.txt")))
