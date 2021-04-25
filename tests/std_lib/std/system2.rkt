#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:import \"std_lib::std.cm\"")

(check-equal? (run ":>current_module")
"0")

(check-equal? (run "file? : \"files/system/a.txt\"")
val-false)

(check-equal? (run "file? : \"files/system2/a.txt\"")
val-true)

(check-equal? (run "directory? : \"files/system2/d\"")
val-false)

(check-equal? (run "directory? : \"files/system2/d1\"")
val-true)

(check-equal? (run "ls_rec : \"files/system2\"")
'("files/system2/a.txt" "files/system2/d1"
  "files/system2/d1/a.txt" "files/system2/d1/d1-1"
  "files/system2/d1/d1-1/b.cm" "files/system2/d1/d1-1/b.txt"))

(run-file-silent "files/system2/d1/d1-1/b.cm")

(check-equal? (run "b") 
3)

;; check that `current_module` still works in the context of another file
(check-regexp-match ".*files.system2.d1.d1-1.b\\.cm$" 
                    (run ":>current_module"))
