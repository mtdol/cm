#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; connect_path_elems
;;

(check-equal? (run "connect_path_elems:{list \"a\"}")
"a")

(check-equal? (run "connect_path_elems:{list}")
"")

(check-equal? (run "connect_path_elems:{list \"a\"|\"b\"}")
(if (equal? (system-type) 'windows)
  "a\\b"
  "a/b"))

(check-equal? (run "connect_path_elems:{list \"a\"|\"b\"|\"f.txt\"}")
(if (equal? (system-type) 'windows)
  "a\\b\\f.txt"
  "a/b/f.txt"))

;;
;; get_extension
;;

(check-equal? (run "get_extension:\"af\"")
"")

(check-equal? (run "get_extension:\"\"")
"")

(check-equal? (run "get_extension:\"af.\"")
"")

(check-equal? (run "get_extension:\"af.c\"")
"c")

(check-equal? (run "get_extension:\"af.c.def\"")
"def")

(check-equal? (run "get_extension:\"f/af.c.def\"")
"def")

;;
;; get_path_elements
;;

(check-equal? (run "get_path_elements:\"a\"")
'("a"))

(check-equal? (run "get_path_elements:\"\"")
'(""))

(check-equal? (run (match (system-type) 
                          ['windows "get_path_elements:\"a\\\\b\""]
                          [_ "get_path_elements:\"a/b\""]))
'("a" "b"))

(check-equal? (run (match (system-type) 
                          ['windows "get_path_elements:\"a\\\\b\\\\f.txt\""]
                          [_ "get_path_elements:\"a/b/f.txt\""]))
'("a" "b" "f.txt"))

;;
;; get_path_to_file
;;

(check-equal? 
  (run (match (system-type) 
    ['windows "get_path_to_file:\"a\\\\b\\\\f.txt\""]
    [_ "get_path_to_file:\"a/b/f.txt\""]))
(if (equal? (system-type) 'windows)
  "a\\b"
  "a/b"))

(check-equal?
  (run (match (system-type) 
    ['windows "get_path_to_file:\"a\\\\f.txt\""]
    [_ "get_path_to_file:\"a/f.txt\""]))
"a")

(check-equal? (run "get_path_to_file:\"f.txt\"")
"")

;;
;; file?, directory?
;;

(check-equal? (run "file? : \"files/system/a.txt\"")
val-false)

(check-equal? (run "file? : \"files/system2/a.txt\"")
val-true)

(check-equal? (run "directory? : \"files/system2/d\"")
val-false)

(check-equal? (run "directory? : \"files/system2/d1\"")
val-true)

;;
;; ls_rec
;;

(check-equal? (run "ls_rec : \"files/system2\"")
(if (equal? (system-type) 'windows)
  '("files\\system2\\a.txt" "files\\system2\\d1"
    "files\\system2\\d1\\a.txt" "files\\system2\\d1\\d1-1"
    "files\\system2\\d1\\d1-1\\b.cm" "files\\system2\\d1\\d1-1\\b.txt")
  '("files/system2/a.txt" "files/system2/d1"
    "files/system2/d1/a.txt" "files/system2/d1/d1-1"
    "files/system2/d1/d1-1/b.cm" "files/system2/d1/d1-1/b.txt")))

;;
;; expand_path
;;

(check-equal? (run "(expand_path : \"n.txt\") = (cd \"\" $ \"n.txt\")")
val-true)

(check-equal? (run "(expand_path : \"./n.txt\") = (cd \"\" $ \"n.txt\")")
val-true)

;;
;; current_module{}
;;

(check-equal? (run ":>current_module")
"0")

(run-file-silent "files/system2/d1/d1-1/b.cm")

(check-equal? (run "b") 
3)

;; check that `current_module` still works in the context of another file
(check-regexp-match ".*files.system2.d1.d1-1.b\\.cm$" 
                    (run ":>current_module"))
