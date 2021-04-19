#lang racket
(require cm/tests/test-utils rackunit)

;; clear out the directory
(run-racket-silent 
  (map delete-directory/files (directory-list "files/system1" #:build? #t)))

(run-silent "def dir := \"files/system1/\"")

(check-equal? (run "ls dir")
'())

(check-equal? (run "writestrf \"ab\\nc\" (dir $ \"f.txt\")")
val-void)

(check-equal? (run "ls dir")
'("f.txt"))

(check-equal? (run "getlinesf (dir $ \"f.txt\")")
'("ab" "c"))

(check-equal? (run "appendstrf \"\\nef\" (dir $ \"f.txt\")")
val-void)

(check-equal? (run "getlinesf (dir $ \"f.txt\")")
'("ab" "c" "ef"))

(check-equal? (run "cp (dir $ \"f.txt\") (dir $ \"f2.txt\")")
val-void)

(check-equal? (run "getlinesf (dir $ \"f.txt\")")
'("ab" "c" "ef"))

(check-equal? (run "getlinesf (dir $ \"f2.txt\")")
'("ab" "c" "ef"))

(check-equal? (run "mkdir (dir $ \"d\")")
val-void)

(check-equal? (run "ls dir")
'("d" "f.txt" "f2.txt"))

(check-equal? (run "ls (dir $ \"d\")")
'())

(check-equal? (run "mv (dir $ \"f2.txt\") (dir $ \"d/f.txt\")")
val-void)

(check-equal? (run "ls dir")
'("d" "f.txt"))

(check-equal? (run "ls (dir $ \"d\")")
'("f.txt"))

(check-equal? (run "getlinesf (dir $ \"d/f.txt\")")
'("ab" "c" "ef"))

(run-silent "def old_dir := cd \"\"")

(check-equal? (run "cd (dir $ \"d\")")
val-void)

(check-equal? (run "ls \"\"")
'("f.txt"))

(check-equal? (run "cd old_dir")
val-void)

(check-equal? (run "ls dir")
'("d" "f.txt"))

(check-equal? (run "file_exists? (dir $ \"d/f.txt\")")
val-true)

(check-equal? (run "file_exists? (dir $ \"f.txt\")")
val-true)

(check-equal? (run "dir_exists? (dir $ \"d\")")
val-true)

(check-equal? (run "dir_exists? (dir $ \"d1\")")
val-false)

(check-equal? (run "file_exists? (dir $ \"f1.txt\")")
val-false)

;; remove everything

;; should not be removable as it does not exist
(check-exn exn:fail? (lambda ()
  (run "rm (dir $ \"d1\")")))

(check-equal? (run "rm (dir $ \"d/f.txt\")")
val-void)

(check-equal? (run "ls dir")
'("d" "f.txt"))

(check-equal? (run "ls (dir $ \"d\")")
'())

(check-equal? (run "rm (dir $ \"d\")")
val-void)

(check-equal? (run "ls dir")
'("f.txt"))

(check-exn exn:fail? (lambda ()
  (run "ls (dir $ \"d\")")))

(check-equal? (run "rm (dir $ \"f.txt\")")
val-void)

(check-equal? (run "ls dir")
'())
