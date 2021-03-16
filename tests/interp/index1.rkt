#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "index \"a\" 0")
"a")

(check-equal? (run "index \"ab\" 0")
"a")

(check-equal? (run "index \"ab\" 1")
"b")

(check-equal? (run "index \"abc\" (0,0;)")
"")

(check-equal? (run "index \"abc\" (0,1;)")
"a")

(check-equal? (run "index \"abc\" (0,2;)")
"ab")

(check-equal? (run "index \"abc\" (1,3;)")
"bc")

(check-exn exn:fail? (lambda ()
  (run "index")))

(check-exn exn:fail? (lambda ()
  (run "index 5")))

(check-exn exn:fail? (lambda ()
  (run "index 5 0")))

(check-exn exn:fail? (lambda ()
  (run "index \"\" 0")))

(check-exn exn:fail? (lambda ()
  (run "index \"\" 1")))

(check-exn exn:fail? (lambda ()
  (run "index \"a\" 1")))

(check-exn exn:fail? (lambda ()
  (run "index \"a\" (-1)")))



(check-exn exn:fail? (lambda ()
  (run "index \"ab\" (0,1)")))

(check-exn exn:fail? (lambda ()
  (run "index \"ab\" (0,-1;)")))

(check-exn exn:fail? (lambda ()
  (run "index \"ab\" (0,3;)")))

(check-exn exn:fail? (lambda ()
  (run "index \"ab\" (1,3;)")))

(check-exn exn:fail? (lambda ()
  (run "index \"ab\" (2,1;)")))


