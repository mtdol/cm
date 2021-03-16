#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (run "index (1;) 0")
1)

(check-equal? (run "index (1,2,3;) 0")
1)

(check-equal? (run "index (1,2,3;) 1")
2)

(check-equal? (run "index (1,2,3;) (0,0;)")
null)

(check-equal? (run "index (1,2,3;) (1,1;)")
null)

(check-equal? (run "string index (1,2,3;) (0,1;)")
"(1;)")

(check-equal? (run "string index (1,2,3;) (1,2;)")
"(2;)")

(check-equal? (run "string index (1,2,3;) (0,2;)")
"(1, 2;)")


(check-exn exn:fail? (lambda ()
  (run "index (1;) 1")))

(check-exn exn:fail? (lambda ()
  (run "index (1,2) 1")))

(check-exn exn:fail? (lambda ()
  (run "index (1;) (-1)")))

(check-exn exn:fail? (lambda ()
  (run "index (1,2;) 2")))


(check-exn exn:fail? (lambda ()
  (run "index (1,2,3;) (0,1)")))

(check-exn exn:fail? (lambda ()
  (run "index (1,2,3;) (0,-1;)")))

(check-exn exn:fail? (lambda ()
  (run "index (1,2,3;) (0,4;)")))

(check-exn exn:fail? (lambda ()
  (run "index (1,2,3;) (1,4;)")))

