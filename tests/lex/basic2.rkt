#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (tokenize "\"\"")
'("\"\""))

(check-equal? (tokenize "\"5\"")
'("\"5\""))

(check-equal? (tokenize "\" 5 \"")
'("\" 5 \""))

(check-equal? (tokenize " \" 5 \"")
'("\" 5 \""))

(check-equal? (tokenize "\" 5 \" ")
'("\" 5 \""))

(check-equal? (tokenize "\"\\\"5\"")
'("\"\"5\""))

(check-equal? (tokenize "\" \\\"5\"")
'("\" \"5\""))

(check-equal? (tokenize "\"\\\"5 \"")
'("\"\"5 \""))

(check-equal? (tokenize "\"\\\"5 g\"")
'("\"\"5 g\""))

(check-equal? (tokenize "\"\\\"5 g\" + 7")
'("\"\"5 g\"" "plus" "7"))


;; check non-terminated strings
(check-exn exn:fail? (lambda ()
  (tokenize "\"")))

(check-exn exn:fail? (lambda ()
  (tokenize "a\"")))

(check-exn exn:fail? (lambda ()
  (tokenize "\"a")))

(check-exn exn:fail? (lambda ()
  (tokenize "a\"a")))

(check-exn exn:fail? (lambda ()
  (tokenize "a \"a")))

(check-exn exn:fail? (lambda ()
  (tokenize "a\" a")))

(check-exn exn:fail? (lambda ()
  (tokenize "a \" a")))
