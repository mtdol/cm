#lang racket
(require cm/core/lex rackunit)

(check-equal? (tokenize-string "\"\"")
'("\"\""))

(check-equal? (tokenize-string "\"5\"")
'("\"5\""))

(check-equal? (tokenize-string "\" 5 \"")
'("\" 5 \""))

(check-equal? (tokenize-string " \" 5 \"")
'("\" 5 \""))

(check-equal? (tokenize-string "\" 5 \" ")
'("\" 5 \""))

(check-equal? (tokenize-string "\"\\\"5\"")
'("\"\\\"5\""))

(check-equal? (tokenize-string "\" \\\"5\"")
'("\" \\\"5\""))

(check-equal? (tokenize-string "\"\\\"5 \"")
'("\"\\\"5 \""))

(check-equal? (tokenize-string "\"\\\"5 g\"")
'("\"\\\"5 g\""))

(check-equal? (tokenize-string "\"\\\"5 g\" + 7")
'("\"\\\"5 g\"" "plus" "7"))


;; check non-terminated strings
(check-exn exn:fail? (lambda ()
  (tokenize-string "\"")))

(check-exn exn:fail? (lambda ()
  (tokenize-string "a\"")))

(check-exn exn:fail? (lambda ()
  (tokenize-string "\"a")))

(check-exn exn:fail? (lambda ()
  (tokenize-string "a\"a")))

(check-exn exn:fail? (lambda ()
  (tokenize-string "a \"a")))

(check-exn exn:fail? (lambda ()
  (tokenize-string "a\" a")))

(check-exn exn:fail? (lambda ()
  (tokenize-string "a \" a")))
