#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test
(check-equal? (interp (parse-expr (tokenize-string "index \"a\" 0")))
"a")

(check-equal? (interp (parse-expr (tokenize-string "index \"ab\" 0")))
"a")

(check-equal? (interp (parse-expr (tokenize-string "index \"ab\" 1")))
"b")

(check-equal? (interp (parse-expr (tokenize-string "index \"abc\" (0,0;)")))
"")

(check-equal? (interp (parse-expr (tokenize-string "index \"abc\" (0,1;)")))
"a")

(check-equal? (interp (parse-expr (tokenize-string "index \"abc\" (0,2;)")))
"ab")

(check-equal? (interp (parse-expr (tokenize-string "index \"abc\" (1,3;)")))
"bc")

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index 5")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index 5 0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index \"\" 0")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index \"\" 1")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index \"a\" 1")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index \"a\" (-1)")))))



(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index \"ab\" (0,1)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index \"ab\" (0,-1;)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index \"ab\" (0,3;)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index \"ab\" (1,3;)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index \"ab\" (2,1;)")))))


)
