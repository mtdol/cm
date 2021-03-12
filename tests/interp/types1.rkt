#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (interp (parse-expr (tokenize-string "def types (\"int\";) x := 5")))
5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5)

(check-equal? (interp (parse-expr (tokenize-string "def types (\"dynamic\";) x := 5.5")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5.5)

(check-equal? (interp (parse-expr (tokenize-string "def types (\"dynamic\", \"int\";) x := 5.5")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5.5)

(check-equal? (interp (parse-expr (tokenize-string "def types (\"float\", \"int\";) x := 5.5")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5.5)

(check-equal? (interp (parse-expr (tokenize-string "def types (\"int\", \"float\";) x := 5.5")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5.5)

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def types (\"float\", \"int\";) x := true")))))

(check-equal? (interp (parse-expr (tokenize-string "def types (\"int\", \"float\";) x, y := 5.5")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "y")))
5.5)

(check-equal? (interp (parse-expr (tokenize-string "def types (\"int\", \"float\";) x, float y := 5.5")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "y")))
5.5)

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def types (\"float\", \"int\";) x, int y := 5.5")))))

(check-equal? (interp (parse-expr (tokenize-string "def types (\"int\", \"fl\"$\"oat\";) x := 5.5")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5.5)

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def types (\"int\", \"fl\"$\"at\";) x := 5.5")))))

(interp (parse-expr (tokenize-string "def str := \"float\"")))
(interp (parse-expr (tokenize-string "def lst := (\"int\", \"float\";)")))

(check-equal? (interp (parse-expr (tokenize-string "def types (\"int\", str;) x := 5.5")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5.5)

(check-equal? (interp (parse-expr (tokenize-string "def types lst x := 5.5")))
5.5)
(check-equal? (interp (parse-expr (tokenize-string "x")))
5.5)

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def types null x := 5.5")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def types 3 x := 5.5")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def types (3;) x := 5.5")))))


(check-equal? (interp (parse-expr (tokenize-string "let types (\"int\", \"float\";) x := 5.5 in x + 1.0")))
6.5)
(check-equal? (interp (parse-expr (tokenize-string "let types (\"int\", str;) x := 5.5 in x + 1.0")))
6.5)
(check-equal? (interp (parse-expr (tokenize-string "let types lst x := 5.5 in x + 1.0")))
6.5)

(check-equal? (interp (parse-expr (tokenize-string "5.0 : lam types (\"int\", \"float\";) x := x + 2.0")))
7.0)
(check-equal? (interp (parse-expr (tokenize-string "5.0 : lam types (\"int\", str;) x := x + 2.0")))
7.0)
(check-equal? (interp (parse-expr (tokenize-string "5.0 : lam types lst x := x + 2.0")))
7.0)
(check-equal? (interp (parse-expr (tokenize-string "8 : 5.0 : lam types lst x, y := + x + float y + 2.0")))
15.0)
(check-equal? (interp (parse-expr (tokenize-string "8 : 5.0 : lam types lst x, types (\"int\", \"string\";) y := + x + float y + 2.0")))
15.0)
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "8.0 : 5.0 : lam types lst x, types (\"int\", \"string\";) y := + x + float y + 2.0")))))

)
