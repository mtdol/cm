#lang racket
(require cm/core/parse-expr cm/core/interp  cm/core/lex)

(module+ test
           (require rackunit))

(module+ test
;; check for things that should fail

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "typedef S = 3")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "typedef S = 3;")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "typedef S = a")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "typedef S = int float a;")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "typedef S a;")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "typedef = a;")))))


(interp (parse-expr (tokenize-string "typedef S = a;")))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "Struct S 3;")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "struct S = 3;")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "struct S 3")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "struct s 3;")))))

(interp (parse-expr (tokenize-string "typedef St2 = int a, int b;")))
(interp (parse-expr (tokenize-string "typedef St1 = int a,  (struct St2 st);")))

(check-exn exn:fail? (lambda ()
 (interp (parse-expr (tokenize-string "def struct St2 s2 = struct S 4;")))))

(check-exn exn:fail? (lambda ()
 (interp (parse-expr (tokenize-string "def struct St2 s2 = 4")))))
)
