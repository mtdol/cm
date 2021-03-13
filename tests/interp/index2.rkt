#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (interp (parse-expr (tokenize-string "index (1;) 0")))
1)

(check-equal? (interp (parse-expr (tokenize-string "index (1,2,3;) 0")))
1)

(check-equal? (interp (parse-expr (tokenize-string "index (1,2,3;) 1")))
2)

(check-equal? (interp (parse-expr (tokenize-string "index (1,2,3;) (0,0;)")))
null)

(check-equal? (interp (parse-expr (tokenize-string "index (1,2,3;) (1,1;)")))
null)

(check-equal? (interp (parse-expr (tokenize-string "string index (1,2,3;) (0,1;)")))
"(1;)")

(check-equal? (interp (parse-expr (tokenize-string "string index (1,2,3;) (1,2;)")))
"(2;)")

(check-equal? (interp (parse-expr (tokenize-string "string index (1,2,3;) (0,2;)")))
"(1, 2;)")


(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index (1;) 1")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index (1,2) 1")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index (1;) (-1)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index (1,2;) 2")))))


(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index (1,2,3;) (0,1)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index (1,2,3;) (0,-1;)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index (1,2,3;) (0,4;)")))))

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "index (1,2,3;) (1,4;)")))))

)
