#lang racket
(require cm/core/parse-expr cm/core/interp cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (interp (parse-expr (tokenize-string "def x = match 5 | 5 -> 2 end")))
2)

(check-equal? (interp (parse-expr (tokenize-string "x")))
2)


(interp (parse-expr (tokenize-string "def x = lam n = match n | 5 -> 3 end")))

(check-equal? (interp (parse-expr (tokenize-string "5 : x")))
3)
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "4:x")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "4,2:x")))))

(interp (parse-expr (tokenize-string "def x = lam n = match n | 5 -> 3 | 7 -> 1 end")))

(check-equal? (interp (parse-expr (tokenize-string "5 : x")))
3)
(check-equal? (interp (parse-expr (tokenize-string "7 : x")))
1)
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "3:x")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "\"6h\":x")))))

(check-equal? (interp (parse-expr (tokenize-string "(match 5 | 5 -> 2 end) + 1")))
3)

(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "def x = match 5 | 6 -> 2 end")))))


;; variable time

(check-equal? (interp (parse-expr (tokenize-string "match 5 | x -> 7 end")))
7)
(check-equal? (interp (parse-expr (tokenize-string "match 5 | x -> x+1 end")))
6)
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "match 5 | x,y -> 2 end")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "match 5 | x,5 -> 2 end")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "match 5 | x -> x + 0.6 end")))))
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "match 5 | float x -> x + 0.6 end")))))

(check-equal? (interp (parse-expr (tokenize-string "match 5.0 | float x -> x + 0.6 end")))
5.6)
(check-equal? (interp (parse-expr (tokenize-string "match 5.0 | float x -> x + 0.6 | 3 -> 2 end")))
5.6)
(check-equal? (interp (parse-expr (tokenize-string "match 5.0 | 3 -> 1 | 5.0 -> 2 end")))
2)

(check-equal? (interp (parse-expr (tokenize-string "string match 5.0,3 | float x,y -> y,x end")))
"(3, 5.0)")
(check-equal? (interp (parse-expr (tokenize-string "string match 5.0,3; | float x,y; -> y,x end")))
"(3, 5.0)")
(check-exn exn:fail? (lambda ()
  (interp (parse-expr (tokenize-string "string match 5.0,3 | float x, bool y -> y,x end")))))
(check-equal? (interp (parse-expr (tokenize-string "string match 5.0,3 | float x, bool y -> y,x | x, y -> \"it worked\" end")))
"it worked")

(check-equal? (interp (parse-expr (tokenize-string "match 5.0,3; | a, b -> a end")))
5.0)
(check-equal? (interp (parse-expr (tokenize-string "string match 5.0,3; | a, b -> b end")))
"(3, null)")

(check-equal? (interp (parse-expr 
                (tokenize-string "match 5.0,3; | a, b when a = 5.0 -> a | _ -> 2 end")))
5.0)
(check-equal? (interp (parse-expr 
                (tokenize-string "match 5.0,3; | a, b when a = 5.1 -> a | _ -> 2 end")))
2)
(check-equal? (interp (parse-expr 
                (tokenize-string "match 5.0,3; | a, b when type a = \"float\" -> a | _ -> 2 end")))
5.0)
(check-equal? (interp (parse-expr 
                (tokenize-string "match 5.0,3; | a, b when type a = \"int\" -> a | _ -> 2 end")))
2)
)
