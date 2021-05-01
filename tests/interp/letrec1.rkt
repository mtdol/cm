#lang racket
(require cm/tests/test-utils rackunit)

;; recursive
(check-equal? 
  (run "letrec f (int x) :=
         | x = 0 -> 0 
         else x + (f:(x - 1)) 
        in f:4")
(+ 4 3 2 1 0))

;; mutually recursive
(check-equal? 
  (run "letrec f (int x) :=
          | x <= 0 -> 0 
          else x + (g:(x - 1)) 
        in letrec g (int x) :=
          f:x
        in g:4 + f:2")
(+ (+ 4 3 2 1 0) (+ 2 1 0)))

;; mutually recursive, multiple args
(check-equal? 
  (run "letrec f (int x, int y) :=
          | x <= 0 -> 0 - y
          else (x - y) + (g:(x - 1):(y + 1)) 
        in letrec g (int x, int y) :=
          f:x:y
        in g:4:2")
(+ (- 4 2) (- 3 3) (- 2 4) (- 1 5) (- 0 6)))

;; get_param
(check-equal? 
  (run "letrec f (int x) :=
          | x <= 0 -> 0 
          else x + ((get_param g):(x - 1)) 
        in letrec g (int x) :=
          (get_param f):x
        in (get_param g):4 + (get_param f):2")
(+ (+ 4 3 2 1 0) (+ 2 1 0)))

;; order of evaluation
(check-equal? 
  (run "let f := 3 in
         letrec f () := 2
          in f")
3)

(run-silent "def f := 4")

(check-equal? 
  (run "letrec f () := 2
          in f")
4)

(check-equal? 
  (run "letrec f () := 2
          in :> get_param f")
2)

(run-silent "defun f2 () := :>g")

;; letrec will bleed through due to it creating a param
(check-equal? 
  (run "letrec g () := 7
          in :>f2")
7)
