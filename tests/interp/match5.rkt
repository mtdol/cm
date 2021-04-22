#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "typedef S := a,b;")

(check-equal? (run "match 3 | ? a (lambda x := int? x) -> a end")
3)

(check-equal? (run "match 3 | ? a (lambda x := float? x) -> a + 1.0
                            | ? a (lambda x := int? x) -> a + 1 end")
4)

(check-failure run "match 3 | ? 3 (lambda x := float? x) -> a + 1.0
                            | ? a (lambda x := int? x) -> a + 1 end")

(check-failure run "match 3 | ? (a,b) (lambda x := float? x) -> a + 1.0
                            | ? a (lambda x := int? x) -> a + 1 end")

;; function must return a bool
(check-failure run "match 3 | ? a (lambda x := 4) -> a + 1.0
                            | ? a (lambda x := int? x) -> a + 1 end")

(check-failure run "match 3 | ? a (lambda x,y := 4) -> a + 1.0
                            | ? a (lambda x := int? x) -> a + 1 end")


(check-equal? (run "match struct S (1,2;)   
                    | struct S (? a (lambda x := x % 2 = 0), b;) -> a + b
                    | struct S (? a (lambda x := x % 2 = 1), b;) -> a + b + 1 
                    end")
4)

(check-equal? (run "match struct S (1,2;)   
                    | struct S (? _ (lambda x := x % 2 = 0), b;) -> a + b
                    | struct S (? _ (lambda x := x % 2 = 1), b;) -> b + 1 
                    end")
3)