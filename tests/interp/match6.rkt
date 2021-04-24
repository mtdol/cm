#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "typedef S := a,b;")

(check-equal? (run "match 3
                   | a, ... -> a
                   end")
'(3))

(check-equal? (run "match 1,2
                   | a, ... -> a
                   end")
'(1 2))

(check-equal? (run "match 1,2;
                   | a, ... -> a
                   end")
'(1 2 ()))

(check-equal? (run "match 1,2;
                   | a, b, ... -> a, b;
                   end")
'(1 (2 ())))

(check-equal? (run "match 1,2;
                   | a, ...; -> a
                   end")
'(1 2))

(check-equal? (run "match 1,2,();
                   | a, ...; -> a
                   end")
'(1 2 ()))

(check-equal? (run "match 1,2,();
                   | a, ..., c -> a
                   end")
'(1 2 ()))

(check-equal? (run "match 1,2,();
                   | a, ..., c -> c
                   end")
'())

(check-equal? (run "match 1,2,3,4;
                   | a, ..., c, d; -> c, d;
                   end")
'(3 4))

(check-equal? (run "match 1,2,3,4;
                   | a, ..., c, d; -> a
                   end")
'(1 2))

(check-equal? (run "match 1,2,3,4;
                   | a, b, ..., c, d; -> a, c, d;
                   end")
'(1 3 4))

(check-equal? (run "match 1, (\"a\", \"b\"), 2, 3, 4;
                   | a, b, ..., c, d; -> b
                   end")
'(("a" . "b") 2))


(check-failure run "match 1, 2, 3;)
                   | a, ..., b, c, d; -> a
                   end")

(check-failure run "match ((1,2;), 1, 3;)
                   | a, a, ...; -> a
                   end")

;; ok since `a` = (1, 2;) and the eventual value of second `a`
;; will be (1, 2;) as well
(check-equal? (run "match ((1,2;), 1, 2;)
                   | a, a, ...; -> a
                   end")
'(1 2))


(check-equal? (run "match 1, struct S (1,2;), 2, 3, 4;
                   | a, b, ..., c, d; -> string b
                   end")
"((struct S (1, 2;)), 2;)")

(check-equal? (run "match struct S (1,2;)
                   | struct S (a, ...;) -> a
                   end")
'(1 2))

(check-equal? (run "match struct S (1,2;), 3, 4;
                   | struct S (a, ...;), b, ...; -> a, b;
                   end")
'((1 2) (3 4)))
