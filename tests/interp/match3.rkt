#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "typedef S := a;")
(run-silent "typedef Si := int a;")
(run-silent "typedef S2 := a, b;")


(check-equal? (run "match struct S (3;) | struct S s -> string s end")
"(struct S (3;))")

(check-equal? (run "match struct S (3;) | s -> string s end")
"(struct S (3;))")

(check-equal? (run "match struct S (3;) | struct S _ -> true end")
val-true)

(check-equal? (run "match struct S (3;) | struct S (3;) -> 2 end")
2)
(check-equal? (run "match struct S (3;) | struct S (a;) -> 2 end")
2)
(check-equal? (run "match struct S (3;) | struct S (a;) -> a + 1 end")
4)
(check-equal? (run "match struct Si (3;) | struct Si (a;) -> a + 1 end")
4)
(check-failure run "match struct S (3;) | struct S float (a;) -> a + 1 end")
(check-failure run "match struct Si (3;) | struct S (a;) -> a + 1 end")

(check-equal? (run "match struct S (3.0;) | struct S (float a;) -> a + 1.0 end")
4.0)

(check-equal? (run "match struct S2 (4,5;) | struct S2 (a, b;) -> 7 end")
7)

(check-equal? (run "match struct S2 (4,5;) | struct S2 (a, b;) -> a + b end")
9)

(check-equal? (run "match struct S2 (4.2,5;) | struct S2 (float a, b;) -> int a + b end")
9)
(check-failure run "match struct S2 (4,5;) | struct S2 (float a, b;) -> int a + b end")
(check-failure run "match struct S2 (4,5;) | struct S2 (a, float b;) -> int a + b end")


(check-equal? (run "match struct S (3;) | s -> 2 end")
2)

(check-equal? (run "match struct S (3;) | s -> string s end")
"(struct S (3;))")

(check-equal? (run "match (struct S (3;)),4 | a,b -> string a end")
"(struct S (3;))")
(check-equal? (run "match (struct S (3;)),4 | a,b -> b end")
4)

(check-equal? (run "match (struct S (3;)),4 | (struct S (a;)),b -> a end")
3)

(check-equal? (run "match (struct S (3;)),4 | (struct S (a;)),b -> a+b end")
7)
(check-failure run "match (struct S2 (3,2;)),4 | (struct S (a;)),b -> a+b end")
(check-failure run "match (struct S (3;)),4 | (struct S (a;)), float b -> a+b end")

(check-equal? (run "match (struct S (3;)),4 | (struct S (a;)), float b -> a+b | (struct S (a;)), int b -> a + b end")
7)

(check-equal? (run "match (struct S2 (3,2;)),4 | (struct S (a;)), b -> a+b | (struct S2 (a,b;)), c -> a + b + c end")
9)
(check-equal? (run "match (struct S2 (3,2;)),4.1 | (struct S2 (a, b;)), c -> a + b + int c | (struct S2 (a,b;)), float c -> a + b + int c + 1 end")
9)
(check-equal? (run "match (struct S2 (3,2;)),4.1 | (struct S2 (a,b;)), float c -> a + b + int c + 1 | (struct S2 (a, b;)), c -> a + b + int c end")
10)


(check-equal? (run "match (struct S2 (3,2.2;)),4.1 | (struct S2 (a, bool b;)), c -> a + b + c | (struct S2 (int a, float b;)), c -> float a + b + c end")
9.3)


(check-equal? (run "match (struct S (3;)),4 | (struct S (a;)), b when a + b = 7 -> a+b+1 | (struct S (a;)), b -> a + b end")
8)
(check-equal? (run "match (struct S (3;)),4 | (struct S (a;)), b when a + b = 8 -> a+b+1 | (struct S (a;)), b -> a + b end")
7)


(check-equal? (run "match (struct S ((struct S (2;));)),4 | (struct S (a;)), b -> b | (struct S (a;)), b -> 0 end")
4)

(check-equal? (run "match (struct S ((struct S (2;));)),4 | (struct S ((struct S (a;));)), b -> a | (struct S (a;)), b -> 0 end")
2)

(check-equal? (run "match (struct S ((struct S2 (2,3;));)),4 | (struct S (struct S (a;);)), b -> a | (struct S ((struct S2 (a,b;));)), c  -> a + b end")
5)

(check-equal? (run "match (struct S ((struct S2 (2,3;));)),4 | (struct S ((struct S2 (a, float b;));)), c -> a | (struct S ((struct S2 (a, int b;));)), c  -> a + b end")
5)
