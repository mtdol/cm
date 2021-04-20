#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "typedef S := a,b;")

(check-equal? 
  (run "match 3
       | var \"a\" -> a end")
3)

(check-equal? 
  (run "match 3
       | var \"a\" -> var \"a\" end")
3)

(check-equal? 
  (run "match 3
       | a -> var \"a\" end")
3)

(check-equal? 
  (run "match 3
       | int var \"a\" -> var \"a\" end")
3)

(check-equal? 
  (run "match (struct S (3,4;))
       | int var \"a\" -> var \"a\" + 1
       | struct (var \"S\") (a, b;) -> a + b 
       end")
7)

(run-silent "def t := \"S\"")

(check-equal? 
  (run "match (struct S (3,4;))
       | int var \"a\" -> var \"a\" + 1
       | struct (var \"S\") (a, b;) -> a + b 
       end")
7)

(check-equal? 
  (run "match (struct S (3,4;))
       | int var \"a\" -> var \"a\" + 1
       | struct (var \"S\") (var \"a\", b;) -> a + var \"b\" 
       end")
7)

(check-equal? 
  (run "match (struct S (3,4;))
       | int var \"a\" -> var \"a\" + 1
       | struct (var t) (var \"a\", b;) -> a + var \"b\" 
       end")
7)

(check-equal? 
  (run "match 4.2
       | int var \"a\" -> var \"a\" + 1
       | types (\"int\", \"float\";) (var \"a\") -> a + 1.2
       end")
5.4)
