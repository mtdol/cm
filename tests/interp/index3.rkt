#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "typedef S := a,b;")
(run-silent "def s := struct S (1,\"e\";)")

(check-equal? (run "index s a")
1)

(check-equal? (run "index s (var \"a\")")
1)

(check-equal? (run "index s b")
"e")

(check-equal? (run "index s var \"b\"")
"e")

(check-failure run "index s c")

(check-failure run "index s var \"c\"")

(check-failure run "index s 1")

(check-failure run "index s true")

(check-failure run "index s \"a\"")

(run-silent "typedef S2 := a, int b, _1;")
(run-silent "def s2 := struct S2 (1, 4, true;)")

(check-equal? (run "index s2 a")
1)

(check-equal? (run "index s2 b")
4)

(check-equal? (run "index s2 _1")
val-true)

(check-failure run "index s2 c")

(check-failure run "index s2 1")
