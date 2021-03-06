#lang racket
(require cm/tests/test-utils rackunit)

(run-silent "typedef St2 := int a, int b;")
(run-silent "typedef St1 := int a, (struct St2 st);")

(check-equal? (run "string def struct St2 s2 := struct St2 (4,5;)")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2")
"(struct St2 (4, 5;))")
(check-equal? (run "string def struct St2 s2, struct St2 s2_2 := struct St2 (4,5;)")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2_2")
"(struct St2 (4, 5;))")
(check-equal? (run "string def struct St2 s2, s2_2 := struct St2 (4,5;)")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2_2")
"(struct St2 (4, 5;))")
(check-equal? (run "string def s2, struct St2 s2_2 := struct St2 (4,5;)")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2_2")
"(struct St2 (4, 5;))")
(check-equal? (run "string def s2, s2_2 := struct St2 (4,5;)")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2")
"(struct St2 (4, 5;))")
(check-equal? (run "string s2_2")
"(struct St2 (4, 5;))")

(check-equal? (run "let struct St2 s2 := struct St2 (4,5;) in string s2")
"(struct St2 (4, 5;))")
(check-equal? (run "let struct St2 s2 := struct St2 (4,5;) in string struct? St2 s2")
"true")
(check-equal? (run "let struct St2 s2 := struct St2 (4,5;) in string struct? St1 s2")
"false")

(run-silent"def sn := lam x := string x")
(check-equal? (run "struct St2 (2,7;) : sn")
"(struct St2 (2, 7;))")
(run-silent "def sn := lam struct St2 x := string x")
(check-equal? (run "struct St2 (2,7;) : sn")
"(struct St2 (2, 7;))")
(run-silent "def sn := lam struct St2 x, struct St2 y := string x $ \" \" $ string y")
(check-equal? (run "(struct St2 (5,6;)) : (struct St2 (2,7;)) : sn")
"(struct St2 (2, 7;)) (struct St2 (5, 6;))")
(run-silent "def sn := lam x, struct St2 y := string x $ \" \" $ string y")
(check-equal? (run "(struct St2 (5,6;)) : (struct St2 (2,7;)) : sn")
"(struct St2 (2, 7;)) (struct St2 (5, 6;))")
(run-silent "def sn := lam struct St2 x, y := string x $ \" \" $ string y")
(check-equal? (run "(struct St2 (5,6;)) : (struct St2 (2,7;)) : sn")
"(struct St2 (2, 7;)) (struct St2 (5, 6;))")
(run-silent "def sn := lam x, y := string x $ \" \" $ string y")
(check-equal? (run "(struct St2 (5,6;)) : (struct St2 (2,7;)) : sn")
"(struct St2 (2, 7;)) (struct St2 (5, 6;))")

(run-silent "def sn := lam struct St2 x, struct St1 y := string x $ \" \" $ string y")
(check-equal? (run "(struct St1 (5,(struct St2 (1,2;);))) : (struct St2 (2,7;)) : sn")
"(struct St2 (2, 7;)) (struct St1 (5, (struct St2 (1, 2;));))")
(run-silent "def sn := lam x, struct St1 y := string x $ \" \" $ string y")
(check-equal? (run "(struct St1 (5,(struct St2 (1,2;));)) : (struct St2 (2,7;)) : sn")
"(struct St2 (2, 7;)) (struct St1 (5, (struct St2 (1, 2;));))")
(run-silent "def sn := lam struct St2 x, y := string x $ \" \" $ string y")
(check-equal? (run "(struct St1 (5,(struct St2 (1,2;));)) : (struct St2 (2,7;)) : sn")
"(struct St2 (2, 7;)) (struct St1 (5, (struct St2 (1, 2;));))")
(run-silent "def sn := lam x, y := string x $ \" \" $ string y")
(check-equal? (run "(struct St1 (5,(struct St2 (1,2;));)) : (struct St2 (2,7;)) : sn")
"(struct St2 (2, 7;)) (struct St1 (5, (struct St2 (1, 2;));))")


(run-silent "typedef Stn := null")

(run-silent "def sn := lam struct Stn x, y := string x $ \" \" $ string y")
(check-equal? (run "(struct St1 (5,(struct St2 (1,2;));)) : (struct Stn null) : sn")
"(struct Stn ()) (struct St1 (5, (struct St2 (1, 2;));))")

(check-equal? (run "string struct? Stn (struct Stn null)")
"true")
(check-equal? (run "string struct? St1 (struct Stn null)")
"false")
(check-equal? (run "string struct? Stn (struct St2 (7,4;))")
"false")

