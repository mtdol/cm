#lang racket
(require cm/tests/test-utils cm/core/ast rackunit)

(run-silent "#:lang cm")

(run-silent "def h1 := :>make_hash")
(run-silent "def h2 := :>make_mutable_hash")
(run-silent "def h3 := handler.make_mutable_hash : (\\x -> \"wazup\")")

(run-silent "typedef S := a,b;")

(run-silent "def h1 := hash_set : h1 : \"a\" : 3")
(run-silent "def h1 := hash_set : h1 : \"b\" : 4")

(run-silent "hash_set : h2 : \"a\" : 3")
(run-silent "hash_set : h2 : \"b\" : 4")

(run-silent "hash_set : h3 : \"a\" : 3")
(run-silent "hash_set : h3 : \"b\" : 4")

(check-equal? (run "hash? : (hash_remove : h1 : \"a\")")
val-true)

(check-equal? (run "hash_remove : h2 : \"a\"")
val-void)

(check-equal? (run "hash_remove : h3 : \"a\"")
val-void)


(check-equal? (run "hash_has_key? : h1 : \"a\"")
val-true)

(check-equal? (run "hash_has_key? : (hash_remove : h1 : \"a\") : \"a\"")
val-false)

(check-equal? (run "hash_has_key? : h2 : \"a\"")
val-false)

(check-equal? (run "hash_has_key? : h3 : \"a\"")
val-false)


(run-silent "hash_set : h2 : \"a\" : -3")

(run-silent "hash_set : h3 : \"a\" : -3")

(check-equal? (run "hash_ref : h2 : \"a\"")
-3)

(check-equal? (run "hash_ref : h3 : \"a\"")
-3)


;; check that we can use structs as keys in the hash

(run-silent "def h1 := hash_set : h1 : (struct S {list 1|2}) : 3")
(run-silent "hash_set : h2 : (struct S {list 1|2}) : 3")
(run-silent "hash_set : h3 : (struct S {list 1|2}) : 3")

(check-equal? (run "hash_has_key? : h1 : (struct S {list 1|2})")
val-true)

(check-equal? (run "hash_has_key? : h2 : (struct S {list 1|2})")
val-true)

(check-equal? (run "hash_has_key? : h3 : (struct S {list 1|2})")
val-true)


(check-equal? (run "hash_ref : h1 : (struct S {list 1|2})")
3)

(check-equal? (run "hash_ref : h2 : (struct S {list 1|2})")
3)

(check-equal? (run "hash_ref : h3 : (struct S {list 1|2})")
3)


(check-failure run "hash_ref : h1 : (struct S {list 2|2})")

(check-failure run "hash_ref : h2 : (struct S {list 2|2})")


(run-silent "def h1 := hash_set : h1 : (struct S {list (struct S {list 3|4})|2}) : 4")
(run-silent "hash_set : h2 : (struct S {list (struct S {list 3|4})|2}) : 4")
(run-silent "hash_set : h3 : (struct S {list (struct S {list 3|4})|2}) : 4")


(check-equal? (run "hash_ref : h1 : (struct S {list (struct S {list 3|4})|2})")
4)

(check-equal? (run "hash_ref : h2 : (struct S {list (struct S {list 3|4})|2})")
4)

(check-equal? (run "hash_ref : h3 : (struct S {list (struct S {list 3|4})|2})")
4)

(check-failure run "hash_ref : h1 : (struct S {list (struct S {list 2|4})|2})")

(check-failure run "hash_ref : h2 : (struct S {list (struct S {list 2|4})|2})")
