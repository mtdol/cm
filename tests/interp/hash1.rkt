#lang racket
(require cm/tests/test-utils cm/core/ast rackunit)

(run-silent "def h1 := make_hash ()")
(run-silent "def h1_2 := make_hash (\"immutable\";)")
(run-silent "def h2 := make_hash (\"mutable\";)")
(run-silent "def h3 := make_hash (\"mutable\",(lambda () := \"wazup\");)")

(check-equal? (run "hash? h1")
val-true)
(check-equal? (run "hash? h1_2")
val-true)
(check-equal? (run "hash? h2")
val-true)
(check-equal? (run "hash? h3")
val-true)
(check-equal? (run "hash? 7")
val-false)
(check-equal? (run "mutable_hash? h1")
val-false)
(check-equal? (run "mutable_hash? h1_2")
val-false)
(check-equal? (run "mutable_hash? h2")
val-true)
(check-equal? (run "mutable_hash? h3")
val-true)
(check-equal? (run "mutable_hash? 7")
val-false)

(check-exn exn:fail? (lambda ()
  (run "make_hash \"mutable\"")))

(check-exn exn:fail? (lambda ()
  (run "make_hash 4")))

(check-exn exn:fail? (lambda ()
  (run "make_hash (\"mutabl\";)")))

(check-exn exn:fail? (lambda ()
  (run "make_hash (\"mutable\",4;)")))


(check-exn exn:fail? (lambda ()
  (run "hash_ref h1 4")))

(check-exn exn:fail? (lambda ()
  (run "hash_ref h1_2 4")))

(check-exn exn:fail? (lambda ()
  (run "hash_ref h2 4")))

(check-equal? (run "hash_ref h3 4")
"wazup")

(check-equal? (run "hash_ref_check h3 4 (lambda () := \"cool\")")
"cool")

(check-equal? (run "hash_ref_check h1 4 (lambda () := \"cool\")")
"cool")

(check-equal? (run "hash_ref_check h1 4 (lambda x := x+1)")
5)

(check-exn exn:fail? (lambda ()
  (run "hash_ref_check h1 4 5")))

(check-exn exn:fail? (lambda ()
  (run "hash_ref_check 1 4 (lambda () := \"cool\")")))


(check-exn exn:fail? (lambda ()
  (run "hash_set 1 4 5")))

(run-silent "def h1 := hash_set h1 4 1")
(check-equal? (run "hash_set h3 4 1")
(Prim0 'void))


(check-exn exn:fail? (lambda ()
  (run "hash_ref h1 5")))

(check-equal? (run "hash_ref h3 5")
"wazup")

(check-equal? (run "hash_ref h1 4")
1)

(check-equal? (run "hash_ref h3 4")
1)


(run-silent "def h1 := hash_set h1 4 2")
(check-equal? (run "hash_set h3 4 2")
(Prim0 'void))

(check-equal? (run "hash_ref h1 4")
2)

(check-equal? (run "hash_ref h3 4")
2)


(run-silent "def h1 := hash_set h1 \"x\" 7")
(check-equal? (run "hash_set h3 \"x\" 7")
(Prim0 'void))

(check-equal? (run "hash_ref h1 \"x\"")
7)

(check-equal? (run "hash_ref h3 \"x\"")
7)

(check-equal? (run "hash_ref h1 4")
2)

(check-equal? (run "hash_ref h3 4")
2)


(check-equal? (run "hash_has_key? h1 4")
val-true)

(check-equal? (run "hash_has_key? h3 4")
val-true)

(check-equal? (run "hash_has_key? h1 5")
val-false)

(check-equal? (run "hash_has_key? h3 5")
val-false)

(check-exn exn:fail? (lambda ()
  (run "hash_has_key? 5 6")))


(check-equal? (run "(((hash_keys h1) == (4, \"x\";)) or ((hash_keys h1) == (\"x\", 4;)))")
val-true)

(check-equal? (run "(((hash_keys h3) == (4, \"x\";)) or ((hash_keys h3) == (\"x\", 4;)))")
val-true)

(check-equal? (run "(((hash_values h1) == (2, 7;)) or ((hash_values h1) == (7, 2;)))")
val-true)

(check-equal? (run "(((hash_values h3) == (2, 7;)) or ((hash_values h3) == (7, 2;)))")
val-true)

(check-equal? (run "[[(hash_to_list h1) == ((4, 2), (\"x\", 7);)] or [(hash_to_list h1) == ((\"x\", 7), (4, 2);)]]")
val-true)

(check-equal? (run "[[(hash_to_list h3) == ((4, 2), (\"x\", 7);)] or [(hash_to_list h3) == ((\"x\", 7), (4, 2);)]]")
val-true)

(check-exn exn:fail? (lambda ()
  (run "hash_to_list 3")))

(check-exn exn:fail? (lambda ()
  (run "hash_values 3")))

(check-exn exn:fail? (lambda ()
  (run "hash_keys 3")))


(check-equal? (run "h1 == h3")
val-false)

(check-equal? (run "h1 == (make_hash h3)")
val-false)

(check-equal? (run "h1 == (make_hash h1)")
val-true)

(check-equal? (run "h3 == (make_hash h1)")
val-false)

(check-equal? (run "(h3 == (make_hash h3))")
val-true)

(check-equal? (run "(h1 == h2)")
val-false)

(check-equal? (run "((make_hash ()) == (make_hash ()))")
val-true)

(check-equal? (run "((make_hash ()) == (make_hash (\"mutable\";)))")
val-false)

(check-equal? (run "((make_hash (\"mutable\";)) == (make_hash (\"mutable\";)))")
val-true)

;; lambda does not affect equal
(check-equal? (run "((make_hash (\"mutable\",(lambda () := 3);)) == (make_hash (\"mutable\";)))")
val-true)
