#lang racket
(require cm/tests/test-utils cm/core/ast rackunit)

(run-silent "#:lang cm")

(run-silent "def s1 := :>make_set")
(run-silent "def s1_2 := :>make_set")
(run-silent "def s2 := :>make_mutable_set")
(run-silent "def s2_2 := :>make_mutable_set")

; calls `set_to_list` and sorts all entries as chars
(run-silent "defun sort.set_to_list (struct Set s) :=
              sort : char_to_int : (set_to_list : s)")

(check-equal? (run "struct? Set s1")
val-true)

(check-equal? (run "struct? Set s2")
val-true)

(check-equal? (run "mutable_set? : s1")
val-false)

(check-equal? (run "mutable_set? : s2")
val-true)

(check-equal? (run "s1 == s1_2")
val-true)

(check-equal? (run "s1 == s2")
val-false)

(check-equal? (run "s2 == s2_2")
val-true)

(check-equal? (run "set_equal? : s1 : s1_2")
val-true)

(check-equal? (run "set_equal? : s1 : s2")
val-false)

(check-equal? (run "set_equal? : s2 : s2_2")
val-true)


(check-equal? (run "sort.set_to_list : s1")
'())

(check-equal? (run "sort.set_to_list : s2")
'())


(check-equal? (run "set_size : s1")
0)

(check-equal? (run "set_size : s2")
0)

; add "a"

(run-silent "def s1 := set_add : s1 : \"a\"")
(run-silent "def s1_2 := set_add : s1_2 : \"a\"")
(run-silent "set_add : s2 : \"a\"")
(run-silent "set_add : s2_2 : \"a\"")


(check-equal? (run "s1 == s1_2")
val-true)

(check-equal? (run "s1 == s2")
val-false)

(check-equal? (run "s2 == s2_2")
val-true)

(check-equal? (run "set_equal? : s1 : s1_2")
val-true)

(check-equal? (run "set_equal? : s1 : s2")
val-false)

(check-equal? (run "set_equal? : s2 : s2_2")
val-true)


(check-equal? (run "sort.set_to_list : s1")
'("a"))

(check-equal? (run "sort.set_to_list : s2")
'("a"))


(check-equal? (run "set_size : s1")
1)

(check-equal? (run "set_size : s2")
1)


; add "b"

(run-silent "def s1 := set_add : s1 : \"b\"")
(run-silent "def s1_2 := set_add : s1_2 : \"b\"")
(run-silent "set_add : s2 : \"b\"")
(run-silent "set_add : s2_2 : \"b\"")

(check-equal? (run "s1 == s1_2")
val-true)

(check-equal? (run "s2 == s2_2")
val-true)

(check-equal? (run "set_equal? : s1 : s1_2")
val-true)

(check-equal? (run "set_equal? : s2 : s2_2")
val-true)


(check-equal? (run "sort.set_to_list : s1")
'("a" "b"))

(check-equal? (run "sort.set_to_list : s2")
'("a" "b"))


(check-equal? (run "set_size : s1")
2)

(check-equal? (run "set_size : s2")
2)


; add "a" again

(run-silent "def s1 := set_add : s1 : \"a\"")
(run-silent "def s1_2 := set_add : s1_2 : \"a\"")
(run-silent "set_add : s2 : \"a\"")
(run-silent "set_add : s2_2 : \"a\"")


(check-equal? (run "s1 == s1_2")
val-true)

(check-equal? (run "s2 == s2_2")
val-true)

(check-equal? (run "set_equal? : s1 : s1_2")
val-true)

(check-equal? (run "set_equal? : s2 : s2_2")
val-true)


(check-equal? (run "sort.set_to_list : s1")
'("a" "b"))

(check-equal? (run "sort.set_to_list : s2")
'("a" "b"))


(check-equal? (run "set_size : s1")
2)

(check-equal? (run "set_size : s2")
2)

; add "c" only to `s2` and `s1`

(run-silent "set_add : s2 : \"c\"")
(run-silent "def s1 := set_add : s1 : \"c\"")

(check-equal? (run "s1 == s1_2")
val-false)

(check-equal? (run "s2 == s2_2")
val-false)

(check-equal? (run "set_equal? : s1 : s1_2")
val-false)

(check-equal? (run "set_equal? : s2 : s2_2")
val-false)


(check-equal? (run "sort.set_to_list : s2")
'("a" "b" "c"))

(check-equal? (run "set_size : s2")
3)

; membership

(check-equal? (run "set_member? : s1 : \"z\"")
val-false)

(check-equal? (run "set_member? : s2 : \"z\"")
val-false)

(check-equal? (run "set_member? : s1 : \"a\"")
val-true)

(check-equal? (run "set_member? : s2 : \"a\"")
val-true)

(check-equal? (run "set_member? : s1 : \"b\"")
val-true)

(check-equal? (run "set_member? : s2 : \"b\"")
val-true)

(check-equal? (run "set_member? : s1 : \"c\"")
val-true)

(check-equal? (run "set_member? : s1_2 : \"c\"")
val-false)

(check-equal? (run "set_member? : s2 : \"c\"")
val-true)

(check-equal? (run "set_member? : s2_2 : \"c\"")
val-false)

; removal

(run-silent "def s1 := set_remove : s1 : \"b\"")
(run-silent "def s1_2 := set_remove : s1_2 : \"b\"")
(run-silent "set_remove : s2 : \"b\"")
(run-silent "set_remove : s2_2 : \"b\"")

(check-equal? (run "set_member? : s1 : \"b\"")
val-false)

(check-equal? (run "set_member? : s1_2 : \"b\"")
val-false)

(check-equal? (run "set_member? : s2 : \"b\"")
val-false)

(check-equal? (run "set_member? : s2_2 : \"b\"")
val-false)

(check-equal? (run "sort.set_to_list : s1")
'("a" "c"))

(check-equal? (run "sort.set_to_list : s1_2")
'("a"))

(check-equal? (run "sort.set_to_list : s2")
'("a" "c"))

(check-equal? (run "sort.set_to_list : s2_2")
'("a"))

;;
;; list_to_set, list_to_mutable_set
;;

(run-silent "def s3 := list_to_set : {list \"a\"|\"b\"}")
(run-silent "def s4 := list_to_mutable_set : {list \"a\"|\"b\"}")

(check-equal? (run "s3 == (set_add : (set_add : (:>make_set) : \"a\") : \"b\")")
val-true)

(check-equal? (run "let s := :>make_mutable_set in
                      (set_add : s : \"a\") comma
                      (set_add : s : \"b\") comma
                      s4 == s")
val-true)

(check-equal? (run "(:>make_set) == (list_to_set : {list})")
val-true)

(check-equal? (run "(:>make_mutable_set) == (list_to_mutable_set : {list})")
val-true)
