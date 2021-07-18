#lang racket
(require cm/tests/test-utils cm/core/ast rackunit)

(run-silent "#:lang cm")

(define-syntax-rule (union-test s1 s2 res)
 (check-equal? 
   (run (format 
    "sort : value : (set_to_list :
      (set_union : (list_to_set : ~a) 
             : (list_to_set : ~a)))"
          s1 s2)) res))

(define-syntax-rule (intersect-test s1 s2 res)
 (check-equal? 
   (run (format 
    "sort : value : (set_to_list :
      (set_intersect : (list_to_set : ~a) 
             : (list_to_set : ~a)))"
          s1 s2)) res))

;; union

(union-test "{list 1|2|3}" "{list 4|5}" (list 1 2 3 4 5))

(union-test "{list 1|2|3}" "{list 3|4|5}" (list 1 2 3 4 5))

(union-test "{list}" "{list 3|4|5}" (list 3 4 5))

(union-test "{list 1|2|3}" "{list}" (list 1 2 3))

;; intersection

(intersect-test "{list 1|2|3}" "{list 4|5}" (list))

(intersect-test "{list 1|2|3}" "{list 3|5}" (list 3))

(intersect-test "{list 1|5|3}" "{list 3|5}" (list 3 5))

(intersect-test "{list 1|5|3}" "{list}" (list))

(intersect-test "{list}" "{list 1|2}" (list))
