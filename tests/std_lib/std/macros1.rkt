#lang racket
(require cm/tests/test-utils rackunit)

;; import macros
(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; list{}
;;

(check-equal? (run-stat "{list}//")
'(()))

(check-equal? (run-stat "{list 1}//")
'((1)))

(check-equal? (run-stat "{list 1|2|3}//")
'((1 2 3)))

(check-equal? (run-stat "9, {list 1|2|3}, 7//")
'((9 (1 2 3) . 7)))

;; parse error 
(check-failure run-stat "{list |3}//")

;;
;; listc{}
;;

(check-equal? (run-stat "{listc x|x|{list 1|2|3}}//")
'((1 2 3)))

(check-equal? (run-stat "{listc x+1|x|{list 1|2|3}}//")
'((2 3 4)))

(check-equal? (run-stat "{listc x-y|x,y|{list 1,3|3,2|5,2}}//")
'((-2 1 3)))

(check-equal? (run-stat "{listc y,x | x,y | zip : {list 1|3|5} : {list 2|4|6}}//")
'(((2 . 1) (4 . 3) (6 . 5))))

(check-failure run-stat "{listc x-z|x,y|{list 1,3|3,2|5,2}}//")

(check-failure run-stat "{listc {empty}|x,y|{list 1,3|3,2|5,2}}//")

(check-failure run-stat "{listc x|{empty}|{list 1,3|3,2|5,2}}//")

(check-failure run-stat "{listc x|x|{empty}}//")

(check-failure run-stat "{listc x-z|x,y,z|{list 1,3|3,2|5,2}}//")

;;
;; empty{}
;;

(run-stat-silent "#:def:_empty_and_arg{a} {empty} {empty} a {empty}")

(check-equal? (run-stat "1 + {empty} 2//")
'(3))

(check-equal? (run-stat "{_empty_and_arg 2 + 3} - 1//")
'(4))

(check-failure run-stat "1 + {empty}//")

(check-failure run-stat "{empty 3}//")

;;
;; --{}
;;

(check-equal? (run-stat "1 + {-- 1 + \\{  } 2//")
'(3))

(check-failure run-stat "{-- must close braces { }//")

(check-equal? (run-stat "1 + {-- cool \\{\\} comment that you got! th~er\\{ } 2//")
'(3))


;;
;; flatten{}
;;

(check-equal? (run-stat "{flatten 1}//")
'(1))

(check-equal? (run-stat "{flatten 1 + | (2 | * -5)}//")
'(-9))


;;
;; vari{}
;;

(check-equal? (run-stat "{vari +|1}//")
'(1))

(check-equal? (run-stat "{vari -|1|2+1}//")
'(-2))

;; (* 1 (^ 2 3) 2)
(check-equal? (run-stat "{vari * | 1 | {vari ^ | 2 | {vari + | 1 | 2}} | 2}//")
'(16))

(check-failure run-stat "{vari +}//")

;;
;; %{}
;;

(check-equal? (run-stat "string {%}//")
'(""))

(check-equal? (run-stat "string {% 1}//")
'(""))

(run-stat-silent "def x := 3//")

(check-equal? (run-stat "string {% 1| set x := {vari -|3|5} | {% 2 | 3}}//")
'(""))

(check-equal? (run-stat "x//")
'(-2))

(check-equal? (run-stat "string {% 1| match 3 \\| int n -> set x := x + n end }//")
'(""))

(check-equal? (run-stat "x//")
'(1))


;;
;; :{}
;;


(run-stat-silent "defun _add (a,b) := a + b//")
(run-stat-silent "defun _nuffin () := 3//")

(check-equal? (run-stat "{: _add | 1 | 2}//")
'(3))

(check-equal? (run-stat "{: _add | {vari ^|2|3} | -2}//")
'(6))

(check-equal? (run-stat "{: _nuffin}//")
'(3))

(check-failure run-stat "{: _nuffin | 2}//")

(check-failure run-stat "{: _nuffin | 2 | 3}//")


;;
;; when{}, unless{}
;;

(check-equal? (run-stat "string {when 1 = 2 | 3}//")
'(""))

(check-equal? (run-stat "{when 1 = 1 | 3}//")
'(3))

(check-equal? (run-stat "{unless 1 = 2 | 3}//")
'(3))

(check-equal? (run-stat "string {unless 1 = 1 | 3}//")
'(""))

;;
;; let{}
;;

(check-equal? (run-stat "{let x|3} in (x + 1) * 2//")
'(8))

(check-equal? (run-stat "{let x|3|y|x+1} in y + x//")
'(7))

(check-equal? (run-stat "{let x|3| y| \\() -> 4| z|:>y + 1} in x - z - :>y//")
'(-6))

(check-failure run-stat "{let int|3} in 1//")

(check-failure run-stat "{let x} in 1//")

(check-failure run-stat "{let x|3|z} in 1//")

;;
;; assert_types={}
;;

(check-equal? (run-stat "{assert_types= \"main\"|{list \"int\"}|1|2} 5//")
'(5))

(check-failure run-stat "{assert_types= \"main\"|{list \"int\"}|1|2.2} 5//")

(check-equal? (run-stat "{assert_types= \"main\"|{list \"int\"|\"float\"}|1|2.2} 5//")
'(5))
