#lang racket
(require cm/tests/test-utils rackunit)

;; import macros
(run-stat-silent "#:import \"std_lib::std.cm\"")

;;
;; list{}
;;

(check-equal? (run-stat "{list}.")
'(()))

(check-equal? (run-stat "{list 1}.")
'((1)))

(check-equal? (run-stat "{list 1|2|3}.")
'((1 2 3)))

(check-equal? (run-stat "9, {list 1|2|3}, 7.")
'((9 (1 2 3) . 7)))

;; parse error 
(check-exn exn:fail? (lambda ()
  (run-stat "{list |3}.")))

;;
;; empty{}
;;

(run-stat-silent "#:def:_empty_and_arg{a} {empty} {empty} a {empty}")

(check-equal? (run-stat "1 + {empty} 2.")
'(3))

(check-equal? (run-stat "{_empty_and_arg 2 + 3} - 1.")
'(4))

(check-exn exn:fail? (lambda ()
  (run-stat "1 + {empty}.")))

(check-exn exn:fail? (lambda ()
  (run-stat "{empty 3}.")))

;;
;; --{}
;;

(check-equal? (run-stat "1 + {-- 1 + \\{  } 2.")
'(3))

(check-exn exn:fail? (lambda ()
  (run-stat "{-- must close braces { }.")))

(check-equal? (run-stat "1 + {-- cool {} comment that you got! th~er\\{ } 2.")
'(3))


;;
;; flatten{}
;;

(check-equal? (run-stat "{flatten 1}.")
'(1))

(check-equal? (run-stat "{flatten 1 + | (2 | * -5)}.")
'(-9))


;;
;; vari{}
;;

(check-equal? (run-stat "{vari +|1}.")
'(1))

(check-equal? (run-stat "{vari -|1|2+1}.")
'(-2))

;; (* 1 (^ 2 3) 2)
(check-equal? (run-stat "{vari * | 1 | {vari ^ | 2 | {vari + | 1 | 2}} | 2}.")
'(16))

(check-exn exn:fail? (lambda ()
  (run-stat "{vari +}.")))

;;
;; %{}
;;

(check-equal? (run-stat "string {%}.")
'(""))

(check-equal? (run-stat "string {% 1}.")
'(""))

(run-stat-silent "def x := 3.")

(check-equal? (run-stat "string {% 1| set x := {vari -|3|5} | {% 2 | 3}}.")
'(""))

(check-equal? (run-stat "x.")
'(-2))

(check-equal? (run-stat "string {% 1| match 3 \\| int n -> set x := x + n end }.")
'(""))

(check-equal? (run-stat "x.")
'(1))


;;
;; :{}
;;


(run-stat-silent "defun _add (a,b) := a + b.")
(run-stat-silent "defun _nuffin () := 3.")

(check-equal? (run-stat "{: _add | 1 | 2}.")
'(3))

(check-equal? (run-stat "{: _add | {vari ^|2|3} | -2}.")
'(6))

(check-equal? (run-stat "{: _nuffin}.")
'(3))

(check-equal? (run-stat "{: _nuffin | \"null arg dont care\"}.")
'(3))

(check-exn exn:fail? (lambda ()
  (run-stat "{: _nuffin | 2 | 3}.")))


;;
;; when{}, unless{}
;;

(check-equal? (run-stat "string {when 1 = 2 | 3}.")
'(""))

(check-equal? (run-stat "{when 1 = 1 | 3}.")
'(3))

(check-equal? (run-stat "{unless 1 = 2 | 3}.")
'(3))

(check-equal? (run-stat "string {unless 1 = 1 | 3}.")
'(""))

;;
;; defcheck{}
;;

(run-stat-silent "def x := 7.")

(check-equal? (run-stat "string ({defcheck int|x} := 3).")
'(""))

(check-equal? (run-stat "x.")
'(7))

(check-equal? (run-stat "({defcheck int|z} := 3).")
'(3))

(check-equal? (run-stat "z.")
'(3))

(check-exn exn:fail? (lambda ()
  (run-stat "{defcheck int|x1} := 3.2.")))

(check-exn exn:fail? (lambda ()
  (run-stat "{defcheck int|x y} := 3.")))

;;
;; let{}
;;

(check-equal? (run-stat "{let x|3} in (x + 1) * 2.")
'(8))

(check-equal? (run-stat "{let x|3|y|x+1} in y + x.")
'(7))

(check-equal? (run-stat "{let x|3| y|lam () := 4| z|:>y + 1} in x - z - :>y.")
'(-6))

(check-exn exn:fail? (lambda ()
  (run-stat "{let int|3} in 1.")))

(check-exn exn:fail? (lambda ()
  (run-stat "{let x} in 1.")))

(check-exn exn:fail? (lambda ()
  (run-stat "{let x|3|z} in 1.")))

;;
;; assert_types={}
;;

(check-equal? (run-stat "{assert_types= \"main\"|{list \"int\"}|1|2} 5.")
'(5))

(check-exn exn:fail? (lambda ()
  (run-stat "{assert_types= \"main\"|{list \"int\"}|1|2.2} 5.")))

(check-equal? (run-stat "{assert_types= \"main\"|{list \"int\"|\"float\"}|1|2.2} 5.")
'(5))
