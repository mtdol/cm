#lang racket
(provide (all-defined-out))

;; type Expr =
;; | (Int i)
;; | (String s)
;; | (Float f)
;; | (Var String)
;; | (Null)
;; | (Prim1 Op1 Expr Expr)
;; | (Prim2 Op2 Expr)
;; | (Def Var Expr)
;; | (Let Var Expr Expr)
;; | (Values Prim1 Expr Expr)
;; | (Lambda Var Expr)
;; | (If Expr Expr Expr)
;; | (Cond Expr Expr)
;; | (Print Expr)
;; | (Format Expr Expr)
;; | (Match Expr Expr)
;; | (Index Expr Expr)
;; | (Eval Expr)
;; | (Error Expr)
;; | (Stat Expr Stat)
;; | (EOP)
;;
;; type Stat =
;; | (Stat Expr Stat)
;; | (EOP)

;; type Op1 = 'apply | 'cons | and | 'or | 'cat | 'add | 'sub | 'gt | 'lt |
;;      'equal | 'ge | 'le | 'add | 'sub | 'mult | 'div | 'mod | 'exp
;;
;; type Op2 = 'neg | 'pos | 'head | 'tail | 'not | 'type | 'int | 'float | 
;;      'string | 'bool | 'int? | 'float? | string? | bool? | list? | 'pair? |
;;      'null | 'length


(struct Int (i)             #:prefab)
(struct Float (f)           #:prefab)
(struct String (s)          #:prefab)
(struct Var (v)             #:prefab)
(struct Null ()             #:prefab)
(struct Prim1 (p e1 e2)     #:prefab)
(struct Prim2 (p e)         #:prefab)
(struct Def (v e)           #:prefab)
(struct Let (v e1 e2)       #:prefab)
(struct Values (e1 e2 e3)   #:prefab)
(struct Lambda (v e)        #:prefab)
(struct If (e1 e2 e3)       #:prefab)
(struct Cond (e1 e2)        #:prefab)
(struct Print (e)           #:prefab)
(struct Format (e l)        #:prefab)
(struct Match (e l)         #:prefab)
(struct Index (e1 e2)       #:prefab)
(struct Eval (e)            #:prefab)
(struct Error (e)           #:prefab)
(struct Stat (e st)         #:prefab)
(struct EOP ()              #:prefab) ;; termination of all statements, end of program
