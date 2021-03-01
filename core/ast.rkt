#lang racket
(provide (all-defined-out))

;; type Expr =
;; | (Int i)
;; | (String s)
;; | (Float f)
;; | (Var String)
;; | (Null)
;; | (Noop)
;; | (Prim1 Op1 Expr)
;; | (Prim2 Op2 Expr Expr)
;; | (Def Var Assign1)
;; | (Let Var Assign2)
;; | (Values Expr Assign2)
;; | (Lambda Var Assign1)
;; | (In Expr)
;; | (Assign1 Expr)
;; | (Assign2 Expr In)
;; | (If Expr Then)
;; | (Then Expr Else)
;; | (Else Expr)
;; | (Cond Expr Else)
;; | (Print Expr)
;; | (Format Expr With)
;; | (Match Expr With)
;; | (Slice Expr With)
;; | (With Expr)
;; | (Eval Expr)
;; | (Error Expr)
;; | (Stat Expr Stat)
;; | (EOP)
;;
;; type Stat =
;; | (Stat Expr Stat)
;; | (EOP)

;; type Op2 = 'apply | 'cons | and | 'or | 'xor | 'cat | 'add | 'sub |
;;      'gt | 'lt | 'equal | 'ge | 'le | 'add | 'sub | 'mult |
;;      'div | 'mod | 'exp
;;
;; type Op1 = 'neg | 'pos | 'head | 'tail | 'not | 'type | 'int | 'float | 
;;      'string | 'bool | 'int? | 'float? | string? | bool? | list? | 'pair? |
;;      'null? | 'to | 'length


(struct Int (i)             #:prefab)
(struct Float (f)           #:prefab)
(struct String (s)          #:prefab)
(struct Var (v)             #:prefab)
(struct Null ()             #:prefab)
(struct Noop ()             #:prefab)
(struct Prim1 (p e)         #:prefab)
(struct Prim2 (p e1 e2)     #:prefab)
(struct Def (e1 e2)         #:prefab)
(struct Let (e1 e2)         #:prefab)
(struct Values (e1 e2)      #:prefab)
(struct Lambda (e1 e2)      #:prefab)
(struct In (e)              #:prefab)
(struct Assign1 (e)         #:prefab)
(struct Assign2 (e1 e2)     #:prefab)
(struct If (e1 e2)          #:prefab)
(struct Then (e1 e2)        #:prefab)
(struct Else (e)            #:prefab)
(struct Cond (e1 e2)        #:prefab)
(struct Print (e)           #:prefab)
(struct Format (e1 e2)      #:prefab)
(struct Match (e1 e2)       #:prefab)
(struct With (e)            #:prefab)
(struct Slice (e1 e2)       #:prefab)
(struct Eval (e)            #:prefab)
(struct Error (e)           #:prefab)
(struct Stat (e st)         #:prefab)
(struct EOP ()              #:prefab) ;; termination of all statements, end of program
