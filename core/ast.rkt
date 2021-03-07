#lang racket
(provide (all-defined-out))

;; type Expr =
;; | (Int i)
;; | (Float f)
;; | (Bool i)
;; | (String s)
;; | (Var s)
;; | (Null)
;; | (Void)
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
;; | (Cond Case)
;; | (Case Expr Expr Expr)
;; | (Yields Expr)
;; | (When Expr Expr)
;; | (Appl Var Expr)
;; | (Print Expr)
;; | (Format Expr With)
;; | (Match Expr With)
;; | (Slice Expr With)
;; | (Wrap Expr)
;; | (Typedef Var Assign1)
;; | (Struct Var Expr)
;; | (IsStruct Var Expr)
;; | (Eval Expr)
;; | (Error Expr)
;; | (Stat i Expr Stat)
;; | (EOP)
;;
;; type Stat =
;; | (Stat i Expr Stat)
;; | (EOP)
;;
;; type Case =
;; | (Case Expr Yields Case)
;; | (Case Expr Yields End)
;; | (Case Expr Yields Else)

;; type Op2 = 'apply | 'cons | and | 'or | 'xor | 'cat | 'add | 'sub |
;;      'gt | 'lt | 'eq | 'ge | 'le | 'add | 'sub | 'mult |
;;      'div | 'mod | 'exp
;;
;; type Op1 = 'neg | 'pos | 'head | 'tail | 'not | 'type | 'dynamic | 'int | 'float | 
;;      'string | 'bool | 'int? | 'float? | string? | bool? | list? | 'pair? |
;;      'null? | 'void? | 'length


(struct Int (i)             #:prefab)
(struct Float (f)           #:prefab)
(struct Bool (i)            #:prefab)
(struct String (s)          #:prefab)
(struct Var (v)             #:prefab)
(struct Null ()             #:prefab)
(struct Void ()             #:prefab)
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
(struct Cond (e)            #:prefab)
(struct Case (e1 e2 e3)     #:prefab)
(struct Yields (e)          #:prefab)
(struct When (e1 e2)        #:prefab)
(struct Load (e)            #:prefab)
(struct End ()              #:prefab)
(struct Print (e)           #:prefab)
(struct Format (e1 e2)      #:prefab)
(struct Match (e1 e2)       #:prefab)
(struct Wrap (e)            #:prefab)
(struct Slice (e1 e2)       #:prefab)
(struct Typedef (e1 e2)     #:prefab)
(struct Struct (e1 e2)      #:prefab)
(struct IsStruct (e1 e2)    #:prefab)
(struct Appl (e1 e2)        #:prefab)
(struct Eval (e)            #:prefab)
(struct Error (e)           #:prefab)
(struct Stat (i e st)       #:prefab) ;; first item is line-number
(struct EOP ()              #:prefab) ;; termination of all statements, end of program
