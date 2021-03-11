#lang racket
(provide (all-defined-out))

;; type Expr =
;; | (Int i)
;; | (Float f)
;; | (Bool i)
;; | (String s)
;; | (Var s)
;; | (Null)
;; | (Prim0 Op0)
;; | (Prim1 Op1 Expr)
;; | (Prim2 Op2 Expr Expr)
;; | (Prefix2 Pop2 Expr Expr)
;; | (Def Var Assign1)
;; | (Let Var Assign2)
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
;; | (Try Expr Catch)
;; | (Catch Var With)
;; | (With Expr)
;; | (Match Expr With)
;; | (Typedef Var Assign1)
;; | (IsStruct Var Expr)
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
;;      'div | 'mod | 'exp | 'when | 'eqq | 'neqq
;;
;; type Pop2 = 'appl | 'struct | 'struct?
;;
;; type Op1 = 'neg | 'pos | 'head | 'tail | 'not | 'type | 'dynamic | 'int | 'float | 
;;      'string | 'bool | 'int? | 'float? | string? | bool? | list? | 'pair? |
;;      'null? | 'void? | 'length | 'eval | 'print | 'load | 'error
;;
;; type Op0 = 'end | 'void


(struct Int (i)             #:prefab)
(struct Float (f)           #:prefab)
(struct Bool (i)            #:prefab)
(struct String (s)          #:prefab)
(struct Var (id)            #:prefab)
(struct Null ()             #:prefab)
(struct Prim0 (p)           #:prefab)
(struct Prim1 (p e)         #:prefab)
(struct Prim2 (p e1 e2)     #:prefab)
(struct Prefix2 (p e1 e2)   #:prefab)
(struct Def (e1 e2)         #:prefab)
(struct Let (e1 e2)         #:prefab)
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
(struct Try (e1 e2)         #:prefab)
(struct Catch (e1 e2)       #:prefab)
(struct With (e)            #:prefab)
(struct Match (e1 e2)       #:prefab)
(struct Typedef (e1 e2)     #:prefab)
(struct Stat (i e st)       #:prefab) ;; first item is line-number
(struct EOP ()              #:prefab) ;; termination of all statements, end of program
