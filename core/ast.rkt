#lang racket
(provide (all-defined-out))

;; Matthew Dolinka

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
;; | (Prefix3 Pop3 Expr Expr Expr)
;; | (Def Var Assign)
;; | (Set Var Assign)
;; | (Defun Var Cons Assign)
;; | (Let Var Assign In)
;; | (Lambda Var Assign)
;; | (In Expr)
;; | (Assign Expr)
;; | (If Expr Then Else)
;; | (Then Expr)
;; | (Else Expr)
;; | (Cond Case)
;; | (Case Expr Expr Expr)
;; | (Yields Expr)
;; | (While Expr Do) 
;; | (Foreach Expr In Do) 
;; | (Do Expr) 
;; | (Try Expr Catch With)
;; | (Catch Var)
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
;; type Pop2 = 'appl | 'struct | 'struct? | 'types | 'index | 'writestr |
;; 	       'appendstr | 'cp | 'mv | 'hash_ref | 'hash_has_key?
;;
;; type Pop3 = 'hash_set | 'hash_ref_check
;;
;; type Op1 = 'neg | 'pos | 'head | 'tail | 'not | 'type | 'dynamic | 'int | 'float | 
;;      'string | 'bool | 'int? | 'float? | string? | bool? | list? | 'pair? |
;;      'null? | 'void? | 'length | 'eval | 'print | 'load | 'error | 'lang | 'apply1 |
;;      'ls | 'cd | 'mkdir | 'rm | 'system | 'sysres |
;;      'file_exists? | 'dir_exists? | 'eof? | 'make_hash | 'hash? | 'mutable_hash? |
;;      'hash_keys | 'hash_values | 'hash_to_list
;;
;; type Op0 = 'end | 'void | 'eof


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
(struct Prefix3 (p e1 e2 e3)#:prefab)
(struct Def (e1 e2)         #:prefab)
(struct Set (e1 e2)         #:prefab)
(struct Defun (e1 e2 e3)    #:prefab)
(struct Let (e1 e2 e3)      #:prefab)
(struct Lambda (e1 e2)      #:prefab)
(struct In (e)              #:prefab)
(struct Assign (e)          #:prefab)
(struct If (e1 e2 e3)       #:prefab)
(struct Then (e)            #:prefab)
(struct Else (e)            #:prefab)
(struct Cond (e)            #:prefab)
(struct Case (e1 e2 e3)     #:prefab)
(struct Yields (e)          #:prefab)
(struct While (e1 e2)       #:prefab)
(struct Foreach (e1 e2 e3)  #:prefab)
(struct Do (e)              #:prefab)
(struct Try (e1 e2 e3)      #:prefab)
(struct Catch (e)           #:prefab)
(struct With (e)            #:prefab)
(struct Match (e1 e2)       #:prefab)
(struct Typedef (e1 e2)     #:prefab)
(struct Stat (i e st)       #:prefab) ;; first item is line-number
(struct EOP ()              #:prefab) ;; termination of all statements, end of program
