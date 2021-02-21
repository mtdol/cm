#lang racket
(provide Int Float Var String Null Prim1 Prim2 Def
         Let Lambda If Print Format Eval Error Stat EOP)

;; AST structs
(struct Int (i)             #:prefab)
(struct Float (f)           #:prefab)
(struct String (s)          #:prefab)
(struct Var (v)             #:prefab)
(struct Null ()             #:prefab)
(struct Prim1 (p e1 e2)     #:prefab)
(struct Prim2 (p e)         #:prefab)
(struct Def (v e)           #:prefab)
(struct Let (v e1 e2)       #:prefab)
(struct Lambda (v e)        #:prefab)
(struct If (e1 e2 e3)       #:prefab)
(struct Print (e)           #:prefab)
(struct Format (s l)        #:prefab)
(struct Eval (s)            #:prefab)
(struct Error (s)           #:prefab)
(struct Stat (e st)         #:prefab)
(struct EOP ()              #:prefab)
