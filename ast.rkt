#lang racket
(provide Int Float Var String Prim1 Prim2 Def Let Lambda If Print Stat EOP)

;; AST structs
(struct Int (i)             #:prefab)
(struct Float (f)           #:prefab)
(struct Var (v)             #:prefab)
(struct String (s)          #:prefab)
(struct Prim1 (p e1 e2)     #:prefab)
(struct Prim2 (p e)         #:prefab)
(struct Def (v e)           #:prefab)
(struct Let (v e1 e2)       #:prefab)
(struct Lambda (v e)        #:prefab)
(struct If (e1 e2 e3)       #:prefab)
(struct Print (e)           #:prefab)
(struct Stat (e st)         #:prefab)
(struct EOP ()              #:prefab)
