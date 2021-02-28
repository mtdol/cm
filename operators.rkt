#lang racket
(provide (all-defined-out))

(define max-precedences 9)

;; arity is op arity, position is "prefix" or "infix", precedence starts from
;; zero, where zero is lowest precedence
(struct OpData (arity pos preced node-name))

(define op-data 
    (make-hash (list
        (cons "print"       (OpData 1 "prefix"  0   'Print))
        (cons "format"      (OpData 2 "prefix"  0   'Format))
        (cons "slice"       (OpData 2 "prefix"  0   'Slice))
        (cons "eval"        (OpData 1 "prefix"  0   'Eval))
        (cons "error"       (OpData 1 "prefix"  0   'Error))
        (cons "match"       (OpData 2 "prefix"  0   'Match))
        (cons "if"          (OpData 2 "prefix"  0   'If))
        (cons "then"        (OpData 2 "prefix"  0   'Then))
        (cons "else"        (OpData 1 "prefix"  0   'Else))
        (cons "cond"        (OpData 2 "prefix"  0   'Cond))
        (cons "let"         (OpData 2 "prefix"  0   'Let))
        (cons "values"      (OpData 2 "prefix"  0   'Values))
        (cons "def"         (OpData 2 "prefix"  0   'Def))
        (cons "lambda"      (OpData 2 "prefix"  0   'Lambda))
        (cons "in"          (OpData 1 "prefix"  0   'In))
        (cons "with"        (OpData 1 "prefix"  0   'With))
        (cons ":assign1"    (OpData 1 "prefix"  0   'Assign1))
        (cons ":assign2"    (OpData 2 "prefix"  0   'Assign2))
        (cons "apply"       (OpData 2 "infix"   1   '(Prim2 apply)))
        (cons "cons"        (OpData 2 "infix"   2   '(Prim2 cons)))
        (cons "and"         (OpData 2 "infix"   3   '(Prim2 and)))
        (cons "or"          (OpData 2 "infix"   3   '(Prim2 or)))
        (cons "xor"         (OpData 2 "infix"   3   '(Prim2 xor)))
        (cons "cat"         (OpData 2 "infix"   3   '(Prim2 cat)))
        (cons "gt"          (OpData 2 "infix"   4   '(Prim2 gt)))
        (cons "lt"          (OpData 2 "infix"   4   '(Prim2 lt)))
        (cons "ge"          (OpData 2 "infix"   4   '(Prim2 ge)))
        (cons "le"          (OpData 2 "infix"   4   '(Prim2 le)))
        (cons "equal"       (OpData 2 "infix"   4   '(Prim2 eq)))
        (cons "plus"        (OpData 2 "infix"   5   '(Prim2 add)))
        (cons "minus"       (OpData 2 "infix"   5   '(Prim2 sub)))
        (cons "mult"        (OpData 2 "infix"   6   '(Prim2 mult)))
        (cons "div"         (OpData 2 "infix"   6   '(Prim2 div)))
        (cons "mod"         (OpData 2 "infix"   6   '(Prim2 mod)))
        (cons "exp"         (OpData 2 "infix"   7   '(Prim2 exp)))
        (cons "head"        (OpData 1 "prefix"  8   '(Prim1 head)))
        (cons "tail"        (OpData 1 "prefix"  8   '(Prim1 tail)))
        (cons "not"         (OpData 1 "prefix"  8   '(Prim1 not)))
        (cons "type"        (OpData 1 "prefix"  8   '(Prim1 type)))
        (cons "int"         (OpData 1 "prefix"  8   '(Prim1 int)))
        (cons "int?"        (OpData 1 "prefix"  8   '(Prim1 int?)))
        (cons "float"       (OpData 1 "prefix"  8   '(Prim1 float)))
        (cons "float?"      (OpData 1 "prefix"  8   '(Prim1 float?)))
        (cons "string"      (OpData 1 "prefix"  8   '(Prim1 string)))
        (cons "string?"     (OpData 1 "prefix"  8   '(Prim1 string?)))
        (cons "bool"        (OpData 1 "prefix"  8   '(Prim1 bool)))
        (cons "bool?"       (OpData 1 "prefix"  8   '(Prim1 bool?)))
        (cons "list"        (OpData 1 "prefix"  8   '(Prim1 list)))
        (cons "list?"       (OpData 1 "prefix"  8   '(Prim1 list?)))
        (cons "pair?"       (OpData 1 "prefix"  8   '(Prim1 pair?)))
        (cons "fun?"        (OpData 1 "prefix"  8   '(Prim1 fun?)))
        (cons "null?"       (OpData 1 "prefix"  8   '(Prim1 null?)))
        (cons "to"          (OpData 1 "prefix"  8   '(Prim1 to)))
        (cons "length"      (OpData 1 "prefix"  8   '(Prim1 length)))
        (cons ":uni_plus"   (OpData 1 "prefix"  8   '(Prim1 pos)))
        (cons ":uni_minus"  (OpData 1 "prefix"  8   '(Prim1 neg)))

        )))


;; these functions parse the above hash

;; returns -1 if op not present, else returns arity
(define (op-to-arity op) 
        (match (hash-ref op-data op (lambda () #f))
               [#f -1]
               [res (match res [(OpData arity _ _ _) arity])]))

;; returns "prefix" or "infix"
(define (op-to-position op)
        (match (hash-ref op-data op (lambda () #f))
               [#f -1]
               [res (match res [(OpData _ pos _ _) pos])]))

;; returns the operator's precedence
(define (op-to-precedence op)
        (match (hash-ref op-data op (lambda () #f))
               [#f -1]
               [res (match res [(OpData _ _ preced _) preced])]))

(define (op-to-node-name op)
        (match (hash-ref op-data op (lambda () #f))
               [#f -1]
               [res (match res [(OpData _ _ _ node-name) node-name])]))

;; checks if operator exists in the hash table
(define (is-operator? v)
    (hash-has-key? op-data v))
