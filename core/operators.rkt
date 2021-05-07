#lang racket
(provide (all-defined-out))

;; Matthew Dolinka
;;

(define max-precedences 8)

;; arity is op arity, position is "prefix" or "infix", precedence starts from
;; zero, where zero is lowest precedence
(struct OpData (arity pos preced node-name extra-node-name))

;; note that prefix2 operators must have highest precedence
(define op-data 
  (make-hash (list
    (cons "print"           (OpData 1 "prefix"  0   'Prim1   'print))
    (cons "match"           (OpData 2 "prefix"  0   'Match   '()))
    (cons "if"              (OpData 3 "prefix"  0   'If      '()))
    (cons "then"            (OpData 1 "prefix"  0   'Then    '()))
    (cons "else"            (OpData 1 "prefix"  0   'Else    '()))
    (cons "cond"            (OpData 1 "prefix"  0   'Cond    '()))
    (cons "try"             (OpData 3 "prefix"  0   'Try     '()))
    (cons "catch"           (OpData 1 "prefix"  0   'Catch   '()))
    (cons "with"            (OpData 1 "prefix"  0   'With    '()))
    (cons "let"             (OpData 3 "prefix"  0   'Let     '()))
    (cons "letaux"          (OpData 2 "prefix"  0   'Letaux  '()))
    (cons "param"           (OpData 3 "prefix"  0   'Param   '()))
    (cons "letrec"          (OpData 4 "prefix"  0   'Letrec  '()))
    (cons "def"             (OpData 2 "prefix"  0   'Def     '()))
    (cons "static"          (OpData 2 "prefix"  0   'Static  '()))
    (cons "set"             (OpData 2 "prefix"  0   'Set     '()))
    (cons "lambda"          (OpData 2 "prefix"  0   'Lambda  '()))
    (cons "in"              (OpData 1 "prefix"  0   'In      '()))
    (cons "->"              (OpData 1 "prefix"  0   'Yields  '()))
    (cons "|"               (OpData 3 "prefix"  0   'Case    '()))
    ;; alias for case (escaped case)
    (cons "\\|"             (OpData 3 "prefix"  0   'Case    '()))
    (cons "while"           (OpData 2 "prefix"  0   'While   '()))
    (cons "foreach"         (OpData 3 "prefix"  0   'Foreach '()))
    (cons "do"              (OpData 1 "prefix"  0   'Do      '()))
    (cons "when"            (OpData 2 "infix"   0   'Prim2   'when))
    (cons "appl"            (OpData 2 "prefix"  0   'Prefix2 'appl))
    (cons ":="              (OpData 1 "prefix"  0   'Assign  '()))
    (cons "typedef"         (OpData 2 "prefix"  0   'Typedef '()))
    (cons "comma"           (OpData 2 "infix"   0   'Prim2   'comma))
    (cons ","               (OpData 2 "infix"   1   'Prim2   'cons))
    (cons "and"             (OpData 2 "infix"   2   'Prim2   'and))
    (cons "or"              (OpData 2 "infix"   2   'Prim2   'or))
    (cons "xor"             (OpData 2 "infix"   2   'Prim2   'xor))
    (cons "$"               (OpData 2 "infix"   2   'Prim2   '$))
    (cons ">"               (OpData 2 "infix"   3   'Prim2   '>))
    (cons "<"               (OpData 2 "infix"   3   'Prim2   '<))
    (cons ">="              (OpData 2 "infix"   3   'Prim2   '>=))
    (cons "<="              (OpData 2 "infix"   3   'Prim2   '<=))
    (cons "="               (OpData 2 "infix"   3   'Prim2   '=))
    (cons "=="              (OpData 2 "infix"   3   'Prim2   '==))
    (cons "!="              (OpData 2 "infix"   3   'Prim2   '!=))
    (cons "!=="             (OpData 2 "infix"   3   'Prim2   '!==))
    (cons "+"               (OpData 2 "infix"   4   'Prim2   '+))
    (cons "-"               (OpData 2 "infix"   4   'Prim2   '-))
    (cons "*"               (OpData 2 "infix"   5   'Prim2   '*))
    (cons "/"               (OpData 2 "infix"   5   'Prim2   '/))
    (cons "%"               (OpData 2 "infix"   5   'Prim2   '%))
    (cons "^"               (OpData 2 "infix"   6   'Prim2   '^))
    (cons "::"              (OpData 2 "infix"   6   'Prim2   '::))
    (cons ":"               (OpData 2 "infix"   6   'Prim2   ':))
    (cons "head"            (OpData 1 "prefix"  7   'Prim1   'head))
    (cons "tail"            (OpData 1 "prefix"  7   'Prim1   'tail))
    (cons "not"             (OpData 1 "prefix"  7   'Prim1   'not))
    (cons "typeof"          (OpData 1 "prefix"  7   'Prim1   'typeof))
    (cons "dynamic"         (OpData 1 "prefix"  7   'Prim1   'dynamic))
    (cons "int"             (OpData 1 "prefix"  7   'Prim1   'int))
    (cons "int?"            (OpData 1 "prefix"  7   'Prim1   'int?))
    (cons "float"           (OpData 1 "prefix"  7   'Prim1   'float))
    (cons "float?"          (OpData 1 "prefix"  7   'Prim1   'float?))
    (cons "string"          (OpData 1 "prefix"  7   'Prim1   'string))
    (cons "string?"         (OpData 1 "prefix"  7   'Prim1   'string?))
    (cons "bool"            (OpData 1 "prefix"  7   'Prim1   'bool))
    (cons "bool?"           (OpData 1 "prefix"  7   'Prim1   'bool?))
    (cons "list"            (OpData 1 "prefix"  7   'Prim1   'list))
    (cons "list?"           (OpData 1 "prefix"  7   'Prim1   'list?))
    (cons "pair"            (OpData 1 "prefix"  7   'Prim1   'pair))
    (cons "pair?"           (OpData 1 "prefix"  7   'Prim1   'pair?))
    (cons "fun"             (OpData 1 "prefix"  7   'Prim1   'fun))
    (cons "fun?"            (OpData 1 "prefix"  7   'Prim1   'fun?))
    (cons "null?"           (OpData 1 "prefix"  7   'Prim1   'null?))
    (cons "void?"           (OpData 1 "prefix"  7   'Prim1   'void?))
    (cons "eof?"            (OpData 1 "prefix"  7   'Prim1   'eof?))
    (cons "var"             (OpData 1 "prefix"  7   'Prim1   'var))
    (cons "get_param"       (OpData 1 "prefix"  7   'Prim1   'get_param))
    (cons "length"          (OpData 1 "prefix"  7   'Prim1   'length))
    (cons ":uni_+"          (OpData 1 "prefix"  7   'Prim1   'pos))
    (cons ":uni_-"          (OpData 1 "prefix"  7   'Prim1   'neg))
    (cons ":>"              (OpData 1 "prefix"  7   'Prim1   ':>))
    (cons "schemaof"        (OpData 1 "prefix"  7   'Prim1   'schemaof))
    (cons "local.defined?"  (OpData 1 "prefix"  7   'Prim1   'local.defined?))
    (cons "params.defined?" (OpData 1 "prefix"  7   'Prim1   'params.defined?))
    (cons "global.defined?" (OpData 2 "prefix"  7   'Prefix2 'global.defined?))
    (cons "defined?"        (OpData 2 "prefix"  7   'Prefix2 'defined?))
    (cons "load"            (OpData 2 "prefix"  7   'Prefix2 'load))
    (cons "struct"          (OpData 2 "prefix"  7   'Prefix2 'struct))
    (cons "struct?"         (OpData 2 "prefix"  7   'Prefix2 'struct?))
    (cons "types"           (OpData 2 "prefix"  7   'Prefix2 'types))
    (cons "defun"           (OpData 3 "prefix"  7   'Defun   '()))
    (cons "index"           (OpData 2 "prefix"  7   'Prefix2 'index))
    (cons "internal_op"     (OpData 2 "prefix"  7   'Prefix2 'internal_op))
    (cons "?"               (OpData 2 "prefix"  7   'Prefix2 '?))

        )))


;; these functions parse the above hash

;; returns #f if op not present, else returns arity
(define (op-to-arity op) 
        (match (hash-ref op-data op (lambda () #f))
               [#f #f]
               [res (match res [(OpData arity _ _ _ _) arity])]))

;; returns "prefix" or "infix"
(define (op-to-position op)
        (match (hash-ref op-data op (lambda () #f))
               [#f #f]
               [res (match res [(OpData _ pos _ _ _) pos])]))

;; returns the operator's precedence
(define (op-to-precedence op)
        (match (hash-ref op-data op (lambda () #f))
               [#f #f]
               [res (match res [(OpData _ _ preced _ _) preced])]))

(define (op-to-node-name op)
        (match (hash-ref op-data op (lambda () #f))
               [#f #f]
               [res (match res [(OpData _ _ _ node-name _) node-name])]))

(define (op-to-extra-node-name op)
        (match (hash-ref op-data op (lambda () #f))
               [#f #f]
               [res (match res [(OpData _ _ _ _ extra-node-name) extra-node-name])]))

;; checks if operator exists in the hash table
(define (is-operator? v)
    (hash-has-key? op-data v))
