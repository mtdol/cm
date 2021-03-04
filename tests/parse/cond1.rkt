#lang racket
(require cm/core/parse-expr cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (parse-expr (tokenize-string "cond | true -> 2 else 3"))
'#s(Cond #s(Case #s(Bool 1) #s(Yields #s(Int 2)) #s(Else #s(Int 3)))))

(check-equal? (parse-expr (tokenize-string "| true -> 2 else 3"))
'#s(Case #s(Bool 1) #s(Yields #s(Int 2)) #s(Else #s(Int 3))))

(check-equal? (parse-expr (tokenize-string "| 4 -> 2 else 3"))
'#s(Case #s(Int 4) #s(Yields #s(Int 2)) #s(Else #s(Int 3))))

(check-equal? (parse-expr (tokenize-string "cond | true -> 2 | 3 -> 4 else 5"))
'#s(Cond #s(Case #s(Bool 1) #s(Yields #s(Int 2)) #s(Case #s(Int 3) #s(Yields #s(Int 4)) #s(Else #s(Int 5))))))

(check-equal? (parse-expr (tokenize-string "| true -> 2 | 3 -> 4 else 5"))
'#s(Case #s(Bool 1) #s(Yields #s(Int 2)) #s(Case #s(Int 3) #s(Yields #s(Int 4)) #s(Else #s(Int 5)))))

(check-equal? (parse-expr (tokenize-string "1 + | bool 0 -> 3 else 4"))
'#s(Prim2 add #s(Int 1) #s(Case #s(Prim1 bool #s(Int 0)) #s(Yields #s(Int 3)) #s(Else #s(Int 4)))))

(check-equal? (parse-expr (tokenize-string "(| bool 0 -> 2 else 3) + 5"))
'#s(Prim2 add #s(Case #s(Prim1 bool #s(Int 0)) #s(Yields #s(Int 2)) #s(Else #s(Int 3))) #s(Int 5)))

(check-equal? (parse-expr (tokenize-string "| bool 0 -> 2 else 3 + 5"))
'#s(Case #s(Prim1 bool #s(Int 0)) #s(Yields #s(Int 2)) #s(Else #s(Prim2 add #s(Int 3) #s(Int 5)))))
)
