#lang racket
(require cm/core/parse-expr cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (parse-expr (tokenize-string "let x := 3 in 5"))
'#s(Let #s(Var "x") #s(Assign #s(Int 3)) #s(In #s(Int 5))))

(check-equal? (parse-expr (tokenize-string "let x := 3 + 7 in -7 + x"))
'#s(Let #s(Var "x") #s(Assign #s(Prim2 add #s(Int 3) #s(Int 7))) #s(In #s(Prim2 add #s(Prim1 neg #s(Int 7)) #s(Var "x")))))

(check-equal? (parse-expr (tokenize-string "let x := 1 + let y := -4 in y + 2 in x - 5"))
'#s(Let #s(Var "x") #s(Assign #s(Prim2 add #s(Int 1) #s(Let #s(Var "y") #s(Assign #s(Prim1 neg #s(Int 4))) #s(In #s(Prim2 add #s(Var "y") #s(Int 2)))))) #s(In #s(Prim2 sub #s(Var "x") #s(Int 5)))))
)
