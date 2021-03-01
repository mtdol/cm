#lang racket
(require cm/core/parse-expr cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (parse-expr (tokenize-string "3 % 5"))
'#s(Prim2 mod #s(Int 3) #s(Int 5)))

(check-equal? (parse-expr (tokenize-string "3 % 5 + 1"))
'#s(Prim2 add #s(Prim2 mod #s(Int 3) #s(Int 5)) #s(Int 1)))

(check-equal? (parse-expr (tokenize-string "5 / 0"))
'#s(Prim2 div #s(Int 5) #s(Int 0)))

(check-equal? (parse-expr (tokenize-string "3 ^ 5"))
'#s(Prim2 exp #s(Int 3) #s(Int 5)))

(check-equal? (parse-expr (tokenize-string "3 % 5 +- 1"))
'#s(Prim2 add #s(Prim2 mod #s(Int 3) #s(Int 5)) #s(Prim1 neg #s(Int 1))))

(check-equal? (parse-expr (tokenize-string "-3"))
'#s(Prim1 neg #s(Int 3)))

(check-equal? (parse-expr (tokenize-string "+3"))
'#s(Prim1 pos #s(Int 3)))

(check-equal? (parse-expr (tokenize-string "1$3"))
'#s(Prim2 cat #s(Int 1) #s(Int 3)))

(check-equal? (parse-expr (tokenize-string "x$3"))
'#s(Prim2 cat #s(Var "x") #s(Int 3)))

(check-equal? (parse-expr (tokenize-string "3$x"))
'#s(Prim2 cat #s(Int 3) #s(Var "x")))

(check-equal? (parse-expr (tokenize-string "-x"))
'#s(Prim1 neg #s(Var "x")))

(check-equal? (parse-expr (tokenize-string "x"))
'#s(Var "x"))

(check-equal? (parse-expr (tokenize-string "+x / 4 + + 5"))
'#s(Prim2 add
    #s(Prim2 div #s(Prim1 pos #s(Var "x")) #s(Int 4))
    #s(Prim1 pos #s(Int 5))))

(check-equal? (parse-expr (tokenize-string "-x / 4 - - 5"))
'#s(Prim2 sub
    #s(Prim2 div #s(Prim1 neg #s(Var "x")) #s(Int 4))
    #s(Prim1 neg #s(Int 5))))

(check-equal? (parse-expr (tokenize-string "3 and 5 +- 1"))
'#s(Prim2 and #s(Int 3) #s(Prim2 add #s(Int 5) #s(Prim1 neg #s(Int 1)))))

(check-equal? (parse-expr (tokenize-string "3 or 5 +- 1"))
'#s(Prim2 or #s(Int 3) #s(Prim2 add #s(Int 5) #s(Prim1 neg #s(Int 1)))))


(check-equal? (parse-expr (tokenize-string "3 or !5 +- 1"))
'#s(Prim2 or #s(Int 3) #s(Prim2 add #s(Prim1 not #s(Int 5)) #s(Prim1 neg #s(Int 1)))))

(check-equal? (parse-expr (tokenize-string "3 xor 5 +- 1"))
'#s(Prim2 xor #s(Int 3) #s(Prim2 add #s(Int 5) #s(Prim1 neg #s(Int 1)))))

(check-equal? (parse-expr (tokenize-string "3 or 5 : 7"))
'#s(Prim2 apply #s(Prim2 or #s(Int 3) #s(Int 5)) #s(Int 7)))

(check-equal? (parse-expr (tokenize-string "3 or 5 , 7"))
'#s(Prim2 cons #s(Prim2 or #s(Int 3) #s(Int 5)) #s(Int 7)))

(check-equal? (parse-expr (tokenize-string "3 : 5 , 7"))
'#s(Prim2 apply #s(Int 3) #s(Prim2 cons #s(Int 5) #s(Int 7))))

(check-equal? (parse-expr (tokenize-string "3 : 5 + print 7"))
'#s(Prim2 apply #s(Int 3) #s(Prim2 add #s(Int 5) #s(Print #s(Int 7)))))
)