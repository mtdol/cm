#lang racket
(require cm/core/parse-expr cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (parse-expr (tokenize-string "def int x := 1 * ( 2 + 3 )"))
'#s(Def #s(Prim1 int #s(Var "x")) #s(Assign #s(Prim2 mult #s(Int 1) #s(Prim2 add #s(Int 2) #s(Int 3))))))

(check-equal? (parse-expr (tokenize-string "def int x, float y := def dynamic z := 1 * ( 2 + 3 )"))
'#s(Def #s(Prim2 cons #s(Prim1 int #s(Var "x")) #s(Prim1 float #s(Var "y"))) #s(Assign #s(Def #s(Prim1 dynamic #s(Var "z")) #s(Assign #s(Prim2 mult #s(Int 1) #s(Prim2 add #s(Int 2) #s(Int 3))))))))

(check-equal? (parse-expr (tokenize-string "def int x, y := def z := 1 * ( 2 + 3 )"))
'#s(Def #s(Prim2 cons #s(Prim1 int #s(Var "x")) #s(Var "y")) #s(Assign #s(Def #s(Var "z") #s(Assign #s(Prim2 mult #s(Int 1) #s(Prim2 add #s(Int 2) #s(Int 3))))))))

(check-equal? (parse-expr (tokenize-string "lambda x := 3"))
'#s(Lambda #s(Var "x") #s(Assign #s(Int 3))))

(check-equal? (parse-expr (tokenize-string "lambda int x := 3"))
'#s(Lambda #s(Prim1 int #s(Var "x")) #s(Assign #s(Int 3))))

(check-equal? (parse-expr (tokenize-string "lambda x,y := lam int y := 3"))
'#s(Lambda #s(Prim2 cons #s(Var "x") #s(Var "y")) #s(Assign #s(Lambda #s(Prim1 int #s(Var "y")) #s(Assign #s(Int 3))))))

(check-equal? (parse-expr (tokenize-string "def fun f := lambda x,y := lam int y := 3"))
'#s(Def #s(Prim1 fun #s(Var "f")) #s(Assign #s(Lambda #s(Prim2 cons #s(Var "x") #s(Var "y")) #s(Assign #s(Lambda #s(Prim1 int #s(Var "y")) #s(Assign #s(Int 3))))))))
)
