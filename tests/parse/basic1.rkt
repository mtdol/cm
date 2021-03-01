#lang racket
(require cm/core/parse-expr cm/core/lex)

(module+ test
           (require rackunit))

(module+ test

(check-equal? (parse-expr (tokenize-string "1 * ( 2 + 3 )"))
'#s(Prim2 mult #s(Int 1) #s(Prim2 add #s(Int 2) #s(Int 3))))

(check-equal? (parse-expr (tokenize-string "1 * 2 + 3"))
'#s(Prim2 add #s(Prim2 mult #s(Int 1) #s(Int 2)) #s(Int 3)))

(check-equal? (parse-expr (tokenize-string "( 1 * 2 ) + 3 "))
'#s(Prim2 add #s(Prim2 mult #s(Int 1) #s(Int 2)) #s(Int 3)))

(check-equal? (parse-expr (tokenize-string "1 * 2 + ( print 3 ) "))
'#s(Prim2 add #s(Prim2 mult #s(Int 1) #s(Int 2)) #s(Print #s(Int 3))))

(check-equal? (parse-expr (tokenize-string "( 1 * 2 ) + ( print 3 ) "))
'#s(Prim2 add #s(Prim2 mult #s(Int 1) #s(Int 2)) #s(Print #s(Int 3))))

(check-equal? (parse-expr (tokenize-string "print ( 1 * 2 ) + 3 "))
'#s(Print #s(Prim2 add #s(Prim2 mult #s(Int 1) #s(Int 2)) #s(Int 3))))

(check-equal? (parse-expr (tokenize-string "print 1 * ( 2 + 3 ) "))
'#s(Print #s(Prim2 mult #s(Int 1) #s(Prim2 add #s(Int 2) #s(Int 3)))))

(check-equal? (parse-expr (tokenize-string " 1 * ( print 2 + 3 ) "))
'#s(Prim2 mult #s(Int 1) #s(Print #s(Prim2 add #s(Int 2) #s(Int 3)))))

(check-equal? (parse-expr (tokenize-string "1 * ( 2 * ( 3 + 4 ) ) "))
'#s(Prim2 mult #s(Int 1) #s(Prim2 mult #s(Int 2) #s(Prim2 add #s(Int 3) #s(Int 4)))))

(check-equal? (parse-expr (tokenize-string "1 * ( print 2 * ( 3 + 4 ) ) "))
'#s(Prim2 mult #s(Int 1) #s(Print #s(Prim2 mult #s(Int 2) #s(Prim2 add #s(Int 3) #s(Int 4))))))

(check-equal? (parse-expr (tokenize-string "1 * 2 / 3"))
'#s(Prim2 mult #s(Int 1) #s(Prim2 div #s(Int 2) #s(Int 3))))

(check-equal? (parse-expr (tokenize-string "1 * 2 + 3 / 4"))
'#s(Prim2 add #s(Prim2 mult #s(Int 1) #s(Int 2)) #s(Prim2 div #s(Int 3) #s(Int 4))))

(check-equal? (parse-expr (tokenize-string "1 * 2 + ( 3 / 4 )"))
'#s(Prim2 add #s(Prim2 mult #s(Int 1) #s(Int 2)) #s(Prim2 div #s(Int 3) #s(Int 4))))

(check-equal? (parse-expr (tokenize-string "( 2 * 2 ) + 3 / 4"))
'#s(Prim2 add #s(Prim2 mult #s(Int 2) #s(Int 2)) #s(Prim2 div #s(Int 3) #s(Int 4))))

(check-equal? (parse-expr (tokenize-string "( 1 * 2 ) + ( 3 / 4 )"))
'#s(Prim2 add #s(Prim2 mult #s(Int 1) #s(Int 2)) #s(Prim2 div #s(Int 3) #s(Int 4))))

(check-equal? (parse-expr (tokenize-string "(1 * ( 2 + 3 ) / 4 )"))
'#s(Prim2 mult #s(Int 1) #s(Prim2 div #s(Prim2 add #s(Int 2) #s(Int 3)) #s(Int 4))))

)
