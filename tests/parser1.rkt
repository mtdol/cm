(parse-expr (list "1" "*" "(" "2" "+" "3" ")" ))
'#s(Prim1 mult #s(Int 1) #s(Prim1 add #s(Int 2) #s(Int 3)))

(parse-expr (list "1" "*" "2" "+" "3"))
'#s(Prim1 add #s(Prim1 mult #s(Int 1) #s(Int 2)) #s(Int 3))

(parse-expr (list "(" "1" "*" "2" ")" "+" "3" ))
'#s(Prim1 add #s(Prim1 mult #s(Int 1) #s(Int 2)) #s(Int 3))

(parse-expr (list "1" "*" "2" "+" "(" "print" "3" ")" ))
'#s(Prim1 add #s(Prim1 mult #s(Int 1) #s(Int 2)) #s(Print #s(Int 3)))

(parse-expr (list "(" "1" "*" "2" ")" "+" "(" "print" "3" ")" ))
'#s(Prim1 add #s(Prim1 mult #s(Int 1) #s(Int 2)) #s(Print #s(Int 3)))

(parse-expr (list "Print" "(" "1" "*" "2" ")" "+" "3" ))
'#s(Print #s(Prim1 add #s(Prim1 mult #s(Int 1) #s(Int 2)) #s(Int 3)))

(parse-expr (list "Print" "1" "*" "(" "2" "+" "3" ")" ))
'#s(Print #s(Prim1 mult #s(Int 1) #s(Prim1 add #s(Int 2) #s(Int 3))))

(parse-expr (list  "1" "*" "(" "Print" "2" "+" "3" ")" ))
'#s(Prim1 mult #s(Int 1) #s(Print #s(Prim1 add #s(Int 2) #s(Int 3))))

(parse-expr (list "1" "*" "(" "2" "*" "(" "3" "+" "4" ")" ")" ))
'#s(Prim1 mult #s(Int 1) (Prim1 mult #s(Int 2) (Prim1 add #s(Int 3) #s(Int 4))))

(parse-expr (list "1" "*" "(" "Print" "2" "*" "(" "3" "+" "4" ")" ")" ))
'#s(Prim1 mult #s(Int 1) #s(Print #s(Prim1 mult #s(Int 2) (Prim1 add #s(Int 3) #s(Int 4)))))

(parse-expr (list "1" "*" "2" "/" "3"))
'#s(Prim div #s(Prim1 mult #s(Int 1) #s(Int 2)) #s(Int 3))

(parse-expr (list "1" "*" "2" "+" "3" "/" "4"))
'#s(Prim1 add #s(Prim1 mult #s(Int 1) #s(Int 2)) #s(Prim1 div #s(Int 3) #s(Int 4)))

(parse-expr (list "1" "*" "2" "+" "(" "3" "/" "4" ")"))
'#s(Prim1 add #s(Prim1 mult #s(Int 1) #s(Int 2)) #s(Prim1 div #s(Int 3) #s(Int 4)))

(parse-expr (list "(" "1" "*" "2" ")" "+" "3" "/" "4"))
'#s(Prim1 add #s(Prim1 mult #s(Int 1) #s(Int 2)) #s(Prim1 div #s(Int 3) #s(Int 4)))

(parse-expr (list "(" "1" "*" "2" ")" "+" "(" "3" "/" "4" ")"))
'#s(Prim1 add #s(Prim1 mult #s(Int 1) #s(Int 2)) #s(Prim1 div #s(Int 3) #s(Int 4)))

(parse-expr (list "1" "*" "(" "2"  "+" "3" ")" "/" "4" ")"))
'#s(Prim1 div #s(Prim1 mult #s(Int 1) #s(Prim add #s(Int 3) #s(Int 4))) #s(Int 4))
