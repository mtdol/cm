#lang racket
(require cm/tests/test-utils rackunit)

(check-equal? (tokenize "3")
'("3"))

(check-equal? (tokenize "3//")
'("3" "//"))

(check-equal? (tokenize "//3")
'("//" "3"))

(check-equal? (tokenize "//3//")
'("//" "3" "//"))

(check-equal? (tokenize "3.4")
'("3.4"))

(check-equal? (tokenize "//3.4")
'("//" "3.4"))

(check-equal? (tokenize "//3.4//")
'("//" "3.4" "//"))

(check-equal? (tokenize "//3.4//")
'("//" "3.4" "//"))

(check-equal? (tokenize "+3.4")
'("+" "3.4"))

(check-equal? (tokenize "5+3.4")
'("5" "+" "3.4"))

(check-equal? (tokenize "5+//3.4")
'("5" "+" "//" "3.4"))

(check-equal? (tokenize "5+*3.4")
'("5" "+" "*" "3.4"))

(check-equal? (tokenize "/5+*3.4//")
'("/" "5" "+" "*" "3.4" "//"))

(check-equal? (tokenize "/%5+*3.4//")
'("/" "%" "5" "+" "*" "3.4" "//"))

(check-equal? (tokenize "cond | true -> 2 else 3")
'("cond" "|" "true" "->" "2" "else" "3"))

(check-equal? (tokenize "cond | true -> 2 else 3//")
'("cond" "|" "true" "->" "2" "else" "3" "//"))

(check-equal? (tokenize "cond|true->2 else 3//")
'("cond" "|" "true" "->" "2" "else" "3" "//"))

(check-equal? (tokenize "cond|true->2else3//")
'("cond" "|" "true" "->" "2else3" "//"))

(check-equal? (tokenize "cond|true->2//else3//")
'("cond" "|" "true" "->" "2" "//" "else3" "//"))

(check-equal? (tokenize "cond|tru--e->2.else3//")
'("cond" "|" "tru"))

(check-equal? (tokenize "cond|\ntrue->2//else3//")
'("cond" "|" "true" "->" "2" "//" "else3" "//"))

(check-equal? (tokenize "cond|\ntrue->\n2//else3//")
'("cond" "|" "true" "->" "2" "//" "else3" "//"))

(check-equal? (tokenize "cond|\ntrue-->\n2//else3//")
'("cond" "|" "true" "2" "//" "else3" "//"))

(check-equal? (tokenize "cond|\nt--rue->\n2//else3//")
'("cond" "|" "t" "2" "//" "else3" "//"))

(check-equal? (tokenize "cond|\nt--rue->\n2//el--se3//")
'("cond" "|" "t" "2" "//" "el"))
