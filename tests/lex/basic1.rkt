#lang racket
(require cm/core/lex rackunit)

(check-equal? (tokenize-string "3")
'("3"))

(check-equal? (tokenize-string "3.")
'("3" "dot"))

(check-equal? (tokenize-string ".3")
'("dot" "3"))

(check-equal? (tokenize-string ".3.")
'("dot" "3" "dot"))

(check-equal? (tokenize-string "3.4")
'("3.4"))

(check-equal? (tokenize-string ".3.4")
'("dot" "3.4"))

(check-equal? (tokenize-string ".3.4.")
'("dot" "3.4" "dot"))

(check-equal? (tokenize-string ".3.4.")
'("dot" "3.4" "dot"))

(check-equal? (tokenize-string "+3.4")
'("plus" "3.4"))

(check-equal? (tokenize-string "5+3.4")
'("5" "plus" "3.4"))

(check-equal? (tokenize-string "5+.3.4")
'("5" "plus" "dot" "3.4"))

(check-equal? (tokenize-string "5+*3.4")
'("5" "plus" "mult" "3.4"))

(check-equal? (tokenize-string "/5+*3.4.")
'("div" "5" "plus" "mult" "3.4" "dot"))

(check-equal? (tokenize-string "/%5+*3.4.")
'("div" "mod" "5" "plus" "mult" "3.4" "dot"))

(check-equal? (tokenize-string "cond | true -> 2 else 3")
'("cond" "case" "true" "yields" "2" "else" "3"))

(check-equal? (tokenize-string "cond | true -> 2 else 3.")
'("cond" "case" "true" "yields" "2" "else" "3" "dot"))

(check-equal? (tokenize-string "cond|true->2 else 3.")
'("cond" "case" "true" "yields" "2" "else" "3" "dot"))

(check-equal? (tokenize-string "cond|true->2else3.")
'("cond" "case" "true" "yields" "2else3" "dot"))

(check-equal? (tokenize-string "cond|true->2.else3.")
'("cond" "case" "true" "yields" "2" "dot" "else3" "dot"))

(check-equal? (tokenize-string "cond|tru#e->2.else3.")
'("cond" "case" "tru"))

(check-equal? (tokenize-string "cond|\ntrue->2.else3.")
'("cond" "case" ":newline" "true" "yields" "2" "dot" "else3" "dot"))

(check-equal? (tokenize-string "cond|\ntrue->\n2.else3.")
'("cond" "case" ":newline" "true" "yields" ":newline" "2" "dot" "else3" "dot"))

(check-equal? (tokenize-string "cond|\ntrue#->\n2.else3.")
'("cond" "case" ":newline" "true" ":newline" "2" "dot" "else3" "dot"))

(check-equal? (tokenize-string "cond|\nt#rue->\n2.else3.")
'("cond" "case" ":newline" "t" ":newline" "2" "dot" "else3" "dot"))

(check-equal? (tokenize-string "cond|\nt#rue->\n2.el#se3.")
'("cond" "case" ":newline" "t" ":newline" "2" "dot" "el"))
