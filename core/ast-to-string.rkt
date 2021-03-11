#lang racket
(require cm/core/ast)
(provide ast-to-string)

;; returns the ast back with proper parenthesis
(define (ast-to-string ast) (apply string-append (ast-to-string-aux ast)))
(define (ast-to-string-aux ast) 
    (match ast
           [(Int i) (list (number->string i))]
           [(Bool i) (list (if (equal? i 1) "true" "false"))]
           [(Float f) (list (number->string f))]
           [(String s) (list "\"" s "\"")]
           [(Var v) (list v)]
           [(Null) "null"]
           [(Prim2 op e1 e2) #:when (symbol=? op 'cons)
                              (flatten (list "(" (ast-to-string-aux e1)
                                     ", " (ast-to-string-aux e2) ")"))]
           [(Prim2 op e1 e2) (flatten
                               (list "(" (ast-to-string-aux e1)
                                     " " (symbol->string (translate-op-name op))
                                     " " (ast-to-string-aux e2) ")"))]
           [(Prefix2 op e1 e2) (flatten
                               (list "(" (symbol->string (translate-op-name op))
                                     " " (ast-to-string-aux e1)
                                     " " (ast-to-string-aux e2) ")"))]
           [(Prim1 op e) (flatten
                               (list "(" (symbol->string (translate-op-name op))
                                     " (" (ast-to-string-aux e) "))"))]
           [(Prim0 op) (flatten
                               (list (symbol->string (translate-op-name op))))]
           [(Def v e) (list "(" "def " (ast-to-string v) " "
                                (ast-to-string e) ")")]
           [(Typedef v e) (list "(" "typedef " (ast-to-string v) " "
                                (ast-to-string e) ")")]
           [(Let e1 e2 e3) (list "(" "let " (ast-to-string e1) " := "
                                (ast-to-string e2) " in " (ast-to-string e3) ")")]
           [(Lambda v e) (flatten (list "(" "lambda "
                        (ast-to-string-aux v) " " (ast-to-string-aux e) ")"))]
           [(Assign e) (flatten (list "= "
                        (ast-to-string-aux e)))]
           [(If e1 e2) (flatten
                               (list "(" "if " (ast-to-string-aux e1) " "
                                  (ast-to-string-aux e2) ")"))]
           [(Then e1 e2) (flatten
                               (list "then " (ast-to-string-aux e1) " "
                                  (ast-to-string-aux e2)))]
           [(Else e) (flatten
                               (list "else " (ast-to-string-aux e)))]
           [(In e) (flatten
                               (list "in " (ast-to-string-aux e)))]
           [(Cond e) (flatten
                               (list "(" "cond " (ast-to-string-aux e) ")"))]
           [(Yields e) (flatten
                               (list "-> " (ast-to-string-aux e)))]
           [(Case e1 e2 e3) (flatten
                               (list "| " (ast-to-string-aux e1) " "
                                  (ast-to-string-aux e2) " " (ast-to-string-aux e3)))]
           [(Match e1 e2) (flatten (list "(" "match " 
                            (ast-to-string-aux e1) " " (ast-to-string-aux e2) ")"))]
           [(Stat i e st) (list (ast-to-string e) ". " (ast-to-string st))]
           [(EOP) (list)]))

;; translates op names if necessary to the form used in the language
(define (translate-op-name op) 
  (match op
    ['add 'plus]
    ['pos 'plus]
    ['sub 'minus]
    ['neg 'minus]
    ['apply ':]
    [op op]))
