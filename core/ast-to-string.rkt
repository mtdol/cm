#lang racket
(require cm/core/ast)
(provide ast-to-string)

;; returns the ast back with proper parenthesis
(define (ast-to-string ast) (apply string-append (ast-to-string-aux ast)))
(define (ast-to-string-aux ast) 
    (match ast
           [(Int i) (list (number->string i))]
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
           [(Prim1 op e) (flatten
                               (list "(" (symbol->string (translate-op-name op))
                                     " (" (ast-to-string-aux e) "))"))]
           [(Def v e) (list "(" "def " (ast-to-string v) " "
                                (ast-to-string e) ")")]
           [(Let e1 e2) (list "(" "let " (ast-to-string e1) " "
                                (ast-to-string e2) ")")]
           [(Values e1 e2) (list "(" "values " (ast-to-string e1) " "
                                (ast-to-string e2) ")")]
           [(Lambda v e) (flatten (list "(" "lambda "
                        (ast-to-string-aux v) " " (ast-to-string-aux e) ")"))]
           [(Assign1 e) (flatten (list "= "
                        (ast-to-string-aux e)))]
           [(Assign2 e1 e2) (flatten (list "= "
                        (ast-to-string-aux e1) " " (ast-to-string e2)))]
           [(If e1 e2) (flatten
                               (list "(" "if " (ast-to-string-aux e1) " "
                                  (ast-to-string-aux e2) ")"))]
           [(Then e1 e2) (flatten
                               (list "then " (ast-to-string-aux e1) " "
                                  (ast-to-string-aux e2)))]
           [(Else e) (flatten
                               (list "else " (ast-to-string-aux e)))]
           [(With e) (flatten
                               (list "with " (ast-to-string-aux e)))]
           [(In e) (flatten
                               (list "in " (ast-to-string-aux e)))]
           [(Cond e1 e2) (flatten
                               (list "(" "cond " (ast-to-string-aux e1) " "
                                  (ast-to-string-aux e2) ")"))]
           [(Print e) (flatten (list "(" "print " (ast-to-string-aux e) ")"))]
           [(Format e1 e2) (flatten (list "(" "format " 
                            (ast-to-string-aux e1) " " (ast-to-string-aux e2) ")"))]
           [(Match e1 e2) (flatten (list "(" "match " 
                            (ast-to-string-aux e1) " " (ast-to-string-aux e2) ")"))]
           [(Slice e1 e2) (flatten (list "(" "slice " 
                            (ast-to-string-aux e1) " " (ast-to-string-aux e2) ")"))]
           [(Error s) (flatten (list "(" "error " (ast-to-string-aux s) ")"))]
           [(Stat e i st) (list (ast-to-string e) ". " (ast-to-string st))]
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
