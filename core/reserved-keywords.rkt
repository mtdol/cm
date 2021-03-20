#lang racket
(provide reserved-keywords is-keyword?)

;; words that cannot be used in variable names
(define reserved-keywords 
 (set "(" ")" "[" "]" "{" "}" "#" "null" "head" "`"
      "tail" "~" "cons" "," ";" "=" "equal" "equals" "!=" "not_equal" ">" "gt" "<"
      "lt" "<=" "le" ">=" "ge" "+" "plus" "-" "minus" "*" "star"
      "/" "slash" "^" "caret" "%" "mod" "&" "and" "||" "or" "xor" "not" "!" "true" "false"
      "@" "print" ":=" "assign" "let" "values" "in" "def" "defun" "final" "lam" "lambda" ":"
      "apply" ":>" "apply1" "comma"
      "|" "case" "->" "yields" "end"
      "if" "then" "else" "cond" "eval" "evalxp" "$" "cat" "type" "types" "error" "try" "catch" "dynamic"
      "int" "int?" "float" "float?" "string" "string?" "bool" "bool?" "list?"
      "pair?" "null?" "fun" "fun?" "format" "match" "index" "length"
      "typedef" "struct" "struct?" "appl" "while" "do" "rec" "this" "to" "of" "void" "void?"
      "load" 
      "ls" "cd" "rm" "cp" "mv" "mkdir" 
      "getlines" "writestr" "appendstr" "system" "sysres" "system_type"
      "file_exists?" "dir_exists?"
      "eof" "eof?"))


(define (is-keyword? v)
  (set-member? reserved-keywords v))
