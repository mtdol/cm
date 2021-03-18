#lang racket
(provide reserved-keywords is-keyword?)

;; words that cannot be used in variable names
(define reserved-keywords 
 (set "(" ")" "[" "]" "{" "}" "#" "\\#" "noop" "null" "head" "`"
      "tail" "~" "cons" "," ";" "=" "equal" "equals" "!=" "not_equal" ">" "gt" "<"
      "lt" "<=" "le" ">=" "ge" "+" "plus" "-" "minus" "*" "mult"
      "/" "div" "^" "exp" "%" "mod" "&" "and" "||" "or" "xor" "not" "!" "true" "false"
      "@" "print" ":=" "assign" "let" "values" "in" "def" "defun" "final" "lam" "lambda" ":"
      "apply" ":>" "apply1" "comma"
      "|" "case" "->" "yields" "end"
      "if" "then" "else" "cond" "eval" "evalxp" "$" "cat" "type" "types" "error" "try" "catch" "dynamic"
      "int" "int?" "float" "float?" "string" "string?" "bool" "bool?" "list?"
      "pair?" "null?" "fun" "fun?" "format" "match" "index" "length"
      "typedef" "struct" "struct?" "appl" "while" "do" "rec" "this" "to" "of" "void" "void?"
      "load" "eof" "read_byte" "write_byte" "peek_bytes" "ls" "cd" "getlines"
      "writestr" "appendstr" "system" "sysres" "system_type"))


(define (is-keyword? v)
  (set-member? reserved-keywords v))
