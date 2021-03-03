#lang racket
(provide reserved-keywords is-keyword?)

;; words that cannot be used in variable names
(define reserved-keywords 
 (set "(" ")" "[" "]" "{" "}" "#" "\\#" "noop" "null" "head" "`"
      "tail" "~" "cons" "," ";" "=" "equal" "equals" "!=" "not_equal" ">" "gt" "<"
      "lt" "<=" "le" ">=" "ge" "+" "plus" "-" "minus" "*" "mult"
      "/" "div" "^" "exp" "%" "mod" "&" "and" "||" "or" "xor" "not" "!" "true" "false"
      "@" "print" "let" "values" "in" "def" "final" "lam" "lambda" ":" "apply" "comma"
      "|" "case" "->" "yields" "end"
      "if" "then" "else" "cond" "eval" "$" "cat" "type" "error" "catch" "dynamic"
      "int" "int?" "float" "float?" "string" "string?" "bool" "bool?" "list?"
      "pair?" "null?" "fun" "fun?" "format" "match" "slice" "length" "with"
      "while" "rec" "this" "to" "of" "_"))


(define (is-keyword? v)
  (set-member? reserved-keywords v))
