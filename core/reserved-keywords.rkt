#lang racket
(provide reserved-keywords is-keyword?)

;; words that cannot be used in variable names
(define reserved-keywords 
 (set "(" ")" "[" "]" "{" "}" "#" "#:" "--" "null" "head" "`"
      "tail" "~" "cons" "," ";" "=" "equal" "equals" "!=" "not_equal" ">" "gt" "<"
      "lt" "<=" "le" ">=" "ge" "+" "plus" "-" "minus" "*" "star"
      "/" "slash" "^" "caret" "%" "mod" "&" "and" "||" "or" "xor" "not" "!" "true" "false"
      "@" "print" ":=" "assign" "let" "values" "in" "def" "set" "update" 
      "defined?" "defun" "final" "lam" "lambda" ":"
      "apply" ":>" "appnull" "comma"
      "|" "case" "->" "yields" "end"
      "if" "then" "else" "cond" "eval" "evalxp" "$" "cat" "type" "types" "error" "try" "catch" "dynamic"
      "int" "int?" "float" "float?" "string" "string?" "bool" "bool?" "list?"
      "pair?" "null?" "fun" "fun?" "match" "when" "?" 
      "index" "::" "appindex" "length"
      "typedef" "struct" "struct?" "schemaof" "var" 
      "appl" "while" "foreach" "do" "rec" "this" "to" "of" "void" "void?"
      "load" 
      "ls" "cd" "rm" "cp" "mv" "mkdir" 
      "getlinesf" "writestrf" "appendstrf" "system" "sysres" "system_type"
      "file_exists?" "dir_exists?"
      "eof" "eof?" "read_string" "peek_string" "write_string" "write_string_raw" "read_line"
      "make_hash" "hash_ref" "hash_ref_check" "hash_set" "hash?" "mutable_hash?"
      "hash_has_key?" "hash_keys" "hash_values" "hash_to_list" "gensym" "regex"
      "random"))


(define (is-keyword? v)
  (set-member? reserved-keywords v))
