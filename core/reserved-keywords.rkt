#lang racket
(provide reserved-keywords is-reserved-keyword?)

;; Matthew Dolinka
;;

;; words that cannot be used as variable names
(define reserved-keywords 
 (set "rec" "this" "for" "by" "let_values" "values" "unless" "where"))


(define (is-reserved-keyword? v)
  (set-member? reserved-keywords v))
