#lang racket
(require parser-tools/lex)
(provide tok line tok=? token->nl-token Token make-token)

;; Matthew Dolinka
;;

(struct Token (tok) #:transparent)
(struct LocToken (tok line) #:transparent)

(define (tok token) 
  (match token
         [(position-token tok _ _) tok]
         [(Token tok) tok]
         [(LocToken tok _) tok]))

;; returns the token line-number else -1 if the token does not have this info
(define (line token) 
  (match token
         [(position-token _ (position _ line _) _) line]
         [(Token _) -1]
         [(LocToken _ line) line]))

(define (tok=? elem) 
  (lambda (token) 
    (equal? (tok token) elem)))

;; creates a token with no linenumber
(define (token->nl-token token)
  (match token
         [(position-token tok _ _) (Token tok)]))

;; if linenum is -1 then creates a bare token
(define (make-token tok linenum)
  (if (= -1 linenum) (Token tok) (LocToken tok linenum)))
