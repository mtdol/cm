#lang racket
(provide Fun
         set-global-var! get-global-var-data
         set-local-var get-local-var-data
         set-type! get-type-data type-exists?)

(struct ContextEntry (value))
(struct TypeEntry (value))
;; what a lambda expr yields
(struct Fun (var type context expr))

(define global-context (make-hash))
(define global-types (make-hash))

(define (set-global-var! var value) 
  (hash-set! global-context var (ContextEntry value)))

(define (get-global-var-data var)
        (match (hash-ref global-context var (lambda () #f))
               [#f #f]
               [res (match res [(ContextEntry data) data])]))

(define (set-type! type schema)
  (hash-set! global-types type (TypeEntry schema)))

(define (get-type-data type)
        (match (hash-ref global-types type (lambda () #f))
               [#f #f]
               [res (match res [(TypeEntry data) data])]))

(define (type-exists? type)
  (hash-has-key? global-types type))

;; local getters, setters

;; returns a hash
(define (set-local-var var value context) 
  (hash-set context var (ContextEntry value)))

(define (get-local-var-data var context)
        (match (hash-ref context var (lambda () #f))
               [#f #f]
               [res (match res [(ContextEntry data) data])]))
