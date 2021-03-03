#lang racket
(provide set-global-var! get-global-var-type get-global-var-data
         set-local-var get-local-var-type get-local-var-data)

(struct ContextEntry (type value))

(define global-context (make-hash))

(define (set-global-var! var type value) 
  (hash-set! global-context var (ContextEntry type value)))

;; returns type if var exists in global context, else false
(define (get-global-var-type var)
        (match (hash-ref global-context var (lambda () #f))
               [#f #f]
               [res (match res [(ContextEntry type _) type])]))

(define (get-global-var-data var)
        (match (hash-ref global-context var (lambda () #f))
               [#f #f]
               [res (match res [(ContextEntry _ data) data])]))

;; local getters, setters

;; returns a hash
(define (set-local-var var type value context) 
  (hash-set context var (ContextEntry type value)))

;; returns type if var exists in global context, else false
(define (get-local-var-type var context)
        (match (hash-ref context var (lambda () #f))
               [#f #f]
               [res (match res [(ContextEntry type _) type])]))

(define (get-local-var-data var context)
        (match (hash-ref context var (lambda () #f))
               [#f #f]
               [res (match res [(ContextEntry _ data) data])]))
