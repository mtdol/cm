#lang racket
(require racket/lazy-require racket/runtime-path)
(require cm/core/error cm/core/ast cm/core/context)
(lazy-require [cm/core/main (run-file silent set-current-module-id!)])
(provide process-import process-lazy-import file-name->module-id module-string->filename
         show-modules write-modules add-to-modules remove-from-modules
         string->module-path-string)

;; Matthew Dolinka
;;

(define modules-file-already-imported? #f)
(define module-regex #rx"^\"(.+)\"\\:\"(.+)\"$")

;; the number of times an import has been run
(define num-imports 0)
(define max-imports 1000)

;; turns a file name into a module id
;;
;; string -> string
(define (file-name->module-id name)
        (path->string (simplify-path (path->complete-path name))))

;; takes in a path to a directory and turns it into a module path
;;
;; string -> string
(define (string->module-path-string path)
  (path->string
  (path->directory-path
    (simplify-path
      (path->complete-path path)))))

;; start from path of this source file and arive at module file
(define-runtime-path modules-file-path "../config/modules.txt")


(define modules (make-hash))

;; imports the file into the namespace
(define (do-import! file)
    (silent (run-file file)) (void))

(define (load-module-hash!)
           (for ([line (file->lines modules-file-path)])
            (cond 
                ;; ignore comments and empty lines
              [(not (regexp-match? #rx"^[\\t ]*$|^[\\t ]*\\#" line))
               (match (regexp-match module-regex line)
                      [(list _ b c) (hash-set! modules b c)]
                      [_ (cm-error "GENERIC" "Modules file is improperly formated.")])] 
              [else (void)])))

(unless modules-file-already-imported? 
  (set! modules-file-already-imported? #t)
  (load-module-hash!))


(define (show-modules)
  (let ([elems 
          (sort (hash->list modules) 
                #:key car string<?)])
    (display "\"Module_name\":\"Module_path\"\n\n")
    (map 
      (lambda (elem) 
        (display (format "\"~a\":\"~a\"\n" (car elem) (cdr elem))))
      elems)
    (void)
    ))

(define (add-to-modules abbrev path)
  (hash-set! modules abbrev path))

(define (remove-from-modules abbrev)
  (hash-remove! modules abbrev))

;; writes the modules hash to the file
(define (write-modules)
  (let ([elems 
          (sort (hash->list modules) 
                #:key car string<?)])
    (let ([str
     (foldl 
      (lambda (elem acc) 
        (string-append (format "\"~a\":\"~a\"\n" (car elem) (cdr elem)) acc))
      "" elems)])
    (display-to-file str modules-file-path #:exists 'truncate)
  )))

;; module string -> filename string
(define (module-string->filename str)
  ;; check if explicit file path
    (match (regexp-match #rx"^f\\:(.+)$" str)
        [(list _ file) file]
        ;; else is a module or unlabeled file path
        [_ (match (regexp-match #rx"^(?:m\\:)?(.+)\\:\\:(.+)$" str)
                  ;; reference module and append onto provided sub-path
                  [(list _ module file-path)
                   (string-append
                      (path->string (path->directory-path 
                        (hash-ref modules module (lambda () 
                          (cm-error "GENERIC" 
                              (format "Could not find module ~a." module))))))
                      file-path)]
                  ;; else assume file
                  [_ str]
             )]))

;; processes import command
(define (process-import str prefix current-module-id)
  (if (> num-imports max-imports) 
    (cm-error "IMPORT" "Too many imports detected. Possible import cycle.") 
    (set! num-imports (add1 num-imports)))
  ;; remember the current module id
  (let ([id (file-name->module-id (module-string->filename str))])
    (unless (not (string=? current-module-id id)) 
      (cm-error "IMPORT" "A module cannot import itself."))
    (do-import! id)
    ;; move references
    (set-refs-from-module-space! id current-module-id prefix #t)
    ;; restore the old module id
    (set-current-module-id! current-module-id)))

(define (process-lazy-import file-str type item prefix current-module-id)
  (let ([module-id (file-name->module-id (module-string->filename file-str))])
    (when (not (map-reference type item module-id current-module-id prefix #t))
      (cm-error "IMPORT" 
                (format "Could not lazy require ~a ~a from module ~a"
                        type item module-id)))
        
    (void)))
