#lang racket
(require racket/lazy-require racket/runtime-path)
(require cm/core/error cm/core/ast cm/core/context)
(lazy-require [cm/core/main (run-file silent)])
(provide process-import)

(define modules-already-imported? #f)
(define module-regex #rx"^\"(.+)\"\\:\"(.+)\"$")

;; the number of times an import has been run
(define num-imports 0)
(define max-imports 500)

;; start from path of this source file and arive at module file
(define-runtime-path module-file-path "../config/modules.txt")


(define modules (make-hash))

;; imports the file into the namespace
(define (do-import! file)
    (silent (run-file file)) (void))

(define (load-module-hash!)
           (for ([line (file->lines module-file-path)])
            (cond 
                ;; ignore comments and empty lines
              [(not (regexp-match? #rx"^[\\t ]*$|^[\\t ]*\\#" line))
               (match (regexp-match module-regex line)
                      [(list _ b c) (hash-set! modules b c)]
                      [_ (cm-error "GENERIC" "Modules file is improperly formated.")])] 
              [else (void)])))

(unless modules-already-imported? 
  (set! modules-already-imported? #t)
  (load-module-hash!))

;; module string -> filename string
(define (get-filename str)
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
                                            (cm-error "GENERIC" (format "Could not find module ~a." module))))))
                        file-path)]
                  ;; else assume file
                  [_ str]
             )]
       )

  )

;; processes import command
(define (process-import str prefix)
  (if (> num-imports max-imports) 
    (cm-error "IMPORT" "Too many imports detected. Possible import cycle.") 
    (set! num-imports (add1 num-imports)))
  ;; remember the current module id
  (let ([module-id-backup current-module-id])
      (do-import! (get-filename str))
      ;; get the module id of the other file
      (let ([module-id current-module-id])
        ;; restore the old module id
        (set-current-module-id! module-id-backup)
        (set-refs-from-module-space! module-id '() prefix #t)
        )
  ))
