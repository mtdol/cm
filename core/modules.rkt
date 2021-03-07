#lang racket
(require racket/lazy-require racket/runtime-path)
(require cm/core/error cm/core/ast)
(lazy-require [cm/core/cm (cm-run-file-silent-abs cm-run-file-silent)])
(provide process-import-string)
(define error-id 5)

(define modules-already-imported? #f)
(define module-regex #rx"^\"(.+)\"\\:\"(.+)\"$")

;; start from path of this source file and arive at module file
(define-runtime-path module-file-path "../config/modules.txt")


(define modules (make-hash))

;; imports the file into the namespace
(define (do-import! file)
    (cm-run-file-silent-abs file) (void))
(define (do-import-relative! file)
    (cm-run-file-silent file) (void))

(define (load-module-hash!)
           (for ([line (file->lines module-file-path)])
            (cond 
                ;; ignore comments and empty lines
              [(not (regexp-match? #rx"^[\\t ]*$|^[\\t ]*\\#" line))
               (match (regexp-match module-regex line)
                      [(list _ b c) (hash-set! modules b c)]
                      [_ (cm-error error-id "Modules file is improperly formated.")])] 
              [else (void)])))

;; processes string from load command
(define (process-import-string str) 
  (unless modules-already-imported? 
    (set! modules-already-imported? #t)
    (load-module-hash!))

  (match (regexp-match #rx"^f\\:(.+)$" str)
    ;[(list _ file) (do-import! file) (Void)]
    [(list _ file) (do-import! file) (Void)]
    ;; else is a module or relative path
    [_ (match (regexp-match #rx"^(.+)\\:\\:(.+)$" str)
              ;; reference module and append onto provided sub-path
              [(list _ module file-path)
               (do-import! (string-append
                    (path->string (path->directory-path 
                            (hash-ref modules module (lambda () 
                                        (cm-error error-id (format "Could not find module ~a." module))))))
                    file-path))]
              ;; else assume relative path
              [_ (do-import-relative! str) (Void)]
         )]
   )
  )
