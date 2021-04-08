#! /usr/bin/env racket
#lang racket
(require racket/enter racket/runtime-path)
(require (prefix-in main: cm/core/main) (prefix-in modules: cm/core/modules))

(define args (vector->list (current-command-line-arguments)))

;; location of this scripts directory
(define-runtime-path cm-dir ".")
(define-runtime-path std_lib-path "std_lib")
(define-runtime-path modules-file-path "./config/modules.txt")

(define help-text
  (string-append
      "No args: run the repl\n"
      "file: run a file\n"
      "-f file: run a file\n"
      "-e: run an expression\n"
      "-E: run a statement\n"
      "--install: install cm; reset the module system\n"
      "--pkg: package manager\n"
      "--help, -h: show this help text\n"
  ))

(define pkg-help-text
  (string-append
    "No args: show this help text\n"
    "\n"
    "add <module-abbrev> <module-path>:\n"
    "   Add the given module abbrev to the module system.\n"
    "\n"
    "remove <module-abbrev>:\n"
    "   Removes the given module-abbrev from the module system.\n"
    "\n"
    "show:\n"
    "   Show the current state of the module system.\n"
    "\n"
    "help, -h: show this help text\n"
  ))

(define (get-basic-modules-file-text)
  (format 
    "# this is a comment\n\"std_lib\":\"~a\"" 
    (path->complete-path std_lib-path)))
(define (reset-modules-file)
  (display-to-file 
    (get-basic-modules-file-text)
    modules-file-path
    #:exists 'truncate))

(define (add-to-modules-file label path)
  (display-to-file 
    (format "\n\"~a\":\"~a\"" label path)
    modules-file-path
    #:exists 'append))

(match args
  ['() (enter! cm/core/repl)]
  [(list v) #:when (or (string=? v "-h") (string=? v "--help"))
            (displayln help-text)]
  [(list "-f" f) (main:silent (main:run-file f))]
  [(list "-e" v) (main:display-expr-output (main:run-expr v))]
  [(list "-E" v) (main:display-output (main:run v))]
  [(list "--install") (reset-modules-file)]
  [(list "--pkg") (displayln pkg-help-text)]
  [(list "--pkg" v) 
   #:when (or (string=? v "-h") (string=? v "help"))
   (displayln pkg-help-text)]
  [(list "--pkg" "show") (modules:show-modules)]
  [(list "--pkg" "add" a1 a2) (add-to-modules-file a1 a2)]
  [(list f) (main:silent (main:run-file f))]
  [_ (displayln "Invalid args.")])
