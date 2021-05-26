#lang racket
(require cm/tests/test-utils rackunit)

;; check for things that should fail

(check-failure run "cond")

(check-failure run "cond | ")

(check-failure run "cond | 1")

(check-failure run "cond | 1 -> 2")

(check-failure run "cond | 1 -> 2 else")

(check-failure run "cond | 1 -> 2 else else")

(check-failure run "cond | 1 2 else 5")

(check-failure run "cond | -> 2 else 5")

(check-failure run "cond | bool 0 -> 2 end")

(check-failure run "cond | 1 else 2 else 5")

(check-failure run "else 2")

(check-failure run "-> 2")

(check-failure run "| -> 2")

(check-failure run "cond | -> 2")

(check-failure run "|")

(check-failure run "| 1")

(check-failure run "| 1 -> 2")

(check-failure run "| 1 -> 2 else")

(check-failure run "| 1 -> 2 else else")

(check-failure run "| 1 2 else 5")

(check-failure run "| -> 2 else 5")

(check-failure run "| bool 0 -> 2 end")

(check-failure run "| 1 else 2 else 5")
