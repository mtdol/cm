#lang racket
(require cm/tests/test-utils rackunit)

;; check for things that should fail

(check-failure run "let x := 3")

(check-failure run "let x := 3 5")

(check-failure run "let x := 3 in")

(check-failure run "let x := 3 in in")

(check-failure run "let x 3 in")

(check-failure run "let x 3 in 5")

(check-failure run "let int x 3 in 5")

(check-failure run "let int x := 3.0 in 5")

(check-failure run "let bool x := 3.0 in 5")

(check-failure run "let string x := 3.0 in 5")

(check-failure run "let struct x := 3.0 in 5")

(check-failure run "let struct St x := 3.0 in 5")

(check-failure run "let lala x := 3.0 in 5")

(check-failure run "x := 3.0 in 5")

(check-failure run "let x := 3.0 in let float z := 5 in x")
