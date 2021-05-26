#lang racket
(require cm/tests/test-utils rackunit)

;; check for things that should fail

(check-failure run "def x 4")

(check-failure run "def float x := 4")

(check-failure run "def int x := 4.0")

(check-failure run "def bool x := 4.0")

(check-failure run "def struct x := 4.0")

(check-failure run "def struct St x := 4.0")

(check-failure run "def lalala x := 4.0")

(check-failure run "def lalala x 4.0")

(check-failure run "def 2 x := 4.0")

(check-failure run "int x := 4.0")

(check-failure run "def list x := 4.0")

(check-failure run "def list x := 4.0,5")

(check-failure run "def pair x := 4.0")

(check-failure run "1 + def int x := 4.0")

(check-failure run "def int x ::= 4")

(check-failure run "def x := int")

(check-failure run "let x := 5 in def float x := x")

(check-failure run "let x := 5 in def float z := x")


(check-failure run "\\x 4")

(check-failure run "(\\float x -> x) : 3")

(check-failure run "(\\string x -> x) : 3")

(check-failure run "(\\bool x -> x) : 3")

(check-failure run "(\\int x -> x) : \"3\"")

(check-failure run "(\\struct x -> x) : \"3\"")

(check-failure run "(\\struct St x -> x) : \"3\"")

(check-failure run "(\\lala x -> x) : \"3\"")

(check-failure run "(\\int x, float y -> x) : \"3\"")

(check-failure run "(\\x, float y -> x) : \"3\" : 4")

(check-failure run " \\float y x")

(check-failure run "(\\int x ->  \\float y -> x) : \"3\"")

(check-failure run "(\\x -> \\float y -> x) : \"3\" : 4")

(check-failure run "let x := \\int y in 3.0 ->x")

(check-failure run "let x := \\int y -> y in x : 3.0")
