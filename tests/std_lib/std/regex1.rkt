#lang racket
(require cm/tests/test-utils rackunit)

(run-stat-silent "#:import \"std_lib::std.cm\"")

;; various sanity tests for the regex engine

;;
;; regex_match
;;

(check-equal? (run "{: regex_match | \"abc\" | \"ab\"}")
val-false)

(check-equal? (run "{: regex_match | \"abc\" | \"abc\"}")
'("abc"))

(check-equal? (run "{: regex_match | \"a(b)c\" | \"abc\"}")
'("abc" "b"))

(check-equal? (run "{: regex_match | \"a(b)c\" | \"zabc\"}")
'("abc" "b"))

(check-equal? (run "{: regex_match | \"^a(b)c\" | \"zabc\"}")
val-false)

(check-equal? (run "{: regex_match | \"a\" | \"\"}")
val-false)

(check-equal? (run "{: regex_match | \"\" | \"\"}")
'(""))

;; malformed
(check-exn exn:fail? (lambda ()
  (run "{: regex_match | \"a(?<f)\" | \"af\"}")))

;;
;; regex_match_all
;;

(check-equal? (run "{: regex_match_all | \"abc\" | \"ab\"}")
'())

(check-equal? (run "{: regex_match_all | \"abc\" | \"abc\"}")
'(("abc")))

(check-equal? (run "{: regex_match_all | \"a(b)c\" | \"abc\"}")
'(("abc" "b")))

(check-equal? (run "{: regex_match_all | \"a(.)c\" | \"zabcfajcd\"}")
'(("abc" "b") ("ajc" "j")))

(check-equal? (run "{: regex_match_all | \"a\" | \"\"}")
'())

(check-equal? (run "{: regex_match_all | \"\" | \"\"}")
'(("")))

;; malformed
(check-exn exn:fail? (lambda ()
  (run "{: regex_match_all | \"a(?<f)\" | \"af\"}")))


;;
;; regex_match?
;;

(check-equal? (run "{: regex_match? | \"abc\" | \"ab\"}")
val-false)

(check-equal? (run "{: regex_match? | \"abc\" | \"abc\"}")
val-true)

(check-equal? (run "{: regex_match? | \"a(b)c\" | \"abc\"}")
val-true)

(check-equal? (run "{: regex_match? | \"a(b)c\" | \"zabc\"}")
val-true)

(check-equal? (run "{: regex_match? | \"^a(b)c\" | \"zabc\"}")
val-false)

(check-equal? (run "{: regex_match? | \"a\" | \"\"}")
val-false)

(check-equal? (run "{: regex_match? | \"\" | \"\"}")
val-true)

;; malformed
(check-exn exn:fail? (lambda ()
  (run "{: regex_match? | \"a(?<f)\" | \"af\"}")))


;;
;; regex_split
;;

(check-equal? (run "{: regex_split | \"abc\" | \"ab\"}")
'("ab"))


(check-equal? (run "{: regex_split | \"a\" | \"\"}")
'(""))

(check-equal? (run "{: regex_split | \"\" | \"\"}")
'("" ""))

(check-equal? (run "{: regex_split | \"\" | \"ab\"}")
'("" "a" "b" ""))

(check-equal? (run "{: regex_split | \"\\n\" | \"ab\\ncd\\nef\"}")
'("ab" "cd" "ef"))

;; malformed
(check-exn exn:fail? (lambda ()
  (run "{: regex_split | \"a(?<f)\" | \"af\"}")))


;;
;; regex_replace
;;

(check-equal? (run "{: regex_replace | \".(.) \" | \"zab c\" | \"\\\\1_\"}")
"zb_c")

(check-equal? (run "{: regex_replace | \".(.) \" | \"zab cd fg\" | \"\\\\1_\"}")
"zb_cd fg")

(check-equal? (run "{: regex_replace | \".(.) \" | \"zab cd fg\" | \"\"}")
"zcd fg")

(check-equal? (run "{: regex_replace | \"\" | \"zab cd fg\" | \"5\"}")
"5zab cd fg")


;; malformed
(check-exn exn:fail? (lambda ()
  (run "{: regex_replace | \"a(?<f)\" | \"af\" | \"\\\\1\"}")))


;;
;; regex_replace_all
;;

(check-equal? (run "{: regex_replace_all | \".(.) \" | \"zab c\" | \"\\\\1_\"}")
"zb_c")

(check-equal? (run "{: regex_replace_all | \".(.) \" | \"zab cd fg\" | \"\\\\1_\"}")
"zb_d_fg")

(check-equal? (run "{: regex_replace_all | \".(.) \" | \"zab cd fg\" | \"\"}")
"zfg")

(check-equal? (run "{: regex_replace_all | \"\" | \"zab cd fg\" | \"5\"}")
"5z5a5b5 5c5d5 5f5g5")


;; malformed
(check-exn exn:fail? (lambda ()
  (run "{: regex_replace_all | \"a(?<f)\" | \"af\" | \"\\\\1\"}")))
