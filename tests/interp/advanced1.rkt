;def find_a = lam a = let sub1 = lam x = x - 1 in a : sub1.

;def fact = lam n = | n < 2 -> 1 else n * (n - 1 : fact).

;def get_last = lam list lst = | null? lst -> null | null? ~lst -> `lst else ~lst : get_last.

;def add2 = let x = 2 in lam int y = x + y.

