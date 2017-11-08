module Sem1.ToTermS (toTermS) where

import Sem1.Types

-- # shorthands:
-- 1 and 0
s = "s" -- 1
z = "z" -- 0
-- plus, mult, ...
m = "m"
n = "n"
-- pred
g = "g"
h = "h"
u = "u"
-- div
c = "c"
d = "d"
-- lists
x = "x"
f = "f"
t = "t"
-- b = "b"
-- a = "a"
-- p = "p"
l = "l"


toTermS :: TermP -> TermS
toTermS (TermP ts) = ts
toTermS (Natural i) = lam s $ lam z $ churchNumeral i
toTermS (Plus tp1 tp2) = app (app plus (toTermS tp1)) $ toTermS tp2
toTermS (Mult tp1 tp2) = app (app mult (toTermS tp1)) $ toTermS tp2
toTermS (Minus tp1 tp2) = app (app minus (toTermS tp1)) $ toTermS tp2
toTermS (Divide tp1 tp2) = app (app divide (toTermS tp1)) $ toTermS tp2
toTermS (Y tp) = app y_comb (toTermS tp)
toTermS (Cons tp1 tp2) = app (app cons (toTermS tp1)) $ toTermS tp2
toTermS Nil = nil
toTermS (IsNil tp) = app iszero (toTermS tp)
toTermS (Head tp) = app head_comb (toTermS tp)
toTermS (Tail tp) = app tail_comb (toTermS tp)

-- http://newstar.rinet.ru/~goga/tapl/tapl.pdf
-- https://neerc.ifmo.ru/wiki/index.php?title=Лямбда-исчисление
-- https://en.wikipedia.org/wiki/Church_encoding

-- P 60
churchNumeral :: Int -> TermS
churchNumeral 0 = sym z
churchNumeral i = app (sym s) $ churchNumeral (i-1)

plus :: TermS
plus = lam m $ lam n $ lam s $ lam z $ app (app (sym m) (sym s)) $ app (app (sym n) (sym s)) (sym z)

succ_comb :: TermS
succ_comb = lam n $ lam s $ lam z $ app (sym s) (app (app (sym n) (sym s)) (sym z))

mult :: TermS
mult = lam m $ lam n $ app (app (sym m) (app plus (sym n))) false

-- wiki
pred_comb :: TermS
pred_comb = lam n $ lam s $ lam z $ app (app (app (sym n) (lam g $ lam h $ app (sym h) (app (sym g) (sym s)))) (lam u $ sym z)) (lam u $ sym u)

minus :: TermS
minus = lam m $ lam n $ app (app (sym n) pred_comb) (sym m)

-- https://en.wikipedia.org/wiki/Church_encoding#Division
divide :: TermS
divide = lam n $ app _divide1 (app succ_comb (sym n))

_divide1 :: TermS
_divide1 = app _y_div _div

_y_div :: TermS
_y_div = lam s $ app (lam z $ app (sym z) (sym z)) (lam z $ app (sym s) (app (sym z) (sym z)))

_div1 :: TermS
_div1 = app (app iszero (sym d)) (app (app false (sym s)) (sym z))

_div2 :: TermS
_div2 = app (sym s) (app (app (app (app (sym c) (sym d)) (sym m)) (sym s)) (sym z))

_div :: TermS
_div = lam c $ lam n $ lam m $ lam s $ lam z $ app (lam d $ app _div1 _div2) (app (app minus (sym n)) (sym m))

iszero :: TermS
iszero = lam n $ app (app (sym n) (lam z false)) true

-- P 67
-- _omega_half :: TermS
-- _omega_half = lam x $ app (sym x) (sym x)
-- omega :: TermS
-- omega = app _omega_half _omega_half

-- _fix_half :: TermS
-- _fix_half = lam x $ app (sym f) (lam y $ app (app (sym x) (sym x)) (sym y))
-- fix :: TermS
-- fix = lam f $ app _fix_half _fix_half

_y_comb_half :: TermS
_y_comb_half = lam x $ app (sym f) (app (sym x) (sym x))
y_comb :: TermS
y_comb = lam f $ app _y_comb_half _y_comb_half

---- http://safalra.com/lambda-calculus/tuples-and-lists/
---- https://en.wikipedia.org/wiki/Church_encoding

false :: TermS
false = lam s $ lam z $ sym z

true :: TermS
true = lam s $ lam z $ sym s

-- P 528 5.2.8

nil :: TermS
nil = false

cons :: TermS
cons = lam h $ lam t $ lam s $ lam z $ app (app (sym s) (sym h)) $ app (app (sym t) (sym s)) (sym z)

head_comb :: TermS
head_comb = lam l $ app (app (sym l) (lam h (lam t (sym h)))) false

tail_comb :: TermS
tail_comb = lam l $ lam s $ lam z $ app (app (app (sym l)
  (lam h $ lam t $ lam g $ app (app (sym g) (sym h)) (app (sym t) (sym s))))
      (lam t $ sym z)) (lam h $ lam t $ sym t)

