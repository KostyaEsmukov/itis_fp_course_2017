module Sem1.ToTermI (toTermI) where

import Sem1.Types

-- (2)
-- перевод выражения в TermI
toTermI :: TermS -> TermI
toTermI term = replaceTerm term []

replaceTerm :: TermS -> [Symbol] -> TermI
replaceTerm (SymS sym) args = SymI (symToI sym args)
replaceTerm (LamS sym term) args = LamI (replaceTerm term ([sym] ++ args))
replaceTerm (AppS term1 term2) args =
    (AppI (replaceTerm term1 args) (replaceTerm term2 args))

symToI :: Symbol -> [Symbol] -> Int
symToI sym args = posInList sym args 0

posInList :: Symbol -> [Symbol] -> Int -> Int
posInList sym (cursym:rest) pos =
    (if sym == cursym then pos else posInList sym rest (pos + 1))
posInList sym [] pos = pos

