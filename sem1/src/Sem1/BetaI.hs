module Sem1.BetaI (betaI) where

import Sem1.Types

-- (2)
-- шаг редукции
betaI :: TermI -> Maybe TermI
betaI (SymI _) = Nothing
betaI (LamI term) = case (betaI term) of
                      Just term2 -> Just (LamI term2)
                      Nothing -> Nothing
betaI (AppI (LamI term1) term2) = Just (reduct 0 term2 term1)
betaI (AppI (AppI term1 term2) term3) = case (betaI (AppI term1 term2)) of
                                          Just term -> Just (AppI term term3)
                                          Nothing -> Nothing
-- that is (AppI (SymI _) _)
betaI (AppI term1 term2) = case (betaI term2) of
                             Just term -> Just (AppI term1 term)
                             Nothing -> Nothing

reduct :: Int -> TermI -> TermI -> TermI
reduct depth term1 (LamI term2) = LamI (reduct (depth + 1) term1 term2)
reduct depth term1 (AppI term2 term3) =
    (AppI (reduct depth term1 term2) (reduct depth term1 term3))
reduct depth term1 (SymI s) | depth == s = (getSym term1 0 depth)
                            | s > depth = SymI (s - 1)
                            | otherwise = SymI s

getSym :: TermI -> Int -> Int -> TermI
getSym (SymI s) idepth depth =
    (if idepth <= s then (SymI (s + depth)) else (SymI s))
getSym (AppI term1 term2) idepth depth =
    (AppI (getSym term1 idepth depth) (getSym term2 idepth depth))
getSym (LamI term) idepth depth = LamI (getSym term (idepth + 1) depth)

