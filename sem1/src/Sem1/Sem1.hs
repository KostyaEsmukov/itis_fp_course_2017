module Sem1.Sem1 where

import Sem1.Types
import Sem1.BetaI
import Sem1.ToTermI
import Sem1.ToTermS

-- выполнять редукцию до конца
-- (но не больше 10000 шагов из-за возможности зависания)
full :: (TermS -> a) -> (a -> Maybe a) -> TermS -> a
full a b term = lastUnf 10000 b (a term)
  where lastUnf :: Int -> (a -> Maybe a) -> a -> a
        lastUnf 0 _ x = x
        lastUnf n f x = case f x of
          Nothing -> x
          Just y -> lastUnf (n-1) f y

solve :: TermP -> Either TermI TermS
solve t = Left (full toTermI betaI (toTermS t))

main :: IO ()
main = do
  s <- read <$> getLine
  print $ solve s
