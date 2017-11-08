module Sem1.Types where

newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Show,Read)

-- (1)
data TermS = SymS Symbol        -- x
           | LamS Symbol TermS  -- \x -> t
           | AppS TermS TermS   -- t1 t2
           deriving (Eq,Show,Read)

-- (2)
data TermI = SymI Int
           | LamI TermI
           | AppI TermI TermI
           deriving (Eq,Show,Read)

data TermP = TermP TermS
           -- (4)
           | Natural Int
           | Plus TermP TermP
           | Mult TermP TermP
           -- (4*) +10%
           | Minus TermP TermP
           | Divide TermP TermP
           -- (5*) +50%
           | Y TermP
           -- (7)
           | Cons TermP TermP
           | Nil
           | IsNil TermP
           | Head TermP
           | Tail TermP
           deriving (Eq,Show,Read)

sym :: String -> TermS
sym x = SymS (Symbol x)

lam :: String -> TermS -> TermS
lam x t = LamS (Symbol x) t

app :: TermS -> TermS -> TermS
app t1 t2 = AppS t1 t2

