module Sem2.Sem2Spec (main, spec) where

import Test.Hspec

import Sem2.Types
import Sem2.Solution (typeOf)
-- import Sem2.Main

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sem2" $ do

    it "tests from the repo" $ do
        (typeOf $ Lam "x" Nat $ Add (Sym "x") (Natural 5))
          `shouldBe` Right (Fun Nat Nat)

        (typeOf $ Lam "x" Bool $ Sym "x")
            `shouldBe` Right (Fun Bool Bool)

        (typeOf $ Add (Natural 5) (Boolean False))
            `shouldBe` Left "Right arg of an App must be a Nat"

        (typeOf $ App (Lam "x" Nat $ Sym "x") (Natural 5))
            `shouldBe` Right Nat

        (typeOf $ App (Lam "x" Nat (Boolean False)) (Natural 5))
            `shouldBe` Right Bool

        (typeOf $ App (Lam "x" Bool $ Boolean False) (Natural 5))
            `shouldBe` Left "Type mismatch on Function Application"

        (typeOf $ Nil Nat)
            `shouldBe` Right (List Nat)

        (typeOf $ Cons (Natural 5) $ Cons (Boolean False) $ Nil Nat)
            `shouldBe` Left "First arg of Cons must have the same type as the List elements"

    it "tests my solution: Sym, Lam, App" $ do
        (typeOf $ Sym "z")
            `shouldBe` Left "Unknown variable \"z\""

        (typeOf $ Lam "x" Nat $ Add (Sym "y") (Natural 5))
            `shouldBe` Left "Unknown variable \"y\""

        (typeOf $ App (App (Lam "x" Nat $ Lam "x" Bool $ Sym "x") (Natural 5)) (Boolean False))
            `shouldBe` Right Bool

        (typeOf $ App (Boolean False) (Boolean True))
            `shouldBe` Left "Left arg of App must be a Fun"

    it "tests my solution: Natural, Add, Mult" $ do
        (typeOf $ (Natural $ -1))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ (Natural 0))
            `shouldBe` Right Nat

        -- Mult is very similar
        (typeOf $ Add (Natural 1) (Natural 2))
            `shouldBe` Right Nat

        (typeOf $ Add (Natural 1) (Natural $ -1))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ Add (Boolean False) (Natural 2))
            `shouldBe` Left "Left arg of an App must be a Nat"

        (typeOf $ Add (Natural 1) (Nil Nat))
            `shouldBe` Left "Right arg of an App must be a Nat"

        (typeOf $ Mult (Natural 1) (Natural 2))
            `shouldBe` Right Nat

    it "tests my solution: Boolean, Not, And, Or, Iff" $ do
        (typeOf $ (Boolean True))
            `shouldBe` Right Bool

        (typeOf $ (Not (Boolean True)))
            `shouldBe` Right Bool

        (typeOf $ (Not (Natural $ -1)))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ (Not (Natural 1)))
            `shouldBe` Left "Arg of Not is not a Bool"

        (typeOf $ And (Natural $ -1) (Boolean False))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ And (Natural 1) (Boolean False))
            `shouldBe` Left "Left arg of an And must be a Bool"

        (typeOf $ And (Boolean True) (IsNil (Nil Nat)))
            `shouldBe` Right Bool

        (typeOf $ Or (Natural $ -1) (Boolean False))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ Or (Natural 1) (Boolean False))
            `shouldBe` Left "Left arg of an Or must be a Bool"

        (typeOf $ Or (Boolean True) (IsNil (Nil Nat)))
            `shouldBe` Right Bool

        (typeOf $ Iff (Natural $ -1) (Boolean False) (Boolean False))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ Iff (Natural 1) (Boolean False) (Boolean False))
            `shouldBe` Left "Condition of an Iff must be a Bool"

        (typeOf $ Iff (IsNil (Nil Nat)) (Natural 1) (Natural 2))
            `shouldBe` Right Nat

        (typeOf $ Iff (IsNil (Nil Nat)) (Natural 1) (Boolean False))
            `shouldBe` Left "Then and Else args of an Iff must not have different types"

    it "tests my solution: Pair, Fst, Snd" $ do
        (typeOf $ Pair (Natural 1) (Boolean True))
            `shouldBe` (Right $ PairT Nat Bool)

        (typeOf $ Pair (Natural $ -1) (Boolean True))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ Fst (Natural $ -1))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ Fst (Natural 1))
            `shouldBe` Left "Arg of Fst must be of PairT type"

        (typeOf $ Fst (Pair (Natural 1) (Boolean False)))
            `shouldBe` Right Nat

        (typeOf $ Snd (Natural $ -1))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ Snd (Natural 1))
            `shouldBe` Left "Arg of Snd must be of PairT type"

        (typeOf $ Snd (Pair (Natural 1) (Boolean False)))
            `shouldBe` Right Bool

    it "tests my solution: Cons, Nil, IsNil, Head, Tail" $ do

        (typeOf $ Cons (Natural $ -1) (Nil Nat))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ Cons (Natural 1) (Nil Nat))
            `shouldBe` (Right $ List Nat)

        (typeOf $ Cons (Natural 1) (Nil Bool))
            `shouldBe` Left "First arg of Cons must have the same type as the List elements"

        (typeOf $ Cons (Natural 1) (Boolean False))
            `shouldBe` Left "Second arg of Cons must be of List type"

        (typeOf $ IsNil (Nil Nat))
            `shouldBe` Right Bool

        (typeOf $ IsNil (Natural 1))
            `shouldBe` Left "Arg of IsNil must be of List type"

        (typeOf $ IsNil (Natural $ -1))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ Head (Cons (Natural 1) (Nil Nat)))
            `shouldBe` Right Nat

        (typeOf $ Head (Cons (Natural $ -1) (Nil Nat)))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ Head (Boolean True))
            `shouldBe` Left "Arg of Head must be of List type"

        (typeOf $ Tail (Cons (Natural 1) (Nil Nat)))
            `shouldBe` (Right $ List Nat)

        (typeOf $ Tail (Cons (Natural $ -1) (Nil Nat)))
            `shouldBe` Left "Natural must be >= 0"

        (typeOf $ Tail (Boolean True))
            `shouldBe` Left "Arg of Tail must be of List type"

    it "other tests (by teacher)" $ do
        (typeOf $ Cons (Lam "x" Nat $ Sym "x") $ Nil (Fun Nat Nat))
            `shouldBe` Right (List $ Fun Nat Nat)

        (typeOf $ Lam "x" Bool $ Lam "x" Nat $ Sym "x")
            `shouldBe` Right (Fun Bool (Fun Nat Nat))

