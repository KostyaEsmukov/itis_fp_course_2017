module Sem1.Sem1Spec (main, spec) where

import Test.Hspec

import Sem1.Types
import Sem1.BetaI
import Sem1.ToTermI
import Sem1.ToTermS
import Sem1.Sem1 (solve)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sem1" $ do

    it "tests toTermI" $ do
        -- example from sem1 details
        toTermI (lam "x" $ app (lam "x" $ sym "x") (lam "x" $ lam "y" $ app (sym "y") (lam "y" $ sym "x")))
          `shouldBe` LamI (AppI (LamI (SymI 0)) (LamI (LamI (AppI (SymI 0) (LamI (SymI 2))))))

        -- P 78
        toTermI (lam "x" (lam "y" (app (sym "x") (app (sym "y") (sym "x")))))
          `shouldBe` (LamI (LamI (AppI (SymI 1) (AppI (SymI 0) (SymI 1)))))

        toTermI (sym "x") `shouldBe` (SymI 0)
        toTermI (lam "x" (sym "x")) `shouldBe` (LamI (SymI 0))
        toTermI (lam "x" (sym "y")) `shouldBe` (LamI (SymI 1))
        toTermI (lam "x" (lam "y" (sym "y"))) `shouldBe` (LamI (LamI (SymI 0)))
        toTermI (lam "x" (lam "x" (sym "x"))) `shouldBe` (LamI (LamI (SymI 0)))
        toTermI (lam "x" (lam "y" (sym "x"))) `shouldBe` (LamI (LamI (SymI 1)))
        toTermI (lam "x" (lam "y" (sym "z"))) `shouldBe` (LamI (LamI (SymI 2)))
        toTermI (lam "x" (lam "y" (lam "x" (lam "q" (sym "z")))))
          `shouldBe` (LamI (LamI (LamI (LamI (SymI 4)))))
        toTermI (app (sym "x") (sym "y")) `shouldBe` (AppI (SymI 0) (SymI 0))

        -- telegram Kirill
        -- toTermI (app (lam "x" $ app (sym "x") (sym "x")) (lam "y" $ app (sym "z") (app (sym "y") (sym "z"))))
        --   `shouldBe` (AppI (LamI (AppI (SymI 0) (SymI 0))) (LamI (AppI (SymI 1) (AppI (SymI 0) (SymI 1)))))
        -- toTermI (lam "y" $ app (sym "z") (app (sym "y") (sym "z")))
        --   `shouldBe` (LamI (AppI (SymI 1) (AppI (SymI 0) (SymI 1))))

    it "tests betaI" $ do
        betaI (LamI (AppI (LamI (SymI 0)) (LamI (LamI (AppI (SymI 0) (LamI (SymI 2)))))))
          `shouldBe` (Just (LamI (LamI (LamI (AppI (SymI 0) (LamI (SymI 2)))))))

        betaI (SymI 0) `shouldBe` Nothing
        betaI (LamI (SymI 1)) `shouldBe` Nothing
        betaI (LamI (LamI (SymI 2))) `shouldBe` Nothing
        betaI (AppI (SymI 0) (SymI 0)) `shouldBe` Nothing
        betaI (LamI $ LamI (AppI (LamI $ LamI (AppI (SymI 1) (SymI 0))) (SymI 1)))
          `shouldBe` Just (LamI (LamI (LamI (AppI (SymI 2) (SymI 0)))))

    -- it "tests reductions" $ do
    --     -- telegram Kirill
    --     betaI (toTermI (app (lam "x" $ app (sym "x") (sym "x")) (lam "y" $ app (sym "z") (app (sym "y") (sym "z")))))
    --       `shouldBe` Just (AppI (LamI (AppI (SymI 1) (AppI (SymI 0) (SymI 1)))) (LamI (AppI (SymI 1) (AppI (SymI 0) (SymI 1)))))

    it "tests toTermS" $ do
        toTermS (Nil) `shouldBe` (lam "s" $ lam "z" $ sym "z")

    let one = (LamI (LamI (AppI (SymI 1) (SymI 0))))
    let four = (LamI (LamI (AppI (SymI 1) (AppI (SymI 1) (AppI (SymI 1) (AppI (SymI 1) (SymI 0)))))))
    let trueI = (LamI (LamI (SymI 1)))
    let falseI = (LamI (LamI (SymI 0)))

    it "tests solve" $ do
        solve (Natural 1) `shouldBe` Left one
        solve (Natural 4) `shouldBe` Left four
        solve (Plus (Natural 2) (Natural 2)) `shouldBe` Left four
        solve (Minus (Natural 9) (Natural 5)) `shouldBe` Left four
        solve (Mult (Natural 2) (Natural 2)) `shouldBe` Left four
        -- 5 + (2 * 2) - 5 = 4
        solve (Minus (Plus (Natural 5) (Mult (Natural 2) (Natural 2))) (Natural 5)) `shouldBe` Left four
        solve (Divide (Natural 8) (Natural 2)) `shouldBe` Left four

        -- todo test Y - it hangs on reduction
        -- solve (Y (Natural 4)) `shouldBe` Left four

        solve (IsNil Nil) `shouldBe` Left trueI
        solve (IsNil (Natural 4)) `shouldBe` Left falseI
        solve (IsNil (Natural 0)) `shouldBe` Left trueI
        -- solve (IsNil (Cons (Natural 2) Nil)) `shouldBe` Left falseI

        solve (Head (Cons (Natural 4) (Cons (Natural 2) (Cons (Natural 1) Nil))))
          `shouldBe` Left four

    it "tests solve with tests I'm not sure about" $ do
        solve (Cons (Natural 1) Nil)
          `shouldBe` Left (LamI (LamI (AppI (AppI (SymI 1) one) (AppI (AppI falseI (SymI 1)) (SymI 0)))))
        solve (Cons (Natural 4) (Cons (Natural 1) Nil))
          `shouldBe` Left (LamI (LamI (AppI (AppI (SymI 1) four) (AppI (AppI (AppI (AppI (LamI (LamI (LamI (LamI (AppI (AppI (SymI 1) (SymI 3)) (AppI (AppI (SymI 2) (SymI 1)) (SymI 0))))))) one) falseI) (SymI 1)) (SymI 0)))))

        solve (Tail (Cons (Natural 3) (Cons (Natural 4) (Cons (Natural 1) Nil))))
          `shouldBe` Left (LamI (LamI (AppI (AppI (SymI 1) four) (AppI (AppI (AppI (AppI (AppI (LamI (LamI (LamI (LamI (AppI (AppI (SymI 1) (SymI 3)) (AppI (AppI (SymI 2) (SymI 1)) (SymI 0))))))) one) falseI) (LamI (LamI (LamI (AppI (AppI (SymI 0) (SymI 2)) (AppI (SymI 1) (SymI 4))))))) (LamI (SymI 1))) (SymI 1)))))


