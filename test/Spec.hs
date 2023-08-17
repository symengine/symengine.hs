{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.Ratio
import Data.Text (pack)
import Symengine
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Num" $ do
    prop "Integer" $ \(a :: Integer) (b :: Integer) -> do
      show (fromIntegral @_ @Basic a) `shouldBe` show a
      show (fromIntegral @_ @Basic a + fromIntegral b) `shouldBe` show (a + b)
      show (fromIntegral @_ @Basic a - fromIntegral b) `shouldBe` show (a - b)
      show (fromIntegral @_ @Basic a * fromIntegral b) `shouldBe` show (a * b)
      show (abs (fromIntegral @_ @Basic a)) `shouldBe` show (abs a)
      show (negate (fromIntegral @_ @Basic a)) `shouldBe` show (negate a)
      show (signum (fromIntegral @_ @Basic a)) `shouldBe` show (signum a)
  describe "AST" $ do
    prop "SymengineInteger" $ \(x :: Integer) -> do
      toAST (fromInteger x) `shouldBe` SymengineInteger x
    prop "SymengineRational" $ \(x :: Rational) -> do
      if denominator x == 1
        then toAST (fromRational x) `shouldBe` SymengineInteger (numerator x)
        else toAST (fromRational x) `shouldBe` SymengineRational x
    it "SymengineConstant" $ do
      toAST (pi :: Basic) `shouldBe` SymengineConstant pi
      toAST e `shouldBe` SymengineConstant e
    prop "SymengineSymbol" $ \(x :: String) -> do
      unless ('\NUL' `elem` x) $
        toAST (symbol (pack x)) `shouldBe` SymengineSymbol (pack x)
    it "SymengineInfinity" $ do
      toAST infinity `shouldBe` SymengineInfinity
    it "SymengineNaN" $ do
      toAST nan `shouldBe` SymengineNaN
    it "SymengineAdd" $ do
      toAST (symbol "x" + symbol "z" + symbol "y") `shouldBe` SymengineAdd [symbol "x", symbol "z", symbol "y"]
      toAST (symbol "x" + symbol "y") `shouldBe` SymengineAdd [symbol "x", symbol "y"]
    it "SymengineMul" $ do
      toAST (2 * symbol "y") `shouldBe` SymengineMul [2, symbol "y"]
      toAST (-symbol "x") `shouldBe` SymengineMul [-1, symbol "x"]
    it "SymenginePow" $ do
      toAST (sqrt (symbol "y")) `shouldBe` SymenginePow (symbol "y") 0.5
      toAST (exp (symbol "y")) `shouldBe` SymenginePow e (symbol "y")
    it "SymengineLog" $ do
      toAST (log (symbol "y")) `shouldBe` SymengineLog (symbol "y")
    it "SymengineSign" $ do
      toAST (signum (symbol "y")) `shouldBe` SymengineSign (symbol "y")
    it "SymengineFunction" $ do
      toAST (parse "f(1, x, y + 2)") `shouldBe` SymengineFunction "f" [1, symbol "x", 2 + symbol "y"]
      show (fromAST (SymengineFunction "f" [1, symbol "x", 2 + symbol "y"])) `shouldBe` "f(1, x, 2 + y)"
    it "SymengineDerivative" $ do
      toAST (diff (parse "f(x)") (symbol "x")) `shouldBe` SymengineDerivative (parse "f(x)") [symbol "x"]
      toAST (diff (symbol "x" ** 2) (symbol "x")) `shouldBe` SymengineMul [2, symbol "x"]

  describe "subs" $ do
    it "" $ do
      subs [("x", 1)] "a + f(x) / x" `shouldBe` "a + f(1)"
      subs [("k", "c")] "a + b" `shouldBe` "a + b"
      subs [] "a + b" `shouldBe` "a + b"
      subs [("a + b", "c")] "a + b" `shouldBe` "c"
  describe "Misc" $ do
    it "" $ do
      print $ parse "a + f(x) / x - 4**2"
      print $ evalf EvalSymbolic 20 $ parse "a + 8/3 * f(x) / x - 4**2"
      print $ inverse InverseDefault (identityMatrix 3)
