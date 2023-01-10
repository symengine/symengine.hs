{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Monoid
import Data.Ord
-- import Symengine as Sym
import Data.Ratio
import Symengine.Internal
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Prelude hiding (pi)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- These are used to check invariants that can be tested by creating
-- random members of the type and then checking invariants on them

-- properties :: TestTree
-- properties = testGroup "Properties" [qcProps]

unitTests =
  testGroup
    "Unit tests"
    [ -- testCase "FFI Sanity Check - ASCII Art should be non-empty" $
      --   do
      --     ascii_art <- Sym.ascii_art_str
      --     assertBool "ASCII art from ascii_art_str is empty" (not . null $ ascii_art),
      testCase "test_complex" $ do
        let r = fromRational (100 % 47) :: Basic
            i = fromRational (76 % 59)
            e = r + i * im
        show e @?= "100/47 + 76/59*I"
        isSymbol e @?= False
        isRational e @?= False
        isInteger e @?= False
        isComplex e @?= True
        isZero e @?= False
        isNegative e @?= False
        isPositive e @?= False

        show (realPart e) @?= "100/47"
        isSymbol (realPart e) @?= False
        isRational (realPart e) @?= True
        isInteger (realPart e) @?= False
        isComplex (realPart e) @?= False

        show (imagPart e) @?= "76/59"
        isSymbol (imagPart e) @?= False
        isRational (imagPart e) @?= True
        isInteger (imagPart e) @?= False
        isComplex (imagPart e) @?= False,
      testCase "test_free_symbols" $ do
        let x = "x" :: Basic
            y = "y"
            z = "z"
            e = 123
            expr = (e + x) ** y / z

        setSize (freeSymbols expr) @?= 3
        toList (freeSymbols expr) @?= ["x", "y", "z"],
      testCase "test_function_symbols" $ do
        let x = "x" :: Basic
            y = "y"
            z = "z"
            g = mkFunction "g" [x]
            h = mkFunction "h" [g]
            f = mkFunction "f" [x + y, g, h]

        show (z + f) @?= "z + f(x + y, g(x), h(g(x)))"
        setSize (functionSymbols f) @?= 3
    ]
