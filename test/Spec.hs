import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

import Data.List
import Data.Ord
import Data.Monoid

import Symengine as Sym

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]


-- These are used to check invariants that can be tested by creating
-- random members of the type and then checking invariants on them

-- properties :: TestTree
-- properties = testGroup "Properties" [qcProps]

unitTests = testGroup "Unit tests"
  [ HU.testCase "FFI Sanity Check - ASCII Art should be non-empty" $ 
    do
      ascii_art <- Sym.ascii_art_str
      HU.assertBool "ASCII art from ascii_art_str is empty" (not . null $ ascii_art)


    , HU.testCase "Basic Constructors" $
    do
      let zero = basic_const_zero
      let one = basic_const_one
      let minus_one = basic_const_minus_one
      "0" @?= (show zero)     
      "1" @?= (show one)     
      "-1" @?= (show minus_one)     
  ]
