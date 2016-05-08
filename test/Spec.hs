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

unitTests = testGroup "FFI Sanity Checks"
  [ HU.testCase "ASCII Art should be non-empty" $ 
    do
      ascii_art <- Sym.ascii_art_str
      HU.assertBool "ASCII art from ascii_art_str is empty" (not . null $ ascii_art)


    , HU.testCase "Basic Constructors" $
    do
      zero <- basic_const_zero
      one <- basic_const_one
      minus_one <- basic_const_minus_one
      "0" @?= (show 0)     
      "1" @?= (show one)     
      "-1" @?= (show minus_one)     
  ]
