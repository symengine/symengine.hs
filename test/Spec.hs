import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

import Data.List
import Data.Ord

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
  ]
