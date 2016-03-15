import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

import Data.List
import Data.Ord

import Lib as L

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]


-- These are used to check invariants that can be tested by creating
-- random members of the type and then checking invariants on them

-- properties :: TestTree
-- properties = testGroup "Properties" [qcProps]

-- qcProps = testGroup "(checked by QuickCheck)"

unitTests = testGroup "Unit tests"
  [ HU.testCase "FFI Sanity Check - ASCII Art" $ 
    do
      ascii_art <- L.ascii_art_str
      let expected_str = " _____           _____         _         \n|   __|_ _ _____|   __|___ ___|_|___ ___ \n|__   | | |     |   __|   | . | |   | -_|\n|_____|_  |_|_|_|_____|_|_|_  |_|_|_|___|\n      |___|               |___|          \n"
      HU.assertEqual "ASCII art from ascii_art_str is wrong" expected_str ascii_art
  ]
