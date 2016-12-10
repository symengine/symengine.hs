import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

import Data.List
import Data.Ord
import Data.Monoid

import Symengine as Sym
import Symengine.DenseMatrix
import Symengine.VecBasic
import Symengine.BasicSym
import Prelude hiding (pi)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [basicTests, vectorTests, denseMatrixTests]


-- These are used to check invariants that can be tested by creating
-- random members of the type and then checking invariants on them

-- properties :: TestTree
-- properties = testGroup "Properties" [qcProps]

instance Arbitrary(BasicSym) where
  arbitrary = do
    intval <- arbitrary :: Gen Int
    strval <- arbitrary :: Gen [Char]
    choice <- arbitrary

    if choice
      then return (fromIntegral intval)
      else return (symbol_new strval)

instance Arbitrary(DenseMatrix) where
  arbitrary = do
    rows <- arbitrary
    cols <- arbitrary
    syms <- arbitrary

    return (densematrix_new_vec rows cols (take rows cols syms))

basicTests = testGroup "Basic tests"
  [ HU.testCase "ascii art" $
    do
      ascii_art <- ascii_art_str
      HU.assertBool "ASCII art from ascii_art_str is empty" (not . null $ ascii_art)
    ,
    HU.testCase "Basic Constructors" $
    do
      "0" @?= (show zero)
      "1" @?= (show one)
      "-1" @?= (show minus_one)
    ,
    HU.testCase "Basic Trignometric Functions" $
    do
      let pi_over_3 = pi / 3 :: BasicSym
      let pi_over_2 = pi / 2 :: BasicSym

      sin zero @?= zero
      cos zero @?= one

      sin (pi / 6) @?= 1 / 2
      sin (pi / 3) @?= (3 ** (1/2)) / 2

      cos (pi / 6) @?= (3 ** (1/2)) / 2
      cos (pi / 3) @?= 1 / 2

      sin pi_over_2 @?= one
      cos pi_over_2 @?= zero
   ,
   HU.testCase "New Symbols, differentiation" $ 
   do
      let x = symbol_new "x"
      let y = symbol_new "y"

      x - x @?= zero
      x + y @?= y + x
      diff (x ** 2 + y) x @?= 2 * x
      diff (x * y) x @?= y
      diff (sin x) x @?= cos x
      diff (cos x) x @?= -(sin x)

  ]
-- tests for vectors
vectorTests = testGroup "Vector"
    [ HU.testCase "Vector - create, push_back, get out value" $
      do
        let v = vecbasic_new
        vecbasic_push_back v (11 :: BasicSym)
        vecbasic_push_back v (12 :: BasicSym)

        vecbasic_get v 0 @?= Right (11 :: BasicSym)
        vecbasic_get v 1 @?= Right (12 :: BasicSym)
        vecbasic_get v 101 @?= Left RuntimeError
    ]


propertyDensematrixAddComm :: DenseMatrix -> DenseMatrix -> Bool
propertyDensematrixAddComm d1 d2 = densematrix_add d1 d2 == densematrix_add d2 d1

-- tests for dense matrices
denseMatrixTests = testGroup "Dense Matrix"
  [ HU.testCase "Create matrix, test string representation, values" $
    do
      let syms = [1, 2, 3, 4]
      let mat = densematrix_new_vec 2 2 syms

      densematrix_get mat 0 0  @?= 1
      densematrix_get mat 0 1  @?= 2
      densematrix_get mat 1 0  @?= 3
      densematrix_get mat 1 1  @?= 4
    , HU.testCase "test set for matrix" $
        do
          let syms = [1, 2, 3, 4]
          let mat = densematrix_new_vec 2 2 syms

          densematrix_get (densematrix_set mat 0 0 10) 0 0 @?= 10
          densematrix_get (densematrix_set mat 0 1 11) 0 1 @?= 11
   , HU.testCase "test get_size for matrix" $
     do
       let syms = [1, 2, 3, 4, 5, 6]
       let mat = densematrix_new_vec 2 3 syms
       densematrix_size mat @?= (2, 3)
  , HU.testCase "Identity matrix" $
    do
      let eye = densematrix_new_eye 2 2 0
      let correct = densematrix_new_vec 2 2 [1, 0, 0, 1]
      eye @?= eye
  , HU.testCase "diagonal matrix" $
    do
     let diag = densematrix_new_diag [1, 2, 3] 1
     let correct = densematrix_new_vec 4 4 [0, 1, 0, 0,
                                         0, 0, 2, 0,
                                         0, 0, 0, 3,
                                         0, 0, 0, 0]
     print diag
     print correct
     diag @=? correct
  , HU.testCase "Dense Matrix + Dense Matrix" $ do
      let eye = densematrix_new_eye 2 2 0
      let ans = densematrix_new_vec 2 2 [2, 0,
                                         0, 2]
      densematrix_add eye eye @=? ans
      -- figure out how to use QuickCheck for this
  , QC.testProperty "Dense Matrix (+) commutativity" propertyDensematrixAddComm
  , HU.testCase "Dense Matrix * scalar" $ do
      False @=? True

  , HU.testCase "Dense Matrix * Matrix" $ do
      False @=? True

  , HU.testCase "Dense Matrix LU" $ do
      False @=? True 
  , HU.testCase "Dense Matrix LDL" $ do
      False @=? True
  , HU.testCase "Dense Matrix FFLU" $ do
      False @=? True
  , HU.testCase "Dense Matrix FFLDU" $ do
      False @=? True
  , HU.testCase "Dense Matrix LU Solve" $ do
      let a = densematrix_new_eye 2 2 0
      let b = densematrix_new_eye 2 2 0
      False @=? True
   ]
