-- for @
{-# LANGUAGE TypeApplications #-}

-- for forall r c capturing with Proxy
{-# LANGUAGE ScopedTypeVariables #-}

-- lift 2, 3, etc to type level
{-# LANGUAGE DataKinds #-}


-- for *, + in type sigs
{-# LANGUAGE TypeOperators #-}


-- for *, + in type sigs
{-# LANGUAGE FlexibleContexts #-}

-- for (*) which is not injective
{-# LANGUAGE UndecidableInstances #-}
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

import Data.List
import Data.Ord
import Data.Monoid
import Data.Ratio

import Symengine as Sym
import Symengine.DenseMatrix
import Symengine.VecBasic
import Symengine.BasicSym
import Foreign.C.Types
import Prelude hiding (pi)


-- TODO: move arbitrary instance _inside_ the library
import GHC.TypeLits
import Data.Proxy
import qualified Data.Vector.Sized as V

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [basicTests, 
                           vectorTests,
                           denseMatrixImperative,
                           symbolIntRing,
                           denseMatrixPlusGroup]


-- These are used to check invariants that can be tested by creating
-- random members of the type and then checking invariants on them

-- properties :: TestTree
-- properties = testGroup "Properties" [qcProps]

instance Arbitrary(BasicSym) where
  arbitrary = do
    --intval <- QC.choose (1, 5000) :: Gen (Ratio Integer)
    let pow2 = 32
    intval <-  choose (-2^pow2, 2 ^ pow2 - 1) :: Gen Int
    return (fromIntegral intval)

instance forall r c. (KnownNat r, KnownNat c, KnownNat (r * c)) => 
  Arbitrary(DenseMatrix r c) where
  arbitrary = do
    let (rows, cols) = (natVal (Proxy @ r), natVal (Proxy @ c))
    syms <- V.replicateM arbitrary

    return (densematrix_new_vec syms)

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
        v <- vecbasic_new
        vecbasic_push_back v (11 :: BasicSym)
        vecbasic_push_back v (12 :: BasicSym)

        vecbasic_get v 0 @?= Right (11 :: BasicSym)
        vecbasic_get v 1 @?= Right (12 :: BasicSym)
        vecbasic_get v 101 @?= Left RuntimeError
    ]

-- tests for symbol(ints)
symbolIntRing = let
  plus_commutativity :: BasicSym -> BasicSym -> Bool
  plus_commutativity b1 b2 = b1 + b2 == b2 + b1

  plus_associativity :: BasicSym -> BasicSym -> BasicSym -> Bool
  plus_associativity b1 b2 b3 = (b1 + b2) + b3 == b1 + (b2 + b3)
  in
    testGroup "Symbols of Ints - Ring" [
      QC.testProperty "(+) commutativity" plus_commutativity,
      QC.testProperty "(+) associativity" plus_associativity
    ]

-- tests for dense matrices
denseMatrixImperative = testGroup "Dense Matrix - Create, Get/Set"
  [ HU.testCase "Create matrix, test getters" $ 
    do
      let syms = V.generate (\pos -> fromIntegral (pos + 1))
      let mat = densematrix_new_vec syms :: DenseMatrix 2 2

      densematrix_get mat 0 0  @?= 1
      densematrix_get mat 0 1  @?= 2
      densematrix_get mat 1 0  @?= 3
      densematrix_get mat 1 1  @?= 4
    , HU.testCase "test set for matrix" $
        do
          let syms = V.generate (\pos -> fromIntegral (pos + 1))
          let mat = densematrix_new_vec syms :: DenseMatrix 2 2

          densematrix_get (densematrix_set mat 0 0 10) 0 0 @?= 10
          densematrix_get (densematrix_set mat 0 1 11) 0 1 @?= 11
  ]

denseMatrixPlusGroup = 
  let
    commutativity :: DenseMatrix 10 10 -> DenseMatrix 10 10 -> Bool
    commutativity d1 d2 = densematrix_add d1 d2 == densematrix_add d2 d1

    associativity :: DenseMatrix 10 10 -> DenseMatrix 10 10 -> 
                      DenseMatrix 10 10 -> Bool
    associativity d1 d2 d3 =
       densematrix_add (densematrix_add d1 d2) d3 == 
      densematrix_add d1 (densematrix_add d2 d3)
  in
    testGroup "DenseMatrix - (+) is commutative group"
  [   QC.testProperty "commutativity" commutativity,
      QC.testProperty "associativity" associativity
  
  ]
{-
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
     diag @=? correct
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
-}
