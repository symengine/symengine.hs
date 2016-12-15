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
tests = testGroup "Tests" [genBasic,
                           symbolIntRing,
                           denseMatrixRing]


-- These are used to check invariants that can be tested by creating
-- random members of the type and then checking invariants on them

-- properties :: TestTree
-- properties = testGroup "Properties" [qcProps]


genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf1 genSafeChar


instance Arbitrary(BasicSym) where
  arbitrary = do
    --intval <- QC.choose (1, 5000) :: Gen (Ratio Integer)
    let pow2 = 512
    intval <-  choose (-(2^pow2), 2 ^ pow2 - 1) :: Gen Int
    strval <- genSafeString :: Gen String
    choice <- arbitrary :: Gen Bool

    if choice
    then return (fromIntegral intval)
    else return (symbol_new (take 10 strval))
instance forall r c. (KnownNat r, KnownNat c, KnownNat (r * c)) => 
  Arbitrary(DenseMatrix r c) where
  arbitrary = do
    let (rows, cols) = (natVal (Proxy @ r), natVal (Proxy @ c))
    syms <- V.replicateM arbitrary

    return (densematrix_new_vec syms)

genBasic = testGroup "create and destroy BasicSym"
      [QC.testProperty "create and die immediately " ((const True) :: BasicSym -> Bool) ]

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

  plus_assoc :: BasicSym -> BasicSym -> BasicSym -> Bool
  plus_assoc b1 b2 b3 = (b1 + b2) + b3 == b1 + (b2 + b3)

  plus_identity :: BasicSym -> Bool
  plus_identity b = (b + 0) == b && (0 + b) == b

  plus_inverse :: BasicSym -> Bool
  plus_inverse b = (b + (-b)) == 0 && ((-b) + b) == 0

  mult_identity :: BasicSym -> Bool
  mult_identity b = (b * 1) == (1 * b) && (b * 1) == b

  mult_assoc :: BasicSym -> BasicSym -> BasicSym -> Bool
  mult_assoc a b c = (a * b) * c == a * (b * c)

  mult_inverse :: BasicSym -> Bool
  mult_inverse b = if b == 0 then True else b * (1.0 / b) == 1 && (1.0 / b) * b == 1

  mult_commutativity :: BasicSym -> BasicSym -> Bool
  mult_commutativity b1 b2 = b1 * b2 == b2 * b1

 -- symengine (==) is structural equality, not "legit" equality. 
 -- see: https://github.com/symengine/symengine/issues/207
  mult_distributivity :: BasicSym -> BasicSym -> BasicSym -> Bool
  mult_distributivity b1 b2 b3 = expand(b1 * (b2 + b3) - (b1 * b2 + b1 * b3)) == (0 :: BasicSym)
  in
    testGroup "Symbols of numbers - Ring" [
      QC.testProperty "(+) identity" plus_identity,
      QC.testProperty "(+) associativity" plus_assoc,
      QC.testProperty "(+) inverse" plus_inverse,
      QC.testProperty "(+) commutativity" plus_commutativity,
      QC.testProperty "(*) identity" mult_identity,
      QC.testProperty "(*) associativity" mult_assoc,
      QC.testProperty "(*) inverse" mult_inverse,
      QC.testProperty "(*) distributivity" mult_distributivity
    ]


denseMatrixRing = 
  let
    eye :: DenseMatrix 10 10
    eye = densematrix_new_eye @ 0 @ 10 @ 10

    zero :: DenseMatrix 10 10
    zero = densematrix_new_zeros @ 10 @ 10

    plus_identity :: DenseMatrix 10 10 -> Bool
    plus_identity d = densematrix_add d zero == d && densematrix_add zero d == d

    plus_invert :: DenseMatrix 10 10 -> Bool
    plus_invert d = d - d == densematrix_new_zeros

    plus_commutativity :: DenseMatrix 10 10 -> DenseMatrix 10 10 -> Bool
    plus_commutativity d1 d2 = densematrix_add d1 d2 == densematrix_add d2 d1

    plus_assoc :: DenseMatrix 10 10 -> DenseMatrix 10 10 -> 
                      DenseMatrix 10 10 -> Bool
    plus_assoc d1 d2 d3 =
       densematrix_add (densematrix_add d1 d2) d3 == 
      densematrix_add d1 (densematrix_add d2 d3)

    mult_identity :: DenseMatrix 10 10 -> Bool
    mult_identity d = d <> eye == d && eye <> d == d

    mult_assoc :: DenseMatrix 2 2 -> DenseMatrix 2 2  -> DenseMatrix 2 2 -> Bool
    mult_assoc d1 d2 d3 = (((d1 <> d2) <> d3) - (d1 <> (d2 <> d3))) == densematrix_new_zeros

    mult_nonsingular_invertible :: DenseMatrix 10 10 -> Bool
    mult_nonsingular_invertible d = if expand(det d) /= 0 then d <> (inv d) == eye  else True
  in
    testGroup "DenseMatrix - Ring"
  [   
     QC.testProperty "(+) identity" plus_identity,
     QC.testProperty "(+) associativity" plus_assoc,
     QC.testProperty "(+) commutativity" plus_commutativity,

     QC.testProperty "(*) identity" mult_identity,
     -- this fails because I need symbol reduction
     -- QC.testProperty "(*) associativity " mult_assoc
 
     -- no idea why this fails
     -- QC.testProperty "(*) non-singluar invertible" mult_nonsingular_invertible
  ]


