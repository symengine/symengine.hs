{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- to write things like KnownNat(r * c) => ...
{-# LANGUAGE FlexibleContexts #-}
-- @
{-# LANGUAGE TypeApplications #-}
-- to bring stuff like (r, c) into scope
{-# LANGUAGE ScopedTypeVariables #-}

-- allow non injective type functions (+)
{-# LANGUAGE AllowAmbiguousTypes #-}

-- data declarations that are empty
{-# LANGUAGE EmptyDataDecls #-}
module Symengine.DenseMatrix
  (
   DenseMatrix,
   -- densematrix_new,
   densematrix_new_vec,
   densematrix_new_eye,
   densematrix_new_diag,
   densematrix_new_zeros,
   densematrix_get,
   densematrix_set,
   densematrix_size,
   
   -- arithmetic
   densematrix_add,
   densematrix_mul_matrix,
   densematrix_mul_scalar,
   det,
   inv,
   transpose,

   --decomposition
   L(L), D(D), U(U),
   densematrix_lu,
   densematrix_ldl,
   densematrix_fflu,
   densematrix_ffldu,
   densematrix_lu_solve,

   -- custom matrix class
   Matrix(..)

   --
   )
where

import Prelude
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import Control.Applicative
import Control.Monad -- for foldM
import System.IO.Unsafe
import Control.Monad
import GHC.Real
import Data.Proxy

import GHC.TypeLits -- type level programming
import qualified Data.Vector.Sized as V -- sized vectors
import Data.Finite -- types to represent numbers

import Symengine.Internal
import Symengine.BasicSym
import Symengine.VecBasic

class Matrix m where
  (<>) :: (KnownNat r, KnownNat c, KnownNat k) => m r k -> m k c -> m r c 


instance Matrix (DenseMatrix) where
  (<>) = densematrix_mul_matrix

data CDenseMatrix
data  DenseMatrix :: Nat -> Nat -> * where
  -- allow constructing raw DenseMatrix from a constructor
  DenseMatrix :: (KnownNat r, KnownNat c) => (ForeignPtr CDenseMatrix) -> DenseMatrix r c

instance  (KnownNat r, KnownNat c) => Wrapped (DenseMatrix  r c) CDenseMatrix where
    with (DenseMatrix p) f = withForeignPtr p f

instance  (KnownNat r, KnownNat c) =>  Show (DenseMatrix r c) where
  show :: DenseMatrix r c -> String
  show mat =
    unsafePerformIO $ with mat  (cdensematrix_str_ffi >=> peekCString)

instance (KnownNat r, KnownNat c) => Eq (DenseMatrix r c) where
  (==) :: DenseMatrix r c -> DenseMatrix r c -> Bool
  (==) mat1 mat2 = 
    1 == fromIntegral (unsafePerformIO $
                       with2 mat1 mat2 cdensematrix_eq_ffi)

instance (KnownNat r, KnownNat c) => Num (DenseMatrix r c) where
  (+) = densematrix_add
  (-) d1 d2 = let
        d2_neg =  densematrix_mul_scalar d2 (fromInteger (-1))
        in d1 + d2_neg
  -- TODO: Should be elementwise multiplcation
  (*) = undefined
  -- TODO: should be elementwise signum
  signum = undefined
  -- TODO: should be elementwise abs
  abs = undefined
  -- make a 1x1 matrix
  fromInteger = undefined -- densematrix_new_vec 

densematrix_new :: (KnownNat r, KnownNat c) => IO (DenseMatrix r c)
densematrix_new = DenseMatrix <$> (mkForeignPtr cdensematrix_new_ffi cdensematrix_free_ffi)

_densematrix_copy :: (KnownNat r, KnownNat c) => DenseMatrix r c -> IO (DenseMatrix r c)
_densematrix_copy mat = do
   newmat <- densematrix_new
   throwOnSymIntException =<< with2 newmat mat cdensematrix_set_ffi
   return newmat

densematrix_new_rows_cols ::  forall r c . (KnownNat r, KnownNat c) => DenseMatrix r c
densematrix_new_rows_cols = 
    unsafePerformIO $ DenseMatrix <$>
      (mkForeignPtr (cdensematrix_new_rows_cols_ffi 
                      (fromIntegral . natVal $ (Proxy @ r))
                      (fromIntegral . natVal $ (Proxy @ c)))
                      cdensematrix_free_ffi)
  

densematrix_new_vec :: forall r c. (KnownNat r, KnownNat c, KnownNat (r * c)) => V.Vector (r * c) BasicSym -> DenseMatrix r c
densematrix_new_vec syms = unsafePerformIO $ do
  vec <- vector_to_vecbasic syms
  let cdensemat =  with vec (\v -> cdensematrix_new_vec_ffi
                                     (fromIntegral . natVal $ (Proxy @ r))
                                     (fromIntegral . natVal $ (Proxy @ c)) v)
  DenseMatrix <$>  mkForeignPtr cdensemat cdensematrix_free_ffi


type Offset = Int
-- |create a matrix with rows 'r, cols 'c' and offset 'k'
densematrix_new_eye :: forall k r c. (KnownNat r,  KnownNat c, KnownNat k, KnownNat (r + k), KnownNat (c + k)) => DenseMatrix (r + k) (c + k)
densematrix_new_eye = unsafePerformIO $ do
  let mat = densematrix_new_rows_cols
  throwOnSymIntException =<< with mat (\m -> cdensematrix_eye_ffi m
                 (fromIntegral . natVal $ (Proxy @ r))
                 (fromIntegral . natVal $ (Proxy @ c))
                 (fromIntegral . natVal $ (Proxy @ k)))


  return mat

densematrix_new_zeros :: forall r c. (KnownNat r, KnownNat c) => DenseMatrix r c
densematrix_new_zeros = unsafePerformIO $ do
  let mat = densematrix_new_rows_cols
  throwOnSymIntException =<< with mat (\m -> cdensematrix_zeros_ffi m
                  (fromIntegral . natVal $ (Proxy @ r))
                  (fromIntegral . natVal $ (Proxy @ c)))
  return mat

-- create a matrix with diagonal elements of length 'd', offset 'k'
densematrix_new_diag :: forall k d. (KnownNat d, KnownNat k, KnownNat (d + k)) => V.Vector d BasicSym -> DenseMatrix (d + k) (d + k)
densematrix_new_diag syms  = unsafePerformIO $ do
  let offset = fromIntegral $ natVal (Proxy @ k)
  let diagonal = fromIntegral $ natVal (Proxy @ d)
  let dim = offset + diagonal
  vecsyms <- vector_to_vecbasic syms
  let mat = densematrix_new_rows_cols :: DenseMatrix (d + k) (d + k)
  throwOnSymIntException =<< with2 mat vecsyms (\m syms -> cdensematrix_diag_ffi m syms offset)


  return mat

type Row = Int
type Col = Int


densematrix_get :: forall r c. (KnownNat r, KnownNat c) => 
  DenseMatrix r c -> Finite r -> Finite c -> BasicSym
densematrix_get mat getr getc = unsafePerformIO $ do
      sym <- basicsym_new
      let indexr = fromIntegral $ (getFinite getr)
      let indexc = fromIntegral $ (getFinite getc)
      throwOnSymIntException =<< with2 mat sym (\m s -> cdensematrix_get_basic_ffi s m indexr indexc)

      return sym

densematrix_set :: forall r c. (KnownNat r, KnownNat c) => 
  DenseMatrix r c -> Finite r -> Finite c -> BasicSym -> DenseMatrix r c
densematrix_set mat r c sym = unsafePerformIO $ do
    mat' <- _densematrix_copy mat
    throwOnSymIntException =<< with2 mat' sym (\m s -> cdensematrix_set_basic_ffi 
                              m
                              (fromIntegral . getFinite $ r)
                              (fromIntegral . getFinite $ c)
                              s)

    return mat'


type NRows = Int
type NCols = Int

-- | provides dimenions of matrix. combination of the FFI calls
-- `dense_matrix_rows` and `dense_matrix_cols`
densematrix_size :: forall r c. (KnownNat r, KnownNat c) => DenseMatrix r c -> (NRows, NCols)
densematrix_size mat = 
   (fromIntegral . natVal $ (Proxy @ r), fromIntegral . natVal $ (Proxy @ c))

densematrix_add :: forall r c. (KnownNat r, KnownNat c) => 
  DenseMatrix r c -> DenseMatrix r c -> DenseMatrix r c
densematrix_add mata matb = unsafePerformIO $ do
   res <- densematrix_new
   throwOnSymIntException =<< with3 res mata matb cdensematrix_add_matrix_ffi
   return res


densematrix_mul_matrix :: forall r k c. (KnownNat r, KnownNat k, KnownNat c) => 
  DenseMatrix r k -> DenseMatrix k c -> DenseMatrix r c
densematrix_mul_matrix mata matb = unsafePerformIO $ do
   res <- densematrix_new
   throwOnSymIntException =<< with3 res mata matb cdensematrix_mul_matrix_ffi
   return res


densematrix_mul_scalar :: forall r c. (KnownNat r, KnownNat c) => 
  DenseMatrix r c -> BasicSym -> DenseMatrix r c
densematrix_mul_scalar mata sym = unsafePerformIO $ do
   res <- densematrix_new
   throwOnSymIntException =<< with3 res mata sym cdensematrix_mul_scalar_ffi
   return res

det :: forall r c. (KnownNat r, KnownNat c) => DenseMatrix r c -> BasicSym
det d = unsafePerformIO $ do
  sym <- basicsym_new
  throwOnSymIntException =<< with2 sym d cdensematrix_det_ffi 
  return sym

inv :: forall r c. (KnownNat r, KnownNat c) => DenseMatrix r c -> DenseMatrix r c
inv d = unsafePerformIO $ do
  m <- densematrix_new
  throwOnSymIntException =<< with2 m d cdensematrix_inv_ffi
  return m

transpose :: forall r c. (KnownNat r, KnownNat c) => DenseMatrix r c -> DenseMatrix r c
transpose d = unsafePerformIO $ do
  m <- densematrix_new
  throwOnSymIntException =<< with2 m d cdensematrix_transpose_ffi
  return m

newtype L r c = L (DenseMatrix r c)
newtype U r c = U (DenseMatrix r c)

densematrix_lu :: (KnownNat r, KnownNat c) => DenseMatrix r c-> (L r c, U r c)
densematrix_lu mat = unsafePerformIO $ do
   l <- densematrix_new
   u <- densematrix_new
   throwOnSymIntException =<< with3 l u mat cdensematrix_lu
   return (L l, U u)

newtype D r c = D (DenseMatrix r c)
densematrix_ldl :: (KnownNat r, KnownNat c) => DenseMatrix r c-> (L r c, D r c)
densematrix_ldl mat = unsafePerformIO $ do
  l <- densematrix_new
  d <- densematrix_new
  throwOnSymIntException =<< with3 l d mat cdensematrix_ldl

  return (L l, D d)


newtype FFLU r c = FFLU (DenseMatrix r c)
densematrix_fflu :: (KnownNat r, KnownNat c) => DenseMatrix r c -> FFLU r c
densematrix_fflu mat = unsafePerformIO $ do
  fflu <- densematrix_new
  throwOnSymIntException =<< with2 fflu mat cdensematrix_fflu
  return (FFLU fflu)


densematrix_ffldu ::  (KnownNat r, KnownNat c) =>
  DenseMatrix r c -> (L r c, D r c, U r c)
densematrix_ffldu mat = unsafePerformIO $ do
  l <- densematrix_new
  d <- densematrix_new
  u <- densematrix_new

  throwOnSymIntException =<< with4 l d u mat cdensematrix_ffldu
  return (L l, D d, U u)

-- solve A x = B
-- A is first param, B is second larameter
densematrix_lu_solve :: (KnownNat r, KnownNat c) => 
  DenseMatrix r c -> DenseMatrix r c -> DenseMatrix r c
densematrix_lu_solve a b = unsafePerformIO $ do
  x <- densematrix_new
  throwOnSymIntException =<< with3 x a b cdensematrix_lu_solve
  return x

foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_new" cdensematrix_new_ffi :: IO (Ptr CDenseMatrix)
foreign import ccall unsafe "symengine/cwrapper.h &dense_matrix_free" cdensematrix_free_ffi :: FunPtr ((Ptr CDenseMatrix) -> IO ())
foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_new_rows_cols" cdensematrix_new_rows_cols_ffi :: CUInt -> CUInt -> IO (Ptr CDenseMatrix)
foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_new_vec" cdensematrix_new_vec_ffi :: CUInt -> CUInt -> Ptr CVecBasic -> IO (Ptr CDenseMatrix)
foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_zeros" cdensematrix_zeros_ffi :: Ptr CDenseMatrix -> CULong -> CULong -> IO CInt
foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_eye" cdensematrix_eye_ffi :: Ptr CDenseMatrix -> CULong -> CULong  -> CULong -> IO CInt
foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_diag" cdensematrix_diag_ffi :: Ptr CDenseMatrix -> Ptr CVecBasic -> CULong  -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_eq" cdensematrix_eq_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_set" cdensematrix_set_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_str" cdensematrix_str_ffi :: Ptr CDenseMatrix -> IO CString

foreign import ccall "symengine/cwrapper.h dense_matrix_get_basic" cdensematrix_get_basic_ffi :: Ptr (CBasicSym)  -> Ptr CDenseMatrix -> CUInt -> CUInt -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_set_basic" cdensematrix_set_basic_ffi :: Ptr CDenseMatrix -> CUInt -> CUInt -> Ptr (CBasicSym)  -> IO CInt


foreign import ccall "symengine/cwrapper.h dense_matrix_rows" cdensematrix_rows_ffi :: Ptr CDenseMatrix -> IO CULong
foreign import ccall "symengine/cwrapper.h dense_matrix_cols" cdensematrix_cols_ffi :: Ptr CDenseMatrix -> IO CULong

foreign import ccall "symengine/cwrapper.h dense_matrix_add_matrix"
  cdensematrix_add_matrix_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_mul_matrix"
  cdensematrix_mul_matrix_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_mul_scalar"
  cdensematrix_mul_scalar_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CBasicSym -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_det"
  cdensematrix_det_ffi :: Ptr CBasicSym -> Ptr CDenseMatrix -> IO CInt


foreign import ccall "symengine/cwrapper.h dense_matrix_inv"
  cdensematrix_inv_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt


foreign import ccall "symengine/cwrapper.h dense_matrix_transpose"
  cdensematrix_transpose_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_LU" cdensematrix_lu :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_LDL" cdensematrix_ldl :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_FFLU" cdensematrix_fflu :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_FFLDU" cdensematrix_ffldu :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_LU_solve" cdensematrix_lu_solve:: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
