{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
module Symengine.DenseMatrix
  (
   DenseMatrix,
   -- densematrix_new,
   densematrix_new_vec,
   densematrix_new_eye,
   densematrix_new_diag,
   densematrix_get,
   densematrix_set,
   densematrix_size,
   -- arithmetic
   densematrix_add,
   densematrix_mul_matrix,
   densematrix_mul_scalar,
   --decomposition
   L(L), D(D), U(U),
   densematrix_lu,
   densematrix_ldl,
   densematrix_fflu,
   densematrix_ffldu,
   densematrix_lu_solve)
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

import Symengine.Internal
import Symengine.BasicSym
import Symengine.VecBasic

data CDenseMatrix = CDenseMatrix
newtype  DenseMatrix = DenseMatrix (ForeignPtr CDenseMatrix)

instance Wrapped DenseMatrix CDenseMatrix where
    with (DenseMatrix p) f = withForeignPtr p f

instance Show (DenseMatrix) where
  show :: DenseMatrix -> String
  show mat =
    unsafePerformIO $ with mat  (cdensematrix_str_ffi >=> peekCString)

instance Eq (DenseMatrix) where
  (==) :: DenseMatrix -> DenseMatrix -> Bool
  (==) mat1 mat2 = 
    1 == fromIntegral (unsafePerformIO $
                       with2 mat1 mat2 cdensematrix_eq_ffi)

densematrix_new :: IO DenseMatrix
densematrix_new = DenseMatrix <$> (mkForeignPtr cdensematrix_new_ffi cdensematrix_free_ffi)

type NRows = Int
type NCols = Int

densematrix_new_rows_cols :: NRows -> NCols -> DenseMatrix
densematrix_new_rows_cols r c = unsafePerformIO $ DenseMatrix <$>
  mkForeignPtr (cdensematrix_new_rows_cols_ffi (fromIntegral r) (fromIntegral c)) cdensematrix_free_ffi

densematrix_new_vec :: NRows -> NCols -> [BasicSym] -> DenseMatrix
densematrix_new_vec r c syms = unsafePerformIO $ do
  vec <- list_to_vecbasic syms
  let cdensemat =  with vec (\v ->  cdensematrix_new_vec_ffi (fromIntegral r) (fromIntegral c) v)
  DenseMatrix <$>  mkForeignPtr cdensemat cdensematrix_free_ffi


type Offset = Int
-- create a matrix with 1's on the diagonal offset by offset
densematrix_new_eye :: NRows -> NCols -> Offset -> DenseMatrix
densematrix_new_eye r c offset = unsafePerformIO $ do
  let mat = densematrix_new_rows_cols r c
  with mat (\m -> cdensematrix_eye_ffi m
                 (fromIntegral r)
                 (fromIntegral c)
                 (fromIntegral offset))
  return mat

-- create a matrix with diagonal elements at offest k
densematrix_new_diag :: [BasicSym] -> Int -> DenseMatrix
densematrix_new_diag syms offset = unsafePerformIO $ do
  let dim = length syms
  vecsyms <- list_to_vecbasic syms
  let mat = densematrix_new_rows_cols dim dim
  with2 mat vecsyms (\m vs -> cdensematrix_diag_ffi m vs (fromIntegral offset))

  return mat

type Row = Int
type Col = Int

densematrix_get :: DenseMatrix -> Row -> Col -> BasicSym
densematrix_get mat r c = unsafePerformIO $ do
      sym <- basicsym_new
      with2 mat sym (\m s -> cdensematrix_get_basic_ffi s m (fromIntegral r) (fromIntegral c))
      return sym

densematrix_set :: DenseMatrix -> Row -> Col -> BasicSym -> DenseMatrix
densematrix_set mat r c sym = unsafePerformIO $ do
    with2 mat sym (\m s -> cdensematrix_set_basic_ffi m (fromIntegral r) (fromIntegral c) s)
    return mat


-- | provides dimenions of matrix. combination of the FFI calls
-- `dense_matrix_rows` and `dense_matrix_cols`
densematrix_size :: DenseMatrix -> (NRows, NCols)
densematrix_size mat = unsafePerformIO $  do
   rs <- with mat cdensematrix_rows_ffi
   cs <- with mat cdensematrix_cols_ffi
   return (fromIntegral rs, fromIntegral cs)

densematrix_add :: DenseMatrix -> DenseMatrix -> DenseMatrix
densematrix_add mata matb = unsafePerformIO $ do
   res <- densematrix_new
   with3 res mata matb cdensematrix_add_matrix
   return res


densematrix_mul_matrix :: DenseMatrix -> DenseMatrix -> DenseMatrix
densematrix_mul_matrix mata matb = unsafePerformIO $ do
   res <- densematrix_new
   with3 res mata matb cdensematrix_mul_matrix
   return res


densematrix_mul_scalar :: DenseMatrix -> BasicSym -> DenseMatrix
densematrix_mul_scalar mata sym = unsafePerformIO $ do
   res <- densematrix_new
   with3 res mata sym cdensematrix_mul_scalar
   return res


newtype L = L DenseMatrix
newtype U = U DenseMatrix

densematrix_lu :: DenseMatrix -> (L, U)
densematrix_lu mat = unsafePerformIO $ do
   l <- densematrix_new
   u <- densematrix_new
   with3 l u mat cdensematrix_lu
   return (L l, U u)

newtype D = D DenseMatrix
densematrix_ldl :: DenseMatrix -> (L, D)
densematrix_ldl mat = unsafePerformIO $ do
  l <- densematrix_new
  d <- densematrix_new
  with3 l d mat cdensematrix_ldl

  return (L l, D d)


newtype FFLU = FFLU DenseMatrix
densematrix_fflu :: DenseMatrix -> FFLU
densematrix_fflu mat = unsafePerformIO $ do
  fflu <- densematrix_new
  with2 fflu mat cdensematrix_fflu
  return (FFLU fflu)


densematrix_ffldu :: DenseMatrix -> (L, D, U)
densematrix_ffldu mat = unsafePerformIO $ do
  l <- densematrix_new
  d <- densematrix_new
  u <- densematrix_new

  with4 l d u mat cdensematrix_ffldu
  return (L l, D d, U u)

-- solve A x = B
-- A is first param, B is second larameter
densematrix_lu_solve :: DenseMatrix -> DenseMatrix -> DenseMatrix
densematrix_lu_solve a b = unsafePerformIO $ do
  x <- densematrix_new
  with3 x a b cdensematrix_lu_solve
  return x

foreign import ccall "symengine/cwrapper.h dense_matrix_new" cdensematrix_new_ffi :: IO (Ptr CDenseMatrix)
foreign import ccall "symengine/cwrapper.h &dense_matrix_free" cdensematrix_free_ffi :: FunPtr ((Ptr CDenseMatrix) -> IO ())
foreign import ccall "symengine/cwrapper.h dense_matrix_new_rows_cols" cdensematrix_new_rows_cols_ffi :: CUInt -> CUInt -> IO (Ptr CDenseMatrix)
foreign import ccall "symengine/cwrapper.h dense_matrix_new_vec" cdensematrix_new_vec_ffi :: CUInt -> CUInt -> Ptr CVecBasic -> IO (Ptr CDenseMatrix)
foreign import ccall "symengine/cwrapper.h dense_matrix_eye" cdensematrix_eye_ffi :: Ptr CDenseMatrix -> CULong -> CULong  -> CULong -> IO ()
foreign import ccall "symengine/cwrapper.h dense_matrix_diag" cdensematrix_diag_ffi :: Ptr CDenseMatrix -> Ptr CVecBasic -> CULong  -> IO ()
foreign import ccall "symengine/cwrapper.h dense_matrix_eq" cdensematrix_eq_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_str" cdensematrix_str_ffi :: Ptr CDenseMatrix -> IO CString

foreign import ccall "symengine/cwrapper.h dense_matrix_get_basic" cdensematrix_get_basic_ffi :: Ptr (CBasicSym)  -> Ptr CDenseMatrix -> CUInt -> CUInt -> IO (Ptr CDenseMatrix)
foreign import ccall "symengine/cwrapper.h dense_matrix_set_basic" cdensematrix_set_basic_ffi :: Ptr CDenseMatrix -> CUInt -> CUInt -> Ptr (CBasicSym)  -> IO ()


foreign import ccall "symengine/cwrapper.h dense_matrix_rows" cdensematrix_rows_ffi :: Ptr CDenseMatrix -> IO CULong
foreign import ccall "symengine/cwrapper.h dense_matrix_cols" cdensematrix_cols_ffi :: Ptr CDenseMatrix -> IO CULong

foreign import ccall "symengine/cwrapper.h dense_matrix_add_matrix" cdensematrix_add_matrix :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO ()
foreign import ccall "symengine/cwrapper.h dense_matrix_mul_matrix" cdensematrix_mul_matrix :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO ()
foreign import ccall "symengine/cwrapper.h dense_matrix_mul_scalar" cdensematrix_mul_scalar :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CBasicSym -> IO ()

foreign import ccall "symengine/cwrapper.h dense_matrix_LU" cdensematrix_lu :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO ()
foreign import ccall "symengine/cwrapper.h dense_matrix_LDL" cdensematrix_ldl :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO ()
foreign import ccall "symengine/cwrapper.h dense_matrix_FFLU" cdensematrix_fflu :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO ()
foreign import ccall "symengine/cwrapper.h dense_matrix_FFLDU" cdensematrix_ffldu :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO ()
foreign import ccall "symengine/cwrapper.h dense_matrix_LU_solve" cdensematrix_lu_solve:: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO ()
