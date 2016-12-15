{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- @
{-# LANGUAGE TypeApplications #-}
-- to bring stuff like (r, c) into scope
{-# LANGUAGE ScopedTypeVariables #-}
module Symengine.VecBasic
  (
    VecBasic,
    vecbasic_new,
    vecbasic_push_back,
    vecbasic_get,
    vecbasic_size,
    vector_to_vecbasic,
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
import Symengine

import GHC.TypeLits -- type level programming
import qualified Data.Vector.Sized as V -- sized vectors

import Symengine.Internal
import Symengine.BasicSym

-- |represents a symbol exported by SymEngine. create this using the functions
-- 'zero', 'one', 'minus_one', 'e', 'im', 'rational', 'complex', and also by
-- constructing a number and converting it to a Symbol
-- 
-- >>> 3.5 :: BasicSym
-- 7/2
--
-- >>> rational 2 10
-- 1 /5
--
-- >>> complex 1 2
-- 1 + 2*I
-- vectors binding

-- | Represents a Vector of BasicSym
-- | usually, end-users are not expected to interact directly with VecBasic
-- | this should at some point be moved to Symengine.Internal
newtype VecBasic = VecBasic (ForeignPtr CVecBasic)

instance Wrapped VecBasic CVecBasic where
    with (VecBasic p) f = withForeignPtr p f


-- | push back an element into a vector
vecbasic_push_back :: VecBasic -> BasicSym -> IO ()
vecbasic_push_back vec sym =  with2 vec sym (\v p ->vecbasic_push_back_ffi v p)


-- | get the i'th element out of a vecbasic
vecbasic_get :: VecBasic -> Int -> Either SymengineException BasicSym
vecbasic_get vec i =
  if i >= 0 && i < vecbasic_size vec
  then
    unsafePerformIO $ do
    sym <- basicsym_new
    exception <- cIntToEnum <$> with2 vec sym (\v s -> vecbasic_get_ffi v i s)
    case exception of
      NoException -> return (Right sym)
      _ -> return (Left exception)
  else
    Left RuntimeError


-- | Create a new VecBasic
vecbasic_new :: IO VecBasic
vecbasic_new = do
    ptr <- vecbasic_new_ffi
    finalized <- newForeignPtr vecbasic_free_ffi ptr
    return $ VecBasic (finalized)


vector_to_vecbasic :: forall n. KnownNat n => V.Vector n BasicSym -> IO VecBasic
vector_to_vecbasic syms = do
  ptr <- vecbasic_new_ffi
  forM_ syms (\sym -> with sym (\s -> vecbasic_push_back_ffi ptr s))
  finalized <- newForeignPtr vecbasic_free_ffi ptr
  return $ VecBasic finalized

vecbasic_size :: VecBasic -> Int
vecbasic_size vec = unsafePerformIO $
  fromIntegral <$> with vec vecbasic_size_ffi

foreign import ccall "symengine/cwrapper.h vecbasic_new" vecbasic_new_ffi :: IO (Ptr CVecBasic)
foreign import ccall "symengine/cwrapper.h vecbasic_push_back" vecbasic_push_back_ffi :: Ptr CVecBasic -> Ptr CBasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h vecbasic_get" vecbasic_get_ffi :: Ptr CVecBasic -> Int -> Ptr CBasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h vecbasic_size" vecbasic_size_ffi :: Ptr CVecBasic -> IO CSize
foreign import ccall "symengine/cwrapper.h &vecbasic_free" vecbasic_free_ffi :: FunPtr (Ptr CVecBasic -> IO ())

