{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

-- data declarations that are empty
{-# LANGUAGE EmptyDataDecls #-}

module Symengine.Internal
  (
    cIntToEnum,
    cIntFromEnum,
    mkForeignPtr,
    Wrapped(..),
    with2,
    with3,
    with4,
    CBasicSym,
    CVecBasic,
    SymengineException(NoException, RuntimeError, DivByZero, NotImplemented, DomainError, ParseError),
    forceException,
    throwOnSymIntException
  ) where

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
import Control.Exception
import Data.Typeable

data SymengineException = NoException |
                           RuntimeError |
                           DivByZero |
                           NotImplemented |
                           DomainError |
                           ParseError deriving (Show, Enum, Eq, Typeable)

instance Exception SymengineException


-- interpret the CInt as a SymengineException, and
-- throw if it is actually an error
throwOnSymIntException :: CInt -> IO ()
throwOnSymIntException i = forceException . cIntToEnum $ i

forceException :: SymengineException -> IO ()
forceException exception = 
  case exception of
  NoException -> return ()
  error @ _ -> throwIO error

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = fromIntegral . fromEnum

-- |given a raw pointer IO (Ptr a) and a destructor function pointer, make a
-- foreign pointer
mkForeignPtr :: (IO (Ptr a)) -> FunPtr (Ptr a -> IO ()) -> IO (ForeignPtr a)
mkForeignPtr cons des = do
  rawptr <- cons
  finalized <- newForeignPtr des rawptr
  return  finalized

class Wrapped o i | o -> i where
  with :: o -> (Ptr i -> IO a) -> IO a

with2 :: Wrapped o1 i1 => Wrapped o2 i2 => o1 -> o2 -> (Ptr i1 -> Ptr i2 -> IO a) -> IO a
with2 o1 o2 f = with o1 (\p1 -> with o2 (\p2 -> f p1 p2))

with3 :: Wrapped o1 i1 => Wrapped o2 i2 => Wrapped o3 i3 => o1 -> o2 -> o3 -> (Ptr i1 -> Ptr i2 -> Ptr i3 -> IO a) -> IO a
with3 o1 o2 o3 f = with2 o1 o2 (\p1 p2 -> with o3 (\p3 -> f p1 p2 p3))


with4:: Wrapped o1 i1 => Wrapped o2 i2 => Wrapped o3 i3 => Wrapped o4 i4 => o1 -> o2 -> o3 -> o4 -> (Ptr i1 -> Ptr i2 -> Ptr i3 -> Ptr i4 -> IO a) -> IO a
with4 o1 o2 o3 o4 f = with o1 (\p1 -> with3 o2 o3 o4 (\p2 p3 p4 -> f p1 p2 p3 p4))

-- BasicSym
data CBasicSym

-- VecBasic
data CVecBasic

-- CDenseMatrix
