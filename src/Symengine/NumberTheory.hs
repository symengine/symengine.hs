
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Symengine.NumberTheory(
  Symengine.NumberTheory.gcd,
  Symengine.NumberTheory.lcm,
  gcd_extended,
  next_prime,
  Symengine.NumberTheory.mod,
  quotient,
{-
  quotient_and_mod,
  mod_f,
  quotient_f,
  mod_inverse,
  fibonacci,
  fibonacci2,
  lucas,
  lucas2,
  binomial,
  factorial
  -}
)
where

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


gcd :: BasicSym -> BasicSym -> BasicSym
gcd = lift_basicsym_binaryop ntheory_gcd_ffi

lcm :: BasicSym -> BasicSym -> BasicSym
lcm = lift_basicsym_binaryop ntheory_lcm_ffi

gcd_extended :: BasicSym -> BasicSym -> (BasicSym, BasicSym, BasicSym)
gcd_extended a b = unsafePerformIO $ do
  g <- basicsym_new
  s <- basicsym_new
  t <- basicsym_new

  with4 g s t a (\g s t a ->
                    with b (\b ->
                              ntheory_gcd_ext_ffi g s t a b))
  return (g, s, t)
  
next_prime :: BasicSym -> BasicSym
next_prime = lift_basicsym_unaryop ntheory_nextprime_ffi

mod :: BasicSym -> BasicSym -> BasicSym
mod = lift_basicsym_binaryop ntheory_mod_ffi


quotient :: BasicSym -> BasicSym -> BasicSym
quotient = lift_basicsym_binaryop ntheory_quotient_ffi

foreign import ccall "symengine/cwrapper.h ntheory_gcd" ntheory_gcd_ffi :: 
  Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()

foreign import ccall "symengine/cwrapper.h ntheory_lcm" ntheory_lcm_ffi :: 
  Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()

foreign import ccall "symengine/cwrapper.h ntheory_gcd_ext" ntheory_gcd_ext_ffi 
  :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> 
      Ptr CBasicSym -> Ptr CBasicSym -> IO ()

foreign import ccall "symengine/cwrapper.h ntheory_nextprime"
  ntheory_nextprime_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO ()

foreign import ccall "symengine/cwrapper.h ntheory_mod"
  ntheory_mod_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()


foreign import ccall "symengine/cwrapper.h ntheory_quotient"
  ntheory_quotient_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()
