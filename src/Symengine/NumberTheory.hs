
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
  quotient_and_mod,
  mod_f,
  quotient_f,
  quotient_and_mod_f,
  mod_inverse,
  fibonacci,
  fibonacci2,
  lucas,
  -- I do not understand exactly what lucas2 does. Clarify and then
  -- export 
  -- lucas2,
  binomial,
  factorial
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

type Quotient = BasicSym
type Modulo = BasicSym

mod :: BasicSym -> BasicSym -> Quotient
mod = lift_basicsym_binaryop ntheory_mod_ffi

quotient :: BasicSym -> BasicSym -> BasicSym
quotient = lift_basicsym_binaryop ntheory_quotient_ffi

quotient_and_mod :: BasicSym -> BasicSym -> (Quotient, Modulo)
quotient_and_mod a b = unsafePerformIO $ do
  quotient <- basicsym_new
  modulo <- basicsym_new
  with4 quotient modulo a b ntheory_quotient_mod_ffi
  return $  (quotient, modulo)


mod_f :: BasicSym -> BasicSym -> Quotient
mod_f = lift_basicsym_binaryop ntheory_mod_f_ffi

quotient_f :: BasicSym -> BasicSym -> BasicSym
quotient_f = lift_basicsym_binaryop ntheory_quotient_f_ffi

quotient_and_mod_f :: BasicSym -> BasicSym -> (Quotient, Modulo)
quotient_and_mod_f a b = unsafePerformIO $ do
  quotient <- basicsym_new
  modulo <- basicsym_new
  with4 quotient modulo a b ntheory_quotient_mod_f_ffi
  return $  (quotient, modulo)


mod_inverse :: BasicSym -> BasicSym -> Quotient
mod_inverse = lift_basicsym_binaryop ntheory_mod_inverse_ffi


fibonacci ::  Int -> BasicSym
fibonacci i = unsafePerformIO $  do
  fib <- basicsym_new
  with fib (\fib -> ntheory_fibonacci_ffi fib (fromIntegral i))
  return fib

fibonacci2 :: Int -> (BasicSym, BasicSym)
fibonacci2 n  = unsafePerformIO $ do
  g <- basicsym_new
  s <- basicsym_new
  
  with2 g s (\g s -> ntheory_fibonacci2_ffi g s (fromIntegral n))
  
  return (g, s) 


lucas :: Int -> BasicSym
lucas n = unsafePerformIO $ do
  l <- basicsym_new
  with l (\l -> ntheory_lucas_ffi l (fromIntegral n))
  return l

{-
lucas2 :: BasicSym -> BasicSym -> (BasicSym, BasicSym)
lucas2 n n_prev = unsafePerformIO $ do
  g <- basicsym_new
  s <- basicsym_new

  with4 g s n n_prev ntheory_lucas2_ffi
  
  return (g, s) 
-}
binomial :: BasicSym -> Int -> BasicSym
binomial n r = unsafePerformIO $ do
  ncr <- basicsym_new
  with2 ncr n (\ncr n -> ntheory_binomial_ffi ncr n (fromIntegral r))
  return ncr


factorial :: Int -> BasicSym
factorial n = unsafePerformIO $ do
  fact <- basicsym_new
  with fact (\fact -> ntheory_factorial_ffi fact (fromIntegral n))
  return fact
-- FFI Bindings
-- gcd, lcm

foreign import ccall "symengine/cwrapper.h ntheory_gcd" ntheory_gcd_ffi :: 
  Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()

foreign import ccall "symengine/cwrapper.h ntheory_lcm" ntheory_lcm_ffi :: 
  Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()

foreign import ccall "symengine/cwrapper.h ntheory_gcd_ext" ntheory_gcd_ext_ffi 
  :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> 
      Ptr CBasicSym -> Ptr CBasicSym -> IO ()

-- prime

foreign import ccall "symengine/cwrapper.h ntheory_nextprime"
  ntheory_nextprime_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO ()

-- modulus

foreign import ccall "symengine/cwrapper.h ntheory_mod"
  ntheory_mod_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()


foreign import ccall "symengine/cwrapper.h ntheory_quotient"
  ntheory_quotient_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()

foreign import ccall "symengine/cwrapper.h ntheory_quotient_mod"
  ntheory_quotient_mod_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> 
                          Ptr CBasicSym -> Ptr CBasicSym -> IO ()


-- _f versions (round towards -inf)
foreign import ccall "symengine/cwrapper.h ntheory_mod_f"
  ntheory_mod_f_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()


foreign import ccall "symengine/cwrapper.h ntheory_quotient_f"
  ntheory_quotient_f_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()

foreign import ccall "symengine/cwrapper.h ntheory_quotient_mod_f"
  ntheory_quotient_mod_f_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> 
                          Ptr CBasicSym -> Ptr CBasicSym -> IO ()

-- mod inverse
foreign import ccall "symengine/cwrapper.h ntheory_mod_inverse"
  ntheory_mod_inverse_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO ()


-- fibonacci
foreign import ccall "symengine/cwrapper.h ntheory_fibonacci"
  ntheory_fibonacci_ffi :: Ptr CBasicSym -> 
    CULong -> IO ()


foreign import ccall "symengine/cwrapper.h ntheory_fibonacci2"
  ntheory_fibonacci2_ffi :: Ptr CBasicSym -> Ptr CBasicSym ->  
    CULong -> IO ()

-- lucas
foreign import ccall "symengine/cwrapper.h ntheory_lucas"
  ntheory_lucas_ffi :: Ptr CBasicSym ->
    CULong -> IO ()


foreign import ccall "symengine/cwrapper.h ntheory_lucas2"
  ntheory_lucas2_ffi :: Ptr CBasicSym -> Ptr CBasicSym ->  
     CULong -> IO ()


-- binomial
foreign import ccall "symengine/cwrapper.h ntheory_binomial"
  ntheory_binomial_ffi :: Ptr CBasicSym -> Ptr CBasicSym ->  
    CULong -> IO ()

-- factorial
foreign import ccall "symengine/cwrapper.h ntheory_factorial"
  ntheory_factorial_ffi :: Ptr CBasicSym -> 
    CULong -> IO ()
