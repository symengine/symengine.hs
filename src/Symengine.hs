{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Symengine
Description : Symengine bindings to Haskell
-}
module Symengine
    (
     ascii_art_str,
     basic_str,
     basic_const_zero,
     basic_const_one,
     basic_const_I,
     basic_const_pi,
     basic_const_EulerGamma,
     basic_const_minus_one,
     basic_int_signed,
     BasicSym,
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Control.Applicative
import System.IO.Unsafe
import Control.Monad
import GHC.Real

data BasicStruct = BasicStruct {
    data_ptr :: Ptr ()
}

instance Storable BasicStruct where
    alignment _ = 8
    sizeOf _ = sizeOf nullPtr
    peek basic_ptr = BasicStruct <$> peekByteOff basic_ptr 0
    poke basic_ptr BasicStruct{..} = pokeByteOff basic_ptr 0 data_ptr

data BasicSym = BasicSym { fptr :: ForeignPtr BasicStruct }

withBasicSym :: BasicSym -> (Ptr BasicStruct -> IO a) -> IO a
withBasicSym p f = withForeignPtr (fptr p ) f

withBasicSym2 :: BasicSym -> BasicSym -> (Ptr BasicStruct -> Ptr BasicStruct -> IO a) -> IO a
withBasicSym2 p1 p2 f = withBasicSym p1 (\p1 -> withBasicSym p2 (\p2 -> f p1 p2))

withBasicSym3 :: BasicSym -> BasicSym -> BasicSym -> (Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO a) -> IO a
withBasicSym3 p1 p2 p3 f = withBasicSym p1 (\p1 -> withBasicSym p2 (\p2 -> withBasicSym p3 (\p3 -> f p1 p2 p3)))

instance Show BasicSym where
    show = basic_str 


basic_const_zero :: BasicSym
basic_const_zero = basic_obj_constructor basic_const_zero_ffi


basic_const_one :: BasicSym
basic_const_one = basic_obj_constructor basic_const_one_ffi

basic_const_minus_one :: BasicSym
basic_const_minus_one = basic_obj_constructor basic_const_minus_one_ffi

basic_const_I :: BasicSym
basic_const_I = basic_obj_constructor basic_const_I_ffi

basic_const_pi :: BasicSym
basic_const_pi = basic_obj_constructor basic_const_pi_ffi

basic_const_E :: BasicSym
basic_const_E = basic_obj_constructor basic_const_E_ffi

basic_const_EulerGamma :: BasicSym
basic_const_EulerGamma = basic_obj_constructor basic_const_EulerGamma_ffi

basic_obj_constructor :: (Ptr BasicStruct -> IO ()) -> BasicSym
basic_obj_constructor init_fn = unsafePerformIO $ do
    basic_ptr <- create_basic_ptr
    withBasicSym basic_ptr init_fn
    return basic_ptr

basic_str :: BasicSym -> String
basic_str basic_ptr = unsafePerformIO $ withBasicSym basic_ptr (basic_str_ffi >=> peekCString)

integerToCLong :: Integer -> CLong
integerToCLong i = CLong (fromInteger i)


intToCLong :: Int -> CLong
intToCLong i = integerToCLong (toInteger i)

basic_int_signed :: Int -> BasicSym
basic_int_signed i = unsafePerformIO $ do
    iptr <- create_basic_ptr
    withBasicSym iptr (\iptr -> integer_set_si_ffi iptr (intToCLong i) )
    return iptr


basic_from_integer :: Integer -> BasicSym
basic_from_integer i = unsafePerformIO $ do
    iptr <- create_basic_ptr
    withBasicSym iptr (\iptr -> integer_set_si_ffi iptr (fromInteger i))
    return iptr

-- |The `ascii_art_str` function prints SymEngine in ASCII art.
-- this is useful as a sanity check
ascii_art_str :: IO String
ascii_art_str = ascii_art_str_ffi >>= peekCString

-- Unexported ffi functions------------------------

-- |Create a basic object that represents all other objects through
-- the FFI
create_basic_ptr :: IO BasicSym
create_basic_ptr = do
    basic_ptr <- newArray [BasicStruct { data_ptr = nullPtr }]
    basic_new_heap_ffi basic_ptr
    finalized_ptr <- newForeignPtr ptr_basic_free_heap_ffi basic_ptr
    return $ BasicSym { fptr = finalized_ptr }

basic_binaryop :: (Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()) -> BasicSym -> BasicSym -> BasicSym
basic_binaryop f a b = unsafePerformIO $ do
    s <- create_basic_ptr
    withBasicSym3 s a b f
    return s 

basic_unaryop :: (Ptr BasicStruct -> Ptr BasicStruct -> IO ()) -> BasicSym -> BasicSym
basic_unaryop f a = unsafePerformIO $ do
    s <- create_basic_ptr
    withBasicSym2 s a f
    return s 


basic_pow :: BasicSym -> BasicSym -> BasicSym
basic_pow = basic_binaryop basic_pow_ffi

basic_neg :: BasicSym -> BasicSym
basic_neg = basic_unaryop basic_neg_ffi

basic_abs :: BasicSym -> BasicSym
basic_abs = basic_unaryop basic_abs_ffi

basic_rational_set :: BasicSym -> BasicSym -> BasicSym
basic_rational_set = basic_binaryop rational_set_ffi

basic_rational_set_signed :: Integer -> Integer -> BasicSym
basic_rational_set_signed i j = unsafePerformIO $ do
    s <- create_basic_ptr
    withBasicSym s (\s -> rational_set_si_ffi s (integerToCLong i) (integerToCLong j))
    return s 



instance Num BasicSym where
    (+) = basic_binaryop basic_add_ffi
    (-) = basic_binaryop basic_sub_ffi
    (*) = basic_binaryop basic_mul_ffi
    negate = basic_unaryop basic_neg_ffi
    abs = basic_unaryop basic_abs_ffi
    signum = undefined
    fromInteger = basic_from_integer

instance Fractional BasicSym where
    (/) = basic_binaryop basic_div_ffi
    fromRational (num :% denom) = basic_rational_set_signed num denom
    recip r = basic_const_one / r

instance Floating BasicSym where
    pi = basic_const_pi
    --exp :: basic_binaryop basic_pow_ffi
    log = undefined
    sqrt x =  basic_pow x 0.5
    (**) = basic_pow
    logBase = undefined
    sin = basic_unaryop basic_sin_ffi
    cos = basic_unaryop basic_cos_ffi
    tan = basic_unaryop basic_tan_ffi
    asin = basic_unaryop basic_asin_ffi
    acos = basic_unaryop basic_acos_ffi
    atan = basic_unaryop basic_atan_ffi
    sinh = basic_unaryop basic_sinh_ffi
    cosh = basic_unaryop basic_cosh_ffi
    tanh = basic_unaryop basic_tanh_ffi
    asinh = basic_unaryop basic_asinh_ffi
    acosh = basic_unaryop basic_acosh_ffi
    atanh = basic_unaryop basic_atanh_ffi

foreign import ccall "symengine/cwrapper.h ascii_art_str" ascii_art_str_ffi :: IO CString
foreign import ccall "symengine/cwrapper.h basic_new_heap" basic_new_heap_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h &basic_free_heap" ptr_basic_free_heap_ffi :: FunPtr(Ptr BasicStruct -> IO ())

-- constants
foreign import ccall "symengine/cwrapper.h basic_const_zero" basic_const_zero_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_one" basic_const_one_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_minus_one" basic_const_minus_one_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_I" basic_const_I_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_pi" basic_const_pi_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_E" basic_const_E_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_EulerGamma" basic_const_EulerGamma_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_str" basic_str_ffi :: Ptr BasicStruct -> IO CString

foreign import ccall "symengine/cwrapper.h integer_set_si" integer_set_si_ffi :: Ptr BasicStruct -> CLong -> IO ()

foreign import ccall "symengine/cwrapper.h rational_set" rational_set_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h rational_set_si" rational_set_si_ffi :: Ptr BasicStruct -> CLong -> CLong -> IO ()

foreign import ccall "symengine/cwrapper.h basic_add" basic_add_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_sub" basic_sub_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_mul" basic_mul_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_div" basic_div_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_pow" basic_pow_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_neg" basic_neg_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_abs" basic_abs_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()

foreign import ccall "symengine/cwrapper.h basic_sin" basic_sin_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_cos" basic_cos_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_tan" basic_tan_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()

foreign import ccall "symengine/cwrapper.h basic_asin" basic_asin_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_acos" basic_acos_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_atan" basic_atan_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()

foreign import ccall "symengine/cwrapper.h basic_sinh" basic_sinh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_cosh" basic_cosh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_tanh" basic_tanh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()

foreign import ccall "symengine/cwrapper.h basic_asinh" basic_asinh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_acosh" basic_acosh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_atanh" basic_atanh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
