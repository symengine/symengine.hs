{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Symengine.BasicSym(
     ascii_art_str,
     zero,
     one,
     im,
     Symengine.BasicSym.pi,
     e,
     minus_one,
     rational,
     complex,
     symbol_new,
     diff,
     -- HACK: this should be internal :(
     basicsym_new,
     BasicSym,
     lift_basicsym_binaryop,
     lift_basicsym_unaryop
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

newtype BasicSym = BasicSym (ForeignPtr CBasicSym)
instance Wrapped BasicSym CBasicSym where
  with (BasicSym (p)) f = withForeignPtr p f
  
withBasicSym :: BasicSym -> (Ptr CBasicSym -> IO a) -> IO a
withBasicSym (BasicSym ptr) = withForeignPtr ptr

withBasicSym2 :: BasicSym -> BasicSym -> (Ptr CBasicSym -> Ptr CBasicSym -> IO a) -> IO a
withBasicSym2 p1 p2 f = withBasicSym p1 (\p1 -> withBasicSym p2 (\p2 -> f p1 p2))

withBasicSym3 :: BasicSym -> BasicSym -> BasicSym -> (Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO a) -> IO a
withBasicSym3 p1 p2 p3 f = withBasicSym p1 (\p1 -> withBasicSym p2 (\p2 -> withBasicSym p3 (\p3 -> f p1 p2 p3)))

-- | constructor for 0
zero :: BasicSym
zero = basicsym_construct basic_const_zero_ffi

-- | constructor for 1
one :: BasicSym
one = basicsym_construct basic_const_one_ffi

-- | constructor for -1
minus_one :: BasicSym
minus_one = basicsym_construct basic_const_minus_one_ffi

-- | constructor for i = sqrt(-1)
im :: BasicSym
im = basicsym_construct basic_const_I_ffi

-- | the ratio of the circumference of a circle to its radius
pi :: BasicSym
pi = basicsym_construct basic_const_pi_ffi

-- | The base of the natural logarithm
e :: BasicSym
e = basicsym_construct basic_const_E_ffi

expand :: BasicSym -> BasicSym
expand = lift_basicsym_unaryop basic_expand_ffi


eulerGamma :: BasicSym
eulerGamma = basicsym_construct basic_const_EulerGamma_ffi

basicsym_construct :: (Ptr CBasicSym -> IO ()) -> BasicSym
basicsym_construct init_fn = unsafePerformIO $ do
    basic_ptr <- basicsym_new
    with basic_ptr init_fn
    return basic_ptr

basic_str :: BasicSym -> String
basic_str basic_ptr = unsafePerformIO $ with basic_ptr (basic_str_ffi >=> peekCString)

integerToCLong :: Integer -> CLong
integerToCLong i = CLong (fromInteger i)


intToCLong :: Int -> CLong
intToCLong i = toEnum i


intToCInt :: Int -> CInt
intToCInt i = toEnum i

basic_int_signed :: Int -> BasicSym
basic_int_signed i = unsafePerformIO $ do
    iptr <- basicsym_new
    with iptr (\iptr -> integer_set_si_ffi iptr (intToCLong i) )
    return iptr


basic_from_integer :: Integer -> BasicSym
basic_from_integer i = unsafePerformIO $ do
    iptr <- basic_new_heap_ffi
    integer_set_si_ffi iptr (fromInteger i)
    finalized_ptr <- newForeignPtr ptr_basic_free_heap_ffi iptr
    return $ (BasicSym finalized_ptr)

-- |The `ascii_art_str` function prints SymEngine in ASCII art.
-- this is useful as a sanity check
ascii_art_str :: IO String
ascii_art_str = ascii_art_str_ffi >>= peekCString

-- Unexported ffi functions------------------------

-- |Create a basic object that represents all other objects through
-- the FFI
basicsym_new :: IO BasicSym
basicsym_new = do
    basic_ptr <- basic_new_heap_ffi
    finalized_ptr <- newForeignPtr ptr_basic_free_heap_ffi basic_ptr
    return $ BasicSym finalized_ptr

lift_basicsym_binaryop :: (Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO a) -> BasicSym -> BasicSym -> BasicSym
lift_basicsym_binaryop f a b = unsafePerformIO $ do
    s <- basic_new_heap_ffi
    with2 a b (\a b -> f s a b)
    finalized_ptr <- newForeignPtr ptr_basic_free_heap_ffi s
    return (BasicSym finalized_ptr)

lift_basicsym_unaryop :: (Ptr CBasicSym -> Ptr CBasicSym -> IO a) -> BasicSym -> BasicSym
lift_basicsym_unaryop f a = unsafePerformIO $ do
    s <- basic_new_heap_ffi
    with a (\a -> f s a)
    finalized_ptr <- newForeignPtr ptr_basic_free_heap_ffi s
    return (BasicSym finalized_ptr)


basic_pow :: BasicSym -> BasicSym -> BasicSym
basic_pow = lift_basicsym_binaryop basic_pow_ffi

-- |Create a rational number with numerator and denominator
rational :: BasicSym -> BasicSym -> BasicSym
rational = lift_basicsym_binaryop rational_set_ffi

-- |Create a complex number a + b * im
complex :: BasicSym -> BasicSym -> BasicSym
complex a b = (lift_basicsym_binaryop complex_set_ffi) a b

basic_rational_from_integer :: Integer -> Integer -> BasicSym
basic_rational_from_integer i j = unsafePerformIO $ do
    s <- basicsym_new
    with s (\s -> rational_set_si_ffi s (integerToCLong i) (integerToCLong j))
    return s 

-- |Create a symbol with the given name
symbol_new :: String -> BasicSym
symbol_new name = unsafePerformIO $ do
    s <- basicsym_new
    cname <- newCString name
    with s (\s -> symbol_set_ffi s cname)
    free cname
    return s

-- |Differentiate an expression with respect to a symbol
diff :: BasicSym -> BasicSym -> BasicSym
diff expr symbol = (lift_basicsym_binaryop basic_diff_ffi) expr symbol

instance Show BasicSym where
    show = basic_str

instance Eq BasicSym where
    (==) a b = unsafePerformIO $ do
                i <- with2 a b basic_eq_ffi
                return $ i == 1

instance Num BasicSym where
    (+) = lift_basicsym_binaryop basic_add_ffi
    (-) = lift_basicsym_binaryop basic_sub_ffi
    (*) = lift_basicsym_binaryop basic_mul_ffi
    negate = lift_basicsym_unaryop basic_neg_ffi
    abs = lift_basicsym_unaryop basic_abs_ffi
    signum = undefined
    -- works only for long [-2^32, 2^32 - 1]
    fromInteger = basic_from_integer

instance Fractional BasicSym where
    (/) = lift_basicsym_binaryop basic_div_ffi
    fromRational (num :% denom) = basic_rational_from_integer num denom
    recip r = one / r

instance Floating BasicSym where
    pi = Symengine.BasicSym.pi
    exp x = e ** x
    log = undefined
    sqrt x = x  ** 1/2
    (**) = basic_pow
    logBase = undefined
    sin = lift_basicsym_unaryop basic_sin_ffi
    cos = lift_basicsym_unaryop basic_cos_ffi
    tan = lift_basicsym_unaryop basic_tan_ffi
    asin = lift_basicsym_unaryop basic_asin_ffi
    acos = lift_basicsym_unaryop basic_acos_ffi
    atan = lift_basicsym_unaryop basic_atan_ffi
    sinh = lift_basicsym_unaryop basic_sinh_ffi
    cosh = lift_basicsym_unaryop basic_cosh_ffi
    tanh = lift_basicsym_unaryop basic_tanh_ffi
    asinh = lift_basicsym_unaryop basic_asinh_ffi
    acosh = lift_basicsym_unaryop basic_acosh_ffi
    atanh = lift_basicsym_unaryop basic_atanh_ffi

foreign import ccall "symengine/cwrapper.h ascii_art_str" ascii_art_str_ffi :: IO CString
foreign import ccall unsafe "symengine/cwrapper.h basic_new_heap" basic_new_heap_ffi :: IO (Ptr CBasicSym)
foreign import ccall unsafe "symengine/cwrapper.h &basic_free_heap" ptr_basic_free_heap_ffi :: FunPtr(Ptr CBasicSym -> IO ())

-- constants
foreign import ccall "symengine/cwrapper.h basic_const_zero" basic_const_zero_ffi :: Ptr CBasicSym -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_one" basic_const_one_ffi :: Ptr CBasicSym -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_minus_one" basic_const_minus_one_ffi :: Ptr CBasicSym -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_I" basic_const_I_ffi :: Ptr CBasicSym -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_pi" basic_const_pi_ffi :: Ptr CBasicSym -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_E" basic_const_E_ffi :: Ptr CBasicSym -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_EulerGamma" basic_const_EulerGamma_ffi :: Ptr CBasicSym -> IO ()
foreign import ccall "symengine/cwrapper.h basic_str" basic_str_ffi :: Ptr CBasicSym -> IO CString
foreign import ccall "symengine/cwrapper.h basic_eq" basic_eq_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO Int

foreign import ccall "symengine/cwrapper.h symbol_set" symbol_set_ffi :: Ptr CBasicSym -> CString -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_diff" basic_diff_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO CInt

foreign import ccall "symengine/cwrapper.h integer_set_si" integer_set_si_ffi :: Ptr CBasicSym -> CLong -> IO CInt

foreign import ccall "symengine/cwrapper.h rational_set" rational_set_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h rational_set_si" rational_set_si_ffi :: Ptr CBasicSym -> CLong -> CLong -> IO ()

foreign import ccall "symengine/cwrapper.h complex_set" complex_set_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_expand" basic_expand_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt


foreign import ccall "symengine/cwrapper.h basic_add" basic_add_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_sub" basic_sub_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_mul" basic_mul_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_div" basic_div_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_pow" basic_pow_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_neg" basic_neg_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_abs" basic_abs_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_sin" basic_sin_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_cos" basic_cos_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_tan" basic_tan_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_asin" basic_asin_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_acos" basic_acos_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_atan" basic_atan_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_sinh" basic_sinh_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_cosh" basic_cosh_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_tanh" basic_tanh_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_asinh" basic_asinh_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_acosh" basic_acosh_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_atanh" basic_atanh_ffi :: Ptr CBasicSym -> Ptr CBasicSym -> IO CInt
