{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

{-|
Module      : Symengine
Description : Symengine bindings to Haskell
-}
module Symengine
    (
     ascii_art_str,
     zero,
     one,
     im,
     Symengine.pi,
     e,
     minus_one,
     rational,
     complex,
     symbol,
     BasicSym,
     VecBasic,
     vecbasic_new,
     vecbasic_push_back,
     vecbasic_get,
     vecbasic_size,
     -- Dense matrices
     DenseMatrix,
     densematrix_new,
     densematrix_new_vec,
     SymengineException(NoException, RuntimeError, DivByZero, NotImplemented, DomainError, ParseError)
    ) where

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

data SymengineException = NoException |
                           RuntimeError |
                           DivByZero |
                           NotImplemented |
                           DomainError |
                           ParseError deriving (Show, Enum, Eq)


cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = fromIntegral . fromEnum

data BasicStruct = BasicStruct {
    data_ptr :: Ptr ()
}

instance Storable BasicStruct where
    alignment _ = 8
    sizeOf _ = sizeOf nullPtr
    peek basic_ptr = BasicStruct <$> peekByteOff basic_ptr 0
    poke basic_ptr BasicStruct{..} = pokeByteOff basic_ptr 0 data_ptr



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
data BasicSym = BasicSym { fptr :: ForeignPtr BasicStruct }

withBasicSym :: BasicSym -> (Ptr BasicStruct -> IO a) -> IO a
withBasicSym p f = withForeignPtr (fptr p ) f

withBasicSym2 :: BasicSym -> BasicSym -> (Ptr BasicStruct -> Ptr BasicStruct -> IO a) -> IO a
withBasicSym2 p1 p2 f = withBasicSym p1 (\p1 -> withBasicSym p2 (\p2 -> f p1 p2))

withBasicSym3 :: BasicSym -> BasicSym -> BasicSym -> (Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO a) -> IO a
withBasicSym3 p1 p2 p3 f = withBasicSym p1 (\p1 -> withBasicSym p2 (\p2 -> withBasicSym p3 (\p3 -> f p1 p2 p3)))


-- | constructor for 0
zero :: BasicSym
zero = basic_obj_constructor basic_const_zero_ffi

-- | constructor for 1
one :: BasicSym
one = basic_obj_constructor basic_const_one_ffi

-- | constructor for -1
minus_one :: BasicSym
minus_one = basic_obj_constructor basic_const_minus_one_ffi

-- | constructor for i = sqrt(-1)
im :: BasicSym
im = basic_obj_constructor basic_const_I_ffi

-- | the ratio of the circumference of a circle to its radius
pi :: BasicSym
pi = basic_obj_constructor basic_const_pi_ffi

-- | The base of the natural logarithm
e :: BasicSym
e = basic_obj_constructor basic_const_E_ffi

expand :: BasicSym -> BasicSym
expand = basic_unaryop basic_expand_ffi


eulerGamma :: BasicSym
eulerGamma = basic_obj_constructor basic_const_EulerGamma_ffi

basic_obj_constructor :: (Ptr BasicStruct -> IO ()) -> BasicSym
basic_obj_constructor init_fn = unsafePerformIO $ do
    basic_ptr <- create_basicsym
    withBasicSym basic_ptr init_fn
    return basic_ptr

basic_str :: BasicSym -> String
basic_str basic_ptr = unsafePerformIO $ withBasicSym basic_ptr (basic_str_ffi >=> peekCString)

integerToCLong :: Integer -> CLong
integerToCLong i = CLong (fromInteger i)


intToCLong :: Int -> CLong
intToCLong i = toEnum i


intToCInt :: Int -> CInt
intToCInt i = toEnum i

basic_int_signed :: Int -> BasicSym
basic_int_signed i = unsafePerformIO $ do
    iptr <- create_basicsym
    withBasicSym iptr (\iptr -> integer_set_si_ffi iptr (intToCLong i) )
    return iptr


basic_from_integer :: Integer -> BasicSym
basic_from_integer i = unsafePerformIO $ do
    iptr <- create_basicsym
    withBasicSym iptr (\iptr -> integer_set_si_ffi iptr (fromInteger i))
    return iptr

-- |The `ascii_art_str` function prints SymEngine in ASCII art.
-- this is useful as a sanity check
ascii_art_str :: IO String
ascii_art_str = ascii_art_str_ffi >>= peekCString

-- Unexported ffi functions------------------------

-- |Create a basic object that represents all other objects through
-- the FFI
create_basicsym :: IO BasicSym
create_basicsym = do
    basic_ptr <- newArray [BasicStruct { data_ptr = nullPtr }]
    basic_new_heap_ffi basic_ptr
    finalized_ptr <- newForeignPtr ptr_basic_free_heap_ffi basic_ptr
    return $ BasicSym { fptr = finalized_ptr }

basic_binaryop :: (Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO a) -> BasicSym -> BasicSym -> BasicSym
basic_binaryop f a b = unsafePerformIO $ do
    s <- create_basicsym
    withBasicSym3 s a b f
    return s 

basic_unaryop :: (Ptr BasicStruct -> Ptr BasicStruct -> IO a) -> BasicSym -> BasicSym
basic_unaryop f a = unsafePerformIO $ do
    s <- create_basicsym
    withBasicSym2 s a f
    return s 


basic_pow :: BasicSym -> BasicSym -> BasicSym
basic_pow = basic_binaryop basic_pow_ffi

-- |Create a rational number with numerator and denominator
rational :: BasicSym -> BasicSym -> BasicSym
rational = basic_binaryop rational_set_ffi

-- |Create a complex number a + b * im
complex :: BasicSym -> BasicSym -> BasicSym
complex a b = (basic_binaryop complex_set_ffi) a b

basic_rational_from_integer :: Integer -> Integer -> BasicSym
basic_rational_from_integer i j = unsafePerformIO $ do
    s <- create_basicsym
    withBasicSym s (\s -> rational_set_si_ffi s (integerToCLong i) (integerToCLong j))
    return s 

-- |Create a symbol with the given name
symbol :: String -> BasicSym
symbol name = unsafePerformIO $ do
    s <- create_basicsym
    cname <- newCString name
    withBasicSym s (\s -> symbol_set_ffi s cname)
    free cname
    return s

-- |Differentiate an expression with respect to a symbol
diff :: BasicSym -> BasicSym -> BasicSym
diff expr symbol = (basic_binaryop basic_diff_ffi) expr symbol

instance Show BasicSym where
    show = basic_str 

instance Eq BasicSym where
    (==) a b = unsafePerformIO $ do 
                i <- withBasicSym2 a b basic_eq_ffi
                return $ i == 1

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
    fromRational (num :% denom) = basic_rational_from_integer num denom
    recip r = one / r

instance Floating BasicSym where
    pi = Symengine.pi
    exp x = e ** x
    log = undefined
    sqrt x = x  ** 1/2
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
foreign import ccall "symengine/cwrapper.h basic_eq" basic_eq_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO Int

foreign import ccall "symengine/cwrapper.h symbol_set" symbol_set_ffi :: Ptr BasicStruct -> CString -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_diff" basic_diff_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO CInt

foreign import ccall "symengine/cwrapper.h integer_set_si" integer_set_si_ffi :: Ptr BasicStruct -> CLong -> IO CInt

foreign import ccall "symengine/cwrapper.h rational_set" rational_set_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h rational_set_si" rational_set_si_ffi :: Ptr BasicStruct -> CLong -> CLong -> IO ()

foreign import ccall "symengine/cwrapper.h complex_set" complex_set_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_expand" basic_expand_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt


foreign import ccall "symengine/cwrapper.h basic_add" basic_add_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_sub" basic_sub_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_mul" basic_mul_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_div" basic_div_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_pow" basic_pow_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_neg" basic_neg_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_abs" basic_abs_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_sin" basic_sin_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_cos" basic_cos_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_tan" basic_tan_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_asin" basic_asin_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_acos" basic_acos_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_atan" basic_atan_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_sinh" basic_sinh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_cosh" basic_cosh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_tanh" basic_tanh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_asinh" basic_asinh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_acosh" basic_acosh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_atanh" basic_atanh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO CInt

-- vectors binding
data CVecBasic = CVecBasic

-- | Represents a Vector of BasicSym
-- | usually, end-users are not expected to interact directly with VecBasic
-- | this should at some point be moved to Symengine.Internal
data VecBasic = VecBasic { vecfptr :: ForeignPtr CVecBasic }

withVecBasic :: VecBasic -> (Ptr CVecBasic -> IO a) -> IO a
withVecBasic v f = withForeignPtr (vecfptr v) f

-- | push back an element into a vector
vecbasic_push_back :: VecBasic -> BasicSym -> IO ()
vecbasic_push_back vec sym =  withVecBasic vec (\v -> withBasicSym sym (\p ->vecbasic_push_back_ffi v p))


-- | get the i'th element out of a vecbasic
vecbasic_get :: VecBasic -> Int -> Either SymengineException BasicSym
vecbasic_get vec i =
  if i >= 0 && i < vecbasic_size vec
  then 
    unsafePerformIO $ do
    basicsym <- create_basicsym
    exception <- cIntToEnum <$> withVecBasic vec (\v -> withBasicSym basicsym (\bs -> vecbasic_get_ffi v i bs))
    --exception <- toEnum <$> withBasicSym basicsym (\bs -> vecbasic_get_ffi vec i bs)
    case exception of
      NoException -> return (Right basicsym)
      _ -> return (Left exception)
  else
    Left RuntimeError


-- | Create a new VecBasic
vecbasic_new :: IO VecBasic
vecbasic_new = do
    ptr <- vecbasic_new_ffi
    finalized <- newForeignPtr vecbasic_free_ffi ptr
    return $ VecBasic (finalized)


list_to_vecbasic :: [BasicSym] -> IO VecBasic
list_to_vecbasic syms = do
  vec <- vecbasic_new 
  forM_ syms (\s -> vecbasic_push_back vec s)
  return vec

vecbasic_size :: VecBasic -> Int
vecbasic_size vec = unsafePerformIO $ 
  fromIntegral <$> withVecBasic vec vecbasic_size_ffi

foreign import ccall "symengine/cwrapper.h vecbasic_new" vecbasic_new_ffi :: IO (Ptr CVecBasic)
foreign import ccall "symengine/cwrapper.h vecbasic_push_back" vecbasic_push_back_ffi :: Ptr CVecBasic -> Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h vecbasic_get" vecbasic_get_ffi :: Ptr CVecBasic -> Int -> Ptr BasicStruct -> IO CInt
foreign import ccall "symengine/cwrapper.h vecbasic_size" vecbasic_size_ffi :: Ptr CVecBasic -> IO CSize
foreign import ccall "symengine/cwrapper.h &vecbasic_free" vecbasic_free_ffi :: FunPtr (Ptr CVecBasic -> IO ())



-- Dense Matrices
data Wrapped a = Wrapped {
  ptr :: ForeignPtr a
}

mkWrapped :: (IO (Ptr a)) -> FunPtr (Ptr a -> IO ()) -> IO (Wrapped a)
mkWrapped cons des = do
  rawptr <- cons
  finalized <- newForeignPtr des rawptr
  return $ Wrapped finalized

withWrapped :: Wrapped a -> (Ptr a -> IO b) -> IO b
withWrapped w f = withForeignPtr (ptr w) f

data CDenseMatrix = CDenseMatrix
newtype  DenseMatrix = DenseMatrix (Wrapped CDenseMatrix)

instance Show (DenseMatrix) where
  show :: DenseMatrix -> String
  show (DenseMatrix mat) = 
    unsafePerformIO $ withWrapped mat  (cdensematrix_str_ffi >=> peekCString)


densematrix_new :: IO DenseMatrix
densematrix_new = DenseMatrix <$> (mkWrapped cdensematrix_new_ffi cdensematrix_free_ffi)

type NRows = Int
type NCols = Int


densematrix_new_rows_cols :: NRows -> NCols -> IO DenseMatrix
densematrix_new_rows_cols r c =  DenseMatrix <$> 
  mkWrapped (cdensematrix_new_rows_cols_ffi (fromIntegral r) (fromIntegral c)) cdensematrix_free_ffi
  

densematrix_new_vec :: NRows -> NCols -> [BasicSym] -> IO DenseMatrix
densematrix_new_vec r c syms = do
  vec <- list_to_vecbasic syms
  let cdensemat =  withVecBasic vec (\v ->  cdensematrix_new_vec_ffi (fromIntegral r) (fromIntegral c) v)
  DenseMatrix <$>  mkWrapped cdensemat cdensematrix_free_ffi

foreign import ccall "symengine/cwrapper.h dense_matrix_new" cdensematrix_new_ffi :: IO (Ptr CDenseMatrix)
foreign import ccall "symengine/cwrapper.h &dense_matrix_free" cdensematrix_free_ffi :: FunPtr ((Ptr CDenseMatrix) -> IO ())
foreign import ccall "symengine/cwrapper.h dense_matrix_str" cdensematrix_str_ffi :: Ptr CDenseMatrix -> IO CString
foreign import ccall "symengine/cwrapper.h dense_matrix_new_rows_cols" cdensematrix_new_rows_cols_ffi :: CUInt -> CUInt -> IO (Ptr CDenseMatrix)
foreign import ccall "symengine/cwrapper.h dense_matrix_new_vec" cdensematrix_new_vec_ffi :: CUInt -> CUInt -> Ptr CVecBasic -> IO (Ptr CDenseMatrix)

