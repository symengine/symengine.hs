{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

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
     symbol_new,
     diff,
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
     densematrix_lu_solve,
     --exception
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

data CBasicSym = CBasicSym

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
newtype BasicSym = BasicSym (ForeignPtr CBasicSym)
instance Wrapped BasicSym CBasicSym where
  with (BasicSym (p)) f = withForeignPtr p f
  
 {-
withBasicSym :: BasicSym -> (Ptr CBasicSym -> IO a) -> IO a
withBasicSym (BasicSym ptr) = withForeignPtr ptr

withBasicSym2 :: BasicSym -> BasicSym -> (Ptr CBasicSym -> Ptr CBasicSym -> IO a) -> IO a
withBasicSym2 p1 p2 f = withBasicSym p1 (\p1 -> withBasicSym p2 (\p2 -> f p1 p2))

withBasicSym3 :: BasicSym -> BasicSym -> BasicSym -> (Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO a) -> IO a
withBasicSym3 p1 p2 p3 f = withBasicSym p1 (\p1 -> withBasicSym p2 (\p2 -> withBasicSym p3 (\p3 -> f p1 p2 p3)))
-}

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

basic_obj_constructor :: (Ptr CBasicSym -> IO ()) -> BasicSym
basic_obj_constructor init_fn = unsafePerformIO $ do
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
    iptr <- basicsym_new
    with iptr (\iptr -> integer_set_si_ffi iptr (fromInteger i))
    return iptr

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

basic_binaryop :: (Ptr CBasicSym -> Ptr CBasicSym -> Ptr CBasicSym -> IO a) -> BasicSym -> BasicSym -> BasicSym
basic_binaryop f a b = unsafePerformIO $ do
    s <- basicsym_new
    with3 s a b f
    return s

basic_unaryop :: (Ptr CBasicSym -> Ptr CBasicSym -> IO a) -> BasicSym -> BasicSym
basic_unaryop f a = unsafePerformIO $ do
    s <- basicsym_new
    with2 s a f
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
    s <- basicsym_new
    with s (\s -> rational_set_si_ffi s (integerToCLong i) (integerToCLong j))
    return s 

-- |Create a symbol with the given name
symbol_new :: String -> IO BasicSym
symbol_new name = do
    s <- basicsym_new
    cname <- newCString name
    with s (\s -> symbol_set_ffi s cname)
    free cname
    return s

-- |Differentiate an expression with respect to a symbol
diff :: BasicSym -> BasicSym -> BasicSym
diff expr symbol = (basic_binaryop basic_diff_ffi) expr symbol

instance Show BasicSym where
    show = basic_str

instance Eq BasicSym where
    (==) a b = unsafePerformIO $ do
                i <- with2 a b basic_eq_ffi
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
foreign import ccall "symengine/cwrapper.h basic_new_heap" basic_new_heap_ffi :: IO (Ptr CBasicSym)
foreign import ccall "symengine/cwrapper.h &basic_free_heap" ptr_basic_free_heap_ffi :: FunPtr(Ptr CBasicSym -> IO ())

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

-- vectors binding
data CVecBasic = CVecBasic

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


list_to_vecbasic :: [BasicSym] -> IO VecBasic
list_to_vecbasic syms = do
  vec <- vecbasic_new
  forM_ syms (\s -> vecbasic_push_back vec s)
  return vec

vecbasic_size :: VecBasic -> Int
vecbasic_size vec = unsafePerformIO $
  fromIntegral <$> with vec vecbasic_size_ffi

foreign import ccall "symengine/cwrapper.h vecbasic_new" vecbasic_new_ffi :: IO (Ptr CVecBasic)
foreign import ccall "symengine/cwrapper.h vecbasic_push_back" vecbasic_push_back_ffi :: Ptr CVecBasic -> Ptr CBasicSym -> IO ()
foreign import ccall "symengine/cwrapper.h vecbasic_get" vecbasic_get_ffi :: Ptr CVecBasic -> Int -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h vecbasic_size" vecbasic_size_ffi :: Ptr CVecBasic -> IO CSize
foreign import ccall "symengine/cwrapper.h &vecbasic_free" vecbasic_free_ffi :: FunPtr (Ptr CVecBasic -> IO ())



-- Dense Matrices



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

densematrix_new_rows_cols :: NRows -> NCols -> IO DenseMatrix
densematrix_new_rows_cols r c =  DenseMatrix <$>
  mkForeignPtr (cdensematrix_new_rows_cols_ffi (fromIntegral r) (fromIntegral c)) cdensematrix_free_ffi

densematrix_new_vec :: NRows -> NCols -> [BasicSym] -> IO DenseMatrix
densematrix_new_vec r c syms = do
  vec <- list_to_vecbasic syms
  let cdensemat =  with vec (\v ->  cdensematrix_new_vec_ffi (fromIntegral r) (fromIntegral c) v)
  DenseMatrix <$>  mkForeignPtr cdensemat cdensematrix_free_ffi


type Offset = Int
-- create a matrix with 1's on the diagonal offset by offset
densematrix_new_eye :: NRows -> NCols -> Offset -> IO DenseMatrix
densematrix_new_eye r c offset = do
  mat <- densematrix_new_rows_cols r c
  with mat (\m -> cdensematrix_eye_ffi m
                 (fromIntegral r)
                 (fromIntegral c)
                 (fromIntegral offset))
  return mat

-- create a matrix with diagonal elements at offest k
densematrix_new_diag :: [BasicSym] -> Int -> IO DenseMatrix
densematrix_new_diag syms offset = do
  let dim = length syms
  vecsyms <- list_to_vecbasic syms
  mat <- densematrix_new_rows_cols dim dim
  with2 mat vecsyms (\m vs -> cdensematrix_diag_ffi m vs (fromIntegral offset))

  return mat

type Row = Int
type Col = Int

densematrix_get :: DenseMatrix -> Row -> Col -> BasicSym
densematrix_get mat r c = unsafePerformIO $ do
      sym <- basicsym_new
      with2 mat sym (\m s -> cdensematrix_get_basic_ffi s m (fromIntegral r) (fromIntegral c))
      return sym

densematrix_set :: DenseMatrix -> Row -> Col -> BasicSym -> IO ()
densematrix_set mat r c sym =
    with2 mat sym (\m s -> cdensematrix_set_basic_ffi m (fromIntegral r) (fromIntegral c) s)


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
