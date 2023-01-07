{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Symengine.Internal
  ( Basic,
    basicFromText,
    basicToText,
    constZero,
    constOne,
    isNumber,
    isPositive,
    isNegative,
    isZero,
    isComplex,
    symengineVersion,
  )
where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Bits (toIntegralSized)
import Data.ByteString (packCString, useAsCString)
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CLong (..))
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts (IsString (..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.IO.Unsafe (unsafePerformIO)

data {-# CTYPE "symengine/cwrapper.h" "basic_struct" #-} Cbasic_struct
  = Cbasic_struct
      {-# UNPACK #-} !(Ptr ())
      {-# UNPACK #-} !(Ptr ())
      {-# UNPACK #-} !CInt
  deriving stock (Show, Eq, Generic)

instance Storable Cbasic_struct where
  sizeOf _ = 24
  {-# INLINE sizeOf #-}
  alignment _ = 8
  {-# INLINE alignment #-}
  peek _ = error "Storable instance for Cbasic_struct does not implement peek, because you should not rely on the internal representation of it"
  poke _ _ = error "Storable instance for Cbasic_struct does not implement poke, because you should not rely on the internal representation of it"

data SymengineError
  = RuntimeError
  | DivideByZero
  | NotImplemented
  | DomainError
  | ParseError
  | SerializationError
  deriving stock (Show, Eq, Generic)

instance Enum SymengineError where
  toEnum e = case e of
    1 -> RuntimeError
    2 -> DivideByZero
    3 -> NotImplemented
    4 -> DomainError
    5 -> ParseError
    6 -> SerializationError
    _ -> error "invalid error code"
  fromEnum _ = error "Enum instance of SymengineError does not provide fromEnum"

newtype Basic = Basic (ForeignPtr Cbasic_struct)

-- | Allocate a new 'Basic' and use the provided function for initialization.
newBasic :: (Ptr Cbasic_struct -> IO ()) -> IO Basic
newBasic initialize = do
  x@(Basic fp) <- newBasicNoDestructor initialize
  addForeignPtrFinalizer basic_free_stack fp
  pure x

-- | Same as 'newBasic', but do not attach a finalizer to the underlying 'ForeignPtr'
newBasicNoDestructor :: (Ptr Cbasic_struct -> IO ()) -> IO Basic
newBasicNoDestructor initialize = do
  fp <- mallocForeignPtr
  withForeignPtr fp (\p -> basic_new_stack p >> initialize p)
  pure $ Basic fp

withBasic :: Basic -> (Ptr Cbasic_struct -> IO a) -> IO a
withBasic (Basic fp) = withForeignPtr fp

checkError :: HasCallStack => Text -> CInt -> IO ()
checkError name e
  | e == 0 = pure ()
  | otherwise =
      error $
        Text.unpack name <> " failed with: " <> show (toEnum (fromIntegral e) :: SymengineError)

unaryOp :: (Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()) -> Basic -> Basic
unaryOp = unaryOp' pure

unaryOp' :: (a -> IO ()) -> (Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO a) -> Basic -> Basic
unaryOp' check f x = unsafePerformIO $!
  withBasic x $ \xPtr ->
    newBasic (\out -> check =<< f out xPtr)

binaryOp :: (Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()) -> Basic -> Basic -> Basic
binaryOp = binaryOp' pure

binaryOp' :: (a -> IO ()) -> (Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO a) -> Basic -> Basic -> Basic
binaryOp' check f x y = unsafePerformIO $!
  withBasic x $ \xPtr ->
    withBasic y $ \yPtr ->
      newBasic (\out -> check =<< f out xPtr yPtr)

queryOp :: (Ptr Cbasic_struct -> IO CInt) -> Basic -> Bool
queryOp f x =
  unsafePerformIO $!
    withBasic x $
      fmap (toEnum . fromIntegral) . f

basicToText :: Basic -> Text
basicToText x = unsafePerformIO $
  withBasic x $ \p ->
    bracket (basic_str p) basic_str_free $ \cStr -> do
      -- NOTE: need to force evaluation before the C string is freed
      !r <- peekUtf8 cStr
      pure r

basicFromText :: Text -> Maybe Basic
basicFromText s = unsafePerformIO $!
  withUtf8 s $ \cStr -> do
    x <- newBasic (\_ -> pure ())
    withBasic x $ \p -> do
      e <- basic_parse p cStr
      if e /= 0
        then case toEnum (fromIntegral e) of
          ParseError -> pure Nothing
          otherError -> error $ "basic_parse of " <> show s <> " failed with: " <> show otherError
        else pure (Just x)

instance Show Basic where
  showsPrec p x =
    showParen (p > 0)
      . showString
      . Text.unpack
      . basicToText
      $ x

instance IsString Basic where
  fromString s = case (basicFromText . Text.pack) s of
    Just x -> x
    Nothing -> error $ "could not convert " <> show s <> " to Basic"

instance Eq Basic where
  (==) a b = unsafePerformIO $!
    withBasic a $ \aPtr ->
      withBasic b $ \bPtr ->
        toEnum . fromIntegral <$> basic_eq aPtr bPtr

basicFromInt :: Int -> Basic
basicFromInt n =
  unsafePerformIO $! do
    x <- newBasic (\_ -> pure ())
    withBasic x $ \p -> do
      checkError "integer_set_si" =<< integer_set_si p (fromIntegral n)
      pure x

isZero :: Basic -> Bool
isZero x = queryOp number_is_zero x

isPositive :: Basic -> Bool
isPositive x = queryOp number_is_positive x

isNegative :: Basic -> Bool
isNegative x = queryOp number_is_negative x

isNumber :: Basic -> Bool
isNumber = queryOp is_a_Number

isComplex :: Basic -> Bool
isComplex = queryOp is_a_Complex

instance Num Basic where
  (+) = binaryOp basic_add
  (-) = binaryOp basic_sub
  (*) = binaryOp basic_mul
  negate = unaryOp basic_neg
  abs = unaryOp basic_abs
  signum _ = error "Num instance of Basic does not implement signum"
  fromInteger n = case toIntegralSized n of
    Just k -> basicFromInt k
    Nothing -> error $ "integer overflow in fromInteger " <> show n

instance Fractional Basic where
  (/) = binaryOp basic_div
  fromRational r =
    binaryOp'
      (checkError "rational_set")
      rational_set
      (fromInteger (numerator r))
      (fromInteger (denominator r))
  recip r = constOne / r

instance Floating Basic where
  pi = constPi
  exp = unaryOp' (checkError "basic_exp") basic_exp
  log = unaryOp' (checkError "basic_log") basic_log
  (**) = binaryOp' (checkError "basic_pow") basic_pow
  sqrt = unaryOp' (checkError "basic_sqrt") basic_sqrt
  sin = unaryOp' (checkError "basic_sin") basic_sin
  cos = unaryOp' (checkError "basic_cos") basic_cos
  tan = unaryOp' (checkError "basic_tan") basic_tan
  asin = unaryOp' (checkError "basic_asin") basic_asin
  acos = unaryOp' (checkError "basic_acos") basic_acos
  atan = unaryOp' (checkError "basic_atan") basic_atan
  sinh = unaryOp' (checkError "basic_sinh") basic_sinh
  cosh = unaryOp' (checkError "basic_cosh") basic_cosh
  tanh = unaryOp' (checkError "basic_tanh") basic_tanh
  asinh = unaryOp' (checkError "basic_asinh") basic_asinh
  acosh = unaryOp' (checkError "basic_acosh") basic_acosh
  atanh = unaryOp' (checkError "basic_atanh") basic_atanh

constZero :: Basic
constZero = unsafePerformIO $! newBasicNoDestructor basic_const_zero

constOne :: Basic
constOne = unsafePerformIO $! newBasicNoDestructor basic_const_one

constPi :: Basic
constPi = unsafePerformIO $! newBasicNoDestructor basic_const_pi

-- | Unicode-safe alternative to 'peekCString'
peekUtf8 :: CString -> IO Text
peekUtf8 = fmap decodeUtf8 . packCString

-- | Unicode-safe alternative to 'withCString'
withUtf8 :: Text -> (CString -> IO a) -> IO a
withUtf8 x = useAsCString (encodeUtf8 x)

-- | Version of the underlying SymEngine C++ library
symengineVersion :: Text
symengineVersion = unsafePerformIO $ peekUtf8 =<< symengine_version
{-# NOINLINE symengineVersion #-}

foreign import capi unsafe "symengine/cwrapper.h basic_new_stack"
  basic_new_stack :: Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h &basic_free_stack"
  basic_free_stack :: FunPtr (Ptr Cbasic_struct -> IO ())

foreign import capi unsafe "symengine/cwrapper.h basic_str"
  basic_str :: Ptr Cbasic_struct -> IO CString

foreign import capi unsafe "symengine/cwrapper.h basic_str_free"
  basic_str_free :: CString -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_const_zero"
  basic_const_zero :: Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_const_one"
  basic_const_one :: Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_const_minus_one"
  basic_const_minus_one :: Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_const_I"
  basic_const_I :: Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_const_pi"
  basic_const_pi :: Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_const_E"
  basic_const_E :: Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_const_EulerGamma"
  basic_const_EulerGamma :: Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_const_Catalan"
  basic_const_Catalan :: Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_const_GoldenRatio"
  basic_const_GoldenRatio :: Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h integer_set_si"
  integer_set_si :: Ptr Cbasic_struct -> CLong -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h rational_set"
  rational_set :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_parse"
  basic_parse :: Ptr Cbasic_struct -> CString -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_eq"
  basic_eq :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_add"
  basic_add :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_sub"
  basic_sub :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_mul"
  basic_mul :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_div"
  basic_div :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_pow"
  basic_pow :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_abs"
  basic_abs :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_neg"
  basic_neg :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

foreign import capi unsafe "symengine/cwrapper.h basic_sqrt"
  basic_sqrt :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_sin"
  basic_sin :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_cos"
  basic_cos :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_tan"
  basic_tan :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_asin"
  basic_asin :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_acos"
  basic_acos :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_atan"
  basic_atan :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_sinh"
  basic_sinh :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_cosh"
  basic_cosh :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_tanh"
  basic_tanh :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_asinh"
  basic_asinh :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_acosh"
  basic_acosh :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_atanh"
  basic_atanh :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_exp"
  basic_exp :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h basic_log"
  basic_log :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h number_is_zero"
  number_is_zero :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h number_is_negative"
  number_is_negative :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h number_is_positive"
  number_is_positive :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h number_is_complex"
  number_is_complex :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h is_a_Number"
  is_a_Number :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h is_a_Integer"
  is_a_Integer :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h is_a_Rational"
  is_a_Rational :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h is_a_Symbol"
  is_a_Symbol :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h is_a_Complex"
  is_a_Complex :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h is_a_RealDouble"
  is_a_RealDouble :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h is_a_ComplexDouble"
  is_a_ComplexDouble :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h is_a_RealMPFR"
  is_a_RealMPFR :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h is_a_ComplexMPC"
  is_a_ComplexMPC :: Ptr Cbasic_struct -> IO CInt

foreign import capi unsafe "symengine/cwrapper.h is_a_Set"
  is_a_Set :: Ptr Cbasic_struct -> IO CInt

foreign import ccall unsafe "symengine/cwrapper.h symengine_version"
  symengine_version :: IO CString
