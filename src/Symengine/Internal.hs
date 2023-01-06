{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Symengine.Internal
  ( Basic,
    basicFromText,
    basicToText,
    constZero,
    constOne,
    symengineVersion,
  )
where

import Control.Exception (bracket)
import Data.Bits (toIntegralSized)
import Data.ByteString (packCString, useAsCString)
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
import System.IO.Unsafe (unsafePerformIO)

data Cbasic_struct
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

unaryOp :: (Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()) -> Basic -> Basic
unaryOp f x = unsafePerformIO $!
  withBasic x $ \xPtr ->
    newBasic (\out -> f out xPtr)
{-# NOINLINE unaryOp #-}

binaryOp :: (Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()) -> Basic -> Basic -> Basic
binaryOp f x y = unsafePerformIO $!
  withBasic x $ \xPtr ->
    withBasic y $ \yPtr ->
      newBasic (\out -> f out xPtr yPtr)
{-# NOINLINE binaryOp #-}

foreign import ccall unsafe "basic_new_stack"
  basic_new_stack :: Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "&basic_free_stack"
  basic_free_stack :: FunPtr (Ptr Cbasic_struct -> IO ())

foreign import ccall unsafe "basic_str"
  basic_str :: Ptr Cbasic_struct -> IO CString

foreign import ccall unsafe "basic_str_free"
  basic_str_free :: CString -> IO ()

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
      e <- integer_set_si p (fromIntegral n)
      if e /= 0
        then error $ "integer_set_si failed: " <> show (toEnum (fromIntegral e) :: SymengineError)
        else pure x

instance Num Basic where
  (+) = binaryOp basic_add
  (-) = binaryOp basic_sub
  (*) = binaryOp basic_mul
  negate = unaryOp basic_neg
  abs = unaryOp basic_abs
  signum = error "Num instance of Basic does not implement signum"
  fromInteger n = case toIntegralSized n of
    Just k -> basicFromInt k
    Nothing -> error $ "integer overflow in fromInteger " <> show n

constZero :: Basic
constZero = unsafePerformIO $! newBasicNoDestructor basic_const_zero

constOne :: Basic
constOne = unsafePerformIO $! newBasicNoDestructor basic_const_one

foreign import ccall unsafe "basic_const_zero"
  basic_const_zero :: Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_const_one"
  basic_const_one :: Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_const_minus_one"
  basic_const_minus_one :: Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_const_I"
  basic_const_I :: Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_const_pi"
  basic_const_pi :: Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_const_E"
  basic_const_E :: Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_const_EulerGamma"
  basic_const_EulerGamma :: Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_const_Catalan"
  basic_const_Catalan :: Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_const_GoldenRatio"
  basic_const_GoldenRatio :: Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "integer_set_si"
  integer_set_si :: Ptr Cbasic_struct -> CLong -> IO CInt

foreign import ccall unsafe "basic_parse"
  basic_parse :: Ptr Cbasic_struct -> CString -> IO CInt

foreign import ccall unsafe "basic_eq"
  basic_eq :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO CInt

foreign import ccall unsafe "basic_add"
  basic_add :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_sub"
  basic_sub :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_mul"
  basic_mul :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_abs"
  basic_abs :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

foreign import ccall unsafe "basic_neg"
  basic_neg :: Ptr Cbasic_struct -> Ptr Cbasic_struct -> IO ()

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

foreign import ccall unsafe "symengine_version"
  symengine_version :: IO CString
