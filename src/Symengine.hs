{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- |
-- Module      : Symengine
-- Description : Symengine bindings to Haskell
module Symengine
  ( Basic (..)
  , DenseMatrix (..)
  , symbol
  , parse
  , e
  , infinity
  , nan
  , diff
  , evalf
  , subs
  , inverse
  , identityMatrix
  , zeroMatrix
  , allocaCxxInteger
  , peekCxxInteger
  , withCxxInteger
  , EvalDomain (..)
  , InverseMethod (..)
  , toAST
  , fromAST
  , AST (..)
  ) where

import Control.Exception (bracket, bracket_)
import Control.Monad
import Data.ByteString (useAsCString)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal (allocaBytesAligned, toBool, withArrayLen)
import Foreign.Ptr
import GHC.Exts (IsString (..))
import GHC.Int
import GHC.Num.BigNat
import GHC.Num.Integer
import GHC.Real (Ratio (..))
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Symengine.Context
import Symengine.Internal
import System.IO.Unsafe

-- | Basic building block of SymEngine expressions.
newtype Basic = Basic (ForeignPtr CxxBasic)

data DenseMatrix a = DenseMatrix {dmRows :: !Int, dmCols :: !Int, dmData :: !(Vector a)}

data CxxBasic

data CxxInteger

data CxxString

data CxxMapBasicBasic

importSymengine

-- | Convert a pointer to @std::string@ into a string.
--
-- It properly handles unicode characters.
-- peekCxxString :: Ptr CxxString -> IO Text
-- peekCxxString p =
--   fmap T.decodeUtf8 $
--     packCString
--       =<< [CU.exp| char const* { $(const std::string* p)->c_str() } |]

-- | Call 'peekCxxString' and @delete@ the pointer.
-- peekAndDeleteCxxString :: Ptr CxxString -> IO Text
-- peekAndDeleteCxxString p = do
--   s <- peekCxxString p
--   [CU.exp| void { delete $(const std::string* p) } |]
--   pure s
constructBasic :: (Ptr CxxBasic -> IO ()) -> IO Basic
constructBasic construct =
  fmap Basic $ constructWithDeleter size deleter $ \ptr -> do
    [CU.block| void { new ($(Object* ptr)) Object{}; } |]
    construct ptr
  where
    size = fromIntegral [CU.pure| size_t { sizeof(Object) } |]
    deleter = [C.funPtr| void deleteBasic(Object* ptr) { ptr->~Object(); } |]

constructWithDeleter :: Int -> FinalizerPtr a -> (Ptr a -> IO ()) -> IO (ForeignPtr a)
constructWithDeleter size deleter constructor = do
  fp <- mallocForeignPtrBytes size
  withForeignPtr fp constructor
  addForeignPtrFinalizer deleter fp
  pure fp

-- newtype VecBasic = VecBasic (ForeignPtr CxxVecBasic)

-- constructVecBasic :: (Ptr CxxVecBasic -> IO ()) -> IO VecBasic
-- constructVecBasic construct =
--   fmap VecBasic $ constructWithDeleter size deleter $ \ptr -> do
--     [CU.block| void { new ($(SymEngine::vec_basic* ptr)) SymEngine::vec_basic{}; } |]
--     construct ptr
--   where
--     size = fromIntegral [CU.pure| size_t { sizeof(SymEngine::vec_basic) } |]
--     deleter = [C.funPtr| void deleteBasic(SymEngine::vec_basic* ptr) { ptr->~vector<Object>(); } |]

withBasic :: Basic -> (Ptr CxxBasic -> IO a) -> IO a
withBasic (Basic fp) = withForeignPtr fp

-- withVecBasic :: VecBasic -> (Ptr CxxVecBasic -> IO a) -> IO a
-- withVecBasic (VecBasic fp) = withForeignPtr fp

-- vecBasicToList :: VecBasic -> [Basic]
-- vecBasicToList v = unsafePerformIO $
--   withVecBasic v $ \v' -> do
--     size <- [CU.exp| size_t { $(const SymEngine::vec_basic* v')->size() } |]
--     forM [0 .. size - 1] $ \i ->
--       constructBasic $ \dest ->
--         [CU.exp| void { CONSTRUCT_BASIC($(Object* dest),
--                                         $(const SymEngine::vec_basic* v')->at($(size_t i))) } |]

cxxVectorSize :: Ptr (Vector Basic) -> IO Int
cxxVectorSize ptr = fromIntegral <$> [CU.exp| size_t { $(const Vector* ptr)->size() } |]

cxxVectorIndex :: Ptr (Vector Basic) -> Int -> IO Basic
cxxVectorIndex ptr (fromIntegral -> i) =
  $(constructBasicFrom "$(const Vector* ptr)->at($(size_t i))")

cxxVectorPushBack :: Ptr (Vector Basic) -> Basic -> IO ()
cxxVectorPushBack ptr basic =
  withBasic basic $ \x ->
    [CU.exp| void { $(Vector* ptr)->push_back(*$(const Object* x)) } |]

peekVector :: Ptr (Vector Basic) -> IO (Vector Basic)
peekVector ptr = do
  size <- cxxVectorSize ptr
  V.forM (V.enumFromStepN 0 1 size) (cxxVectorIndex ptr)

allocaVector :: (Ptr (Vector Basic) -> IO a) -> IO a
allocaVector action =
  allocaBytesAligned sizeBytes alignmentBytes $ \v ->
    let construct = [CU.exp| void { new ($(Vector* v)) Vector{} } |]
        destruct = [CU.exp| void { $(Vector* v)->~Vector() } |]
     in bracket_ construct destruct (action v)
  where
    sizeBytes = fromIntegral [CU.pure| size_t { sizeof(Vector) } |]
    alignmentBytes = fromIntegral [CU.pure| size_t { alignof(Vector) } |]

withVector :: Vector Basic -> (Ptr (Vector Basic) -> IO a) -> IO a
withVector v action = do
  allocaVector $ \ptr -> do
    V.forM_ v $ cxxVectorPushBack ptr
    action ptr

-- \$ \dest ->
--   [CU.exp| void { CONSTRUCT_BASIC($(Object* dest),
--                                   $(const Vector* ptr)->at($(size_t i))) } |]

allocaDenseMatrix :: Int -> Int -> (Ptr (DenseMatrix Basic) -> IO a) -> IO a
allocaDenseMatrix (fromIntegral -> nrows) (fromIntegral -> ncols) action = do
  allocaBytesAligned sizeBytes alignmentBytes $ \v ->
    let construct =
          [CU.exp| void { new ($(DenseMatrix * v)) DenseMatrix{
            $(unsigned nrows), $(unsigned ncols)} } |]
        destruct = [CU.exp| void { $(DenseMatrix* v)->~DenseMatrix() } |]
     in bracket_ construct destruct (action v)
  where
    sizeBytes = fromIntegral [CU.pure| size_t { sizeof(DenseMatrix) } |]
    alignmentBytes = fromIntegral [CU.pure| size_t { alignof(DenseMatrix) } |]

withDenseMatrix :: DenseMatrix Basic -> (Ptr (DenseMatrix Basic) -> IO a) -> IO a
withDenseMatrix matrix action =
  allocaDenseMatrix 0 0 $ \ptr ->
    withVector (dmData matrix) $ \v -> do
      let n = fromIntegral $ dmRows matrix
          m = fromIntegral $ dmCols matrix
      [CU.block| void {
        *$(DenseMatrix* ptr) = DenseMatrix{$(unsigned n), $(unsigned m), *$(const Vector* v)};
      } |]
      action ptr

peekDenseMatrix :: Ptr (DenseMatrix Basic) -> IO (DenseMatrix Basic)
peekDenseMatrix ptr = do
  n <- fromIntegral <$> [CU.exp| unsigned { $(const DenseMatrix* ptr)->nrows() } |]
  m <- fromIntegral <$> [CU.exp| unsigned { $(const DenseMatrix* ptr)->ncols() } |]
  v <-
    allocaVector $ \v -> do
      [CU.block| void { *$(Vector* v) = $(const DenseMatrix* ptr)->as_vec_basic(); } |]
      peekVector v
  pure $ DenseMatrix n m v

instance Show Basic where
  show basic = unpack . unsafePerformIO $
    withBasic basic $ \basic' ->
      $(constructStringFrom "SymEngine::str(**$(Object* basic'))")

deriving stock instance Show (DenseMatrix Basic)

instance Eq Basic where
  a == b = unsafePerformIO $
    withBasic a $ \a' ->
      withBasic b $ \b' ->
        toBool
          <$> [CU.exp| bool { eq(**$(const Object* a'), **$(const Object* b')) } |]

parse :: Text -> Basic
parse (T.encodeUtf8 -> name) =
  unsafePerformIO $ $(constructBasicFrom "parse($bs-cstr:name)")

instance IsString Basic where
  fromString = parse . pack

symbol :: Text -> Basic
symbol (T.encodeUtf8 -> name) =
  unsafePerformIO $
    $(constructBasicFrom "symbol(std::string{$bs-ptr:name, static_cast<size_t>($bs-len:name)})")

-- constructBasic $ \dest ->
--   [CU.exp| void { new ($(Object* dest)) Object{} } |]

-- pureUnaryOp :: (Ptr CxxBasic -> Ptr CxxBasic -> IO ()) -> Basic -> Basic
-- pureUnaryOp f a = unsafePerformIO $
--   withBasic a $ \a' ->
--     constructBasic $ \dest ->
--       f dest a'

-- pureBinaryOp :: (Ptr CxxBasic -> Ptr CxxBasic -> Ptr CxxBasic -> IO ()) -> Basic -> Basic -> Basic
-- pureBinaryOp f a b = unsafePerformIO $
--   withBasic a $ \a' ->
--     withBasic b $ \b' ->
--       constructBasic $ \dest ->
--         f dest a' b'

allocaCxxInteger :: (Ptr CxxInteger -> IO a) -> IO a
allocaCxxInteger f =
  allocaBytesAligned sizeBytes alignmentBytes $ \i ->
    let construct =
          [CU.exp| void { new ($(integer_class * i)) integer_class{} } |]
        destruct = [CU.exp| void { $(integer_class * i)->~integer_class() } |]
     in bracket_ construct destruct (f i)
  where
    sizeBytes = fromIntegral [CU.pure| size_t { sizeof(integer_class) } |]
    alignmentBytes = fromIntegral [CU.pure| size_t { alignof(integer_class) } |]

integerToWords :: Integer -> [Word]
integerToWords (IP b) = bigNatToWordList b
integerToWords (IN b) = bigNatToWordList b
integerToWords (IS n) = [fromIntegral (abs (I# n))]

withCxxInteger :: Integer -> (Ptr CxxInteger -> IO a) -> IO a
withCxxInteger n action =
  allocaCxxInteger $ \i ->
    withArrayLen (fromIntegral <$> integerToWords n) $
      \(fromIntegral -> numWords) wordsPtr -> do
        [CU.block| void {
          auto const numWords = $(int numWords);
          auto const* words = $(const uint64_t* wordsPtr);
          if (numWords > 0) {
            integer_class x{words[0]};
            for (int k = 1; k < numWords; ++k) {
              x <<= 64;
              x += words[k];
            }
            *$(integer_class* i) = x;
          }
        } |]
        when (n < 0) $ do
          [CU.block| void {
            auto& i = *$(integer_class* i);
            i = -i;
          } |]
        action i

peekCxxInteger :: Ptr CxxInteger -> IO Integer
peekCxxInteger i = do
  allocaCxxInteger $ \j -> do
    isNegative <-
      toBool
        <$> [CU.block| bool {
              auto const& i = *$(integer_class const* i);
              auto& j = *$(integer_class* j);
              j = mp_abs(i);
              return i < 0;
            } |]
    let go acc = do
          w <-
            [CU.block| uint64_t {
              auto const& j = *$(integer_class const* j);
              return mp_get_ui(j);
            } |]
          continue <-
            toBool
              <$> [CU.block| bool {
                    auto& j = *$(integer_class* j);
                    j >>= 64;
                    return j != 0;
                  } |]
          if continue
            then go $ w : acc
            else pure $ w : acc
    integerFromWordList isNegative . fmap fromIntegral <$> go []

instance Num Basic where
  fromInteger n = unsafePerformIO $
    withCxxInteger n $ \i ->
      $(constructBasicFrom "integer(*$(const integer_class* i))")
  (+) = $(mkBinaryFunction "add(a, b)")
  (-) = $(mkBinaryFunction "sub(a, b)")
  (*) = $(mkBinaryFunction "mul(a, b)")
  abs = $(mkUnaryFunction "abs(a)")
  signum = $(mkUnaryFunction "sign(a)")

instance Fractional Basic where
  (/) = $(mkBinaryFunction "div(a, b)")
  fromRational (numer :% denom) =
    unsafePerformIO $
      withCxxInteger numer $ \numer' ->
        withCxxInteger denom $ \denom' ->
          $( constructBasicFrom
              "Rational::from_two_ints(\
              \Integer(*$(const integer_class* numer')),\
              \Integer(*$(const integer_class* denom')))"
           )

e :: Basic
e = unsafePerformIO $ $(constructBasicFrom "E")
{-# NOINLINE e #-}

infinity :: Basic
infinity = unsafePerformIO $ $(constructBasicFrom "Inf")
{-# NOINLINE infinity #-}

nan :: Basic
nan = unsafePerformIO $ $(constructBasicFrom "Nan")
{-# NOINLINE nan #-}

instance Floating Basic where
  pi = unsafePerformIO $ $(constructBasicFrom "pi")
  exp = $(mkUnaryFunction "exp(a)")
  log = $(mkUnaryFunction "log(a)")
  sqrt = $(mkUnaryFunction "sqrt(a)")
  (**) = $(mkBinaryFunction "pow(a, b)")
  sin = $(mkUnaryFunction "sin(a)")
  cos = $(mkUnaryFunction "cos(a)")
  tan = $(mkUnaryFunction "tan(a)")
  asin = $(mkUnaryFunction "asin(a)")
  acos = $(mkUnaryFunction "acos(a)")
  atan = $(mkUnaryFunction "atan(a)")
  sinh = $(mkUnaryFunction "sinh(a)")
  cosh = $(mkUnaryFunction "cosh(a)")
  tanh = $(mkUnaryFunction "tanh(a)")
  asinh = $(mkUnaryFunction "asinh(a)")
  acosh = $(mkUnaryFunction "acosh(a)")
  atanh = $(mkUnaryFunction "atanh(a)")

diff :: Basic -> Basic -> Basic
diff f x
  | basicTypeCode x == [CU.pure| int { static_cast<int>(SYMENGINE_SYMBOL) } |] =
      $(mkBinaryFunction "a->diff(rcp_static_cast<Symbol const>(b))") f x
  | otherwise = error "can only differentiate with respect to symbols"

data EvalDomain = EvalComplex | EvalReal | EvalSymbolic
  deriving stock (Show, Eq)

evalDomainToCInt :: EvalDomain -> CInt
evalDomainToCInt EvalComplex = [CU.pure| int { static_cast<int>(SymEngine::EvalfDomain::Complex) } |]
evalDomainToCInt EvalReal = [CU.pure| int { static_cast<int>(SymEngine::EvalfDomain::Real) } |]
evalDomainToCInt EvalSymbolic = [CU.pure| int { static_cast<int>(SymEngine::EvalfDomain::Symbolic) } |]

evalf :: EvalDomain -> Int -> Basic -> Basic
evalf (evalDomainToCInt -> domain) (fromIntegral -> bits) x = unsafePerformIO $
  withBasic x $ \x' ->
    $(constructBasicFrom "evalf(**$(const Object* x'), $(int bits), static_cast<EvalfDomain>($(int domain)))")

withCxxMapBasicBasic :: [(Basic, Basic)] -> (Ptr CxxMapBasicBasic -> IO a) -> IO a
withCxxMapBasicBasic pairs action =
  bracket allocate destroy $ \p -> do
    forM_ pairs $ \(from, to) ->
      withBasic from $ \fromPtr -> withBasic to $ \toPtr ->
        [CU.exp| void {
          $(map_basic_basic* p)->emplace(*$(Object const* fromPtr), *$(Object const* toPtr)) } |]
    action p
  where
    allocate = [CU.exp| map_basic_basic* { new map_basic_basic } |]
    destroy p = [CU.exp| void { delete $(map_basic_basic* p) } |]

subs :: [(Basic, Basic)] -> Basic -> Basic
subs replacements expr =
  unsafePerformIO $
    withCxxMapBasicBasic replacements $ \replacementsPtr ->
      withBasic expr $ \exprPtr ->
        $(constructBasicFrom "subs(*$(Object const* exprPtr), *$(map_basic_basic const* replacementsPtr))")

generateDenseMatrix :: Int -> Int -> (Int -> Int -> Basic) -> DenseMatrix Basic
generateDenseMatrix nrows ncols f =
  DenseMatrix nrows ncols $
    V.generate (nrows * ncols) $ \i ->
      let (!r, !c) = i `divMod` ncols
       in f r c

identityMatrix :: Int -> DenseMatrix Basic
identityMatrix n = generateDenseMatrix n n (\i j -> if i == j then 1 else 0)

zeroMatrix :: Int -> Int -> DenseMatrix Basic
zeroMatrix n m = generateDenseMatrix n m (\_ _ -> 0)

data InverseMethod
  = InverseDefault
  | InverseFractionFreeLU
  | InverseLU
  | InversePivotedLU
  | InverseGaussJordan
  deriving stock (Show, Eq)

inverse :: InverseMethod -> DenseMatrix Basic -> DenseMatrix Basic
inverse InverseDefault m = unsafePerformIO $ withDenseMatrix m $ \a ->
  $( createDenseMatrixVia
      "auto const& a = *$(const DenseMatrix* a);\
      \out.resize(a.nrows(), a.ncols());\
      \a.inv(out);"
   )

data AST
  = SymengineInteger Integer
  | SymengineRational Rational
  | SymengineInfinity
  | SymengineNaN
  | SymengineConstant Basic
  | SymengineSymbol Text
  | SymengineMul (Vector Basic)
  | SymengineAdd (Vector Basic)
  | SymenginePow Basic Basic
  | SymengineLog Basic
  | SymengineSign Basic
  | SymengineFunction Text (Vector Basic)
  | SymengineDerivative Basic (Vector Basic)
  deriving stock (Show, Eq)

basicTypeCode :: Basic -> CInt
basicTypeCode x = unsafePerformIO $
  withBasic x $
    \x' -> [CU.exp| int { static_cast<int>((*$(const Object* x'))->get_type_code()) } |]

forceOneArg :: (Basic -> a) -> Vector Basic -> a
forceOneArg f v = case V.toList v of
  [a] -> f a
  _ -> error "expected a one-element vector"

forceTwoArgs :: (Basic -> Basic -> a) -> Vector Basic -> a
forceTwoArgs f v = case V.toList v of
  [a, b] -> f a b
  _ -> error "expected a two-element vector"

unsafeIntegerToAST :: Basic -> AST
unsafeIntegerToAST x = SymengineInteger n
  where
    n = unsafePerformIO $
      withBasic x $ \x' ->
        allocaCxxInteger $ \i -> do
          [CU.exp| void {
            *$(integer_class* i) =
              down_cast<Integer const&>(**$(const Object* x')).as_integer_class()
          } |]
          peekCxxInteger i

unsafeRationalToAST :: Basic -> AST
unsafeRationalToAST x = SymengineRational q
  where
    q = unsafePerformIO $
      withBasic x $ \x' ->
        allocaCxxInteger $ \m ->
          allocaCxxInteger $ \n -> do
            [CU.block| void {
              auto const& x =
                down_cast<Rational const&>(**$(const Object* x')).as_rational_class();
              *$(integer_class* m) = x.get_num();
              *$(integer_class* n) = x.get_den();
            } |]
            (:%) <$> peekCxxInteger m <*> peekCxxInteger n

unsafeSymbolToAST :: Basic -> AST
unsafeSymbolToAST x = SymengineSymbol . unsafePerformIO $ do
  withBasic x $ \x' ->
    $(constructStringFrom "down_cast<Symbol const&>(**$(const Object* x')).get_name()")

toAST :: Basic -> AST
toAST x
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_INTEGER) } |] = unsafeIntegerToAST x
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_RATIONAL) } |] = unsafeRationalToAST x
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_INFTY) } |] = SymengineInfinity
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_NOT_A_NUMBER) } |] = SymengineNaN
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_CONSTANT) } |] = SymengineConstant x
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_SYMBOL) } |] = unsafeSymbolToAST x
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_ADD) } |] =
      unsafePerformIO $ SymengineAdd . V.reverse <$> $(unpackFunction "Add") x
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_MUL) } |] =
      unsafePerformIO $ SymengineMul <$> $(unpackFunction "Mul") x
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_POW) } |] =
      unsafePerformIO $ forceTwoArgs SymenginePow <$> $(unpackFunction "Pow") x
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_LOG) } |] =
      unsafePerformIO $ forceOneArg SymengineLog <$> $(unpackFunction "Log") x
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_SIGN) } |] =
      unsafePerformIO $ forceOneArg SymengineSign <$> $(unpackFunction "Sign") x
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_FUNCTIONSYMBOL) } |] =
      unsafePerformIO $ do
        name <- withBasic x $ \x' ->
          $(constructStringFrom "down_cast<FunctionSymbol const&>(**$(const Object* x')).get_name()")
        args <- $(unpackFunction "FunctionSymbol") x
        pure $ SymengineFunction name args
  | tp == [CU.pure| int { static_cast<int>(SYMENGINE_DERIVATIVE) } |] =
      unsafePerformIO $ do
        args <- $(unpackFunction "Derivative") x
        pure $ SymengineDerivative (V.head args) (V.tail args)
  | otherwise = error $ "unknown type code: " <> show tp
  where
    tp = basicTypeCode x

fromAST :: AST -> Basic
fromAST = \case
  SymengineInteger x -> fromInteger x
  SymengineRational x -> fromRational x
  SymengineInfinity -> infinity
  SymengineNaN -> nan
  SymengineConstant x -> x
  SymengineSymbol x -> symbol x
  SymengineAdd v -> V.foldl' (+) 0 v
  SymengineMul v -> V.foldl' (*) 0 v
  SymenginePow a b -> a ** b
  SymengineLog x -> log x
  SymengineSign x -> signum x
  SymengineDerivative f v -> V.foldl' diff f v
  SymengineFunction (T.encodeUtf8 -> s) v -> unsafePerformIO $
    withVector v $ \args ->
      $(constructBasicFrom "function_symbol(std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}, *$(const Vector* args))")

{-
-- | Convert a C string into a Haskell string properly handling unicode characters.
peekCString :: CString -> IO Text
peekCString = fmap T.decodeUtf8 . packCString

withTempCString :: IO CString -> (CString -> IO a) -> IO a
withTempCString allocate = bracket allocate destroy
  where
    destroy p = [CU.exp| void { basic_str_free($(char* p)) } |]

asciiArt :: IO Text
asciiArt = withTempCString [CU.exp| char* { ascii_art_str() } |] peekCString
-}

-- newtype BasicStruct = BasicStruct
--   { data_ptr :: Ptr ()
--   }
--
-- instance Storable BasicStruct where
--   alignment _ = 8
--   sizeOf _ = sizeOf nullPtr
--   peek basic_ptr = BasicStruct <$> peekByteOff basic_ptr 0
--   poke basic_ptr BasicStruct {..} = pokeByteOff basic_ptr 0 data_ptr
--
-- -- |represents a symbol exported by SymEngine. create this using the functions
-- -- 'zero', 'one', 'minus_one', 'e', 'im', 'rational', 'complex', and also by
-- -- constructing a number and converting it to a Symbol
-- --
-- -- >>> 3.5 :: BasicSym
-- -- 7/2
-- --
-- -- >>> rational 2 10
-- -- 1 /5
-- --
-- -- >>> complex 1 2
-- -- 1 + 2*I
-- data BasicSym = BasicSym {fptr :: ForeignPtr BasicStruct}
--
-- withBasicSym :: BasicSym -> (Ptr BasicStruct -> IO a) -> IO a
-- withBasicSym p f = withForeignPtr (fptr p) f
--
-- withBasicSym2 :: BasicSym -> BasicSym -> (Ptr BasicStruct -> Ptr BasicStruct -> IO a) -> IO a
-- withBasicSym2 p1 p2 f = withBasicSym p1 (\p1 -> withBasicSym p2 (\p2 -> f p1 p2))
--
-- withBasicSym3 :: BasicSym -> BasicSym -> BasicSym -> (Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO a) -> IO a
-- withBasicSym3 p1 p2 p3 f = withBasicSym p1 (\p1 -> withBasicSym p2 (\p2 -> withBasicSym p3 (\p3 -> f p1 p2 p3)))
--
-- -- | constructor for 0
-- zero :: BasicSym
-- zero = basic_obj_constructor basic_const_zero_ffi
--
-- -- | constructor for 1
-- one :: BasicSym
-- one = basic_obj_constructor basic_const_one_ffi
--
-- -- | constructor for -1
-- minus_one :: BasicSym
-- minus_one = basic_obj_constructor basic_const_minus_one_ffi
--
-- -- | constructor for i = sqrt(-1)
-- im :: BasicSym
-- im = basic_obj_constructor basic_const_I_ffi
--
-- -- | the ratio of the circumference of a circle to its radius
-- pi :: BasicSym
-- pi = basic_obj_constructor basic_const_pi_ffi
--
-- -- | The base of the natural logarithm
-- e :: BasicSym
-- e = basic_obj_constructor basic_const_E_ffi
--
-- expand :: BasicSym -> BasicSym
-- expand = basic_unaryop basic_expand_ffi
--
-- eulerGamma :: BasicSym
-- eulerGamma = basic_obj_constructor basic_const_EulerGamma_ffi
--
-- basic_obj_constructor :: (Ptr BasicStruct -> IO ()) -> BasicSym
-- basic_obj_constructor init_fn = unsafePerformIO $ do
--   basic_ptr <- create_basic_ptr
--   withBasicSym basic_ptr init_fn
--   return basic_ptr
--
-- basic_str :: BasicSym -> String
-- basic_str basic_ptr = unsafePerformIO $ withBasicSym basic_ptr (basic_str_ffi >=> peekCString)
--
-- integerToCLong :: Integer -> CLong
-- integerToCLong i = CLong (fromInteger i)
--
-- intToCLong :: Int -> CLong
-- intToCLong i = integerToCLong (toInteger i)
--
-- basic_int_signed :: Int -> BasicSym
-- basic_int_signed i = unsafePerformIO $ do
--   iptr <- create_basic_ptr
--   withBasicSym iptr (\iptr -> integer_set_si_ffi iptr (intToCLong i))
--   return iptr
--
-- basic_from_integer :: Integer -> BasicSym
-- basic_from_integer i = unsafePerformIO $ do
--   iptr <- create_basic_ptr
--   withBasicSym iptr (\iptr -> integer_set_si_ffi iptr (fromInteger i))
--   return iptr
--
-- -- |The `ascii_art_str` function prints SymEngine in ASCII art.
-- -- this is useful as a sanity check
-- ascii_art_str :: IO String
-- ascii_art_str = ascii_art_str_ffi >>= peekCString
--
-- -- Unexported ffi functions------------------------
--
-- -- |Create a basic object that represents all other objects through
-- -- the FFI
-- create_basic_ptr :: IO BasicSym
-- create_basic_ptr = do
--   basic_ptr <- newArray [BasicStruct {data_ptr = nullPtr}]
--   basic_new_heap_ffi basic_ptr
--   finalized_ptr <- newForeignPtr ptr_basic_free_heap_ffi basic_ptr
--   return $ BasicSym {fptr = finalized_ptr}
--
-- basic_binaryop :: (Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()) -> BasicSym -> BasicSym -> BasicSym
-- basic_binaryop f a b = unsafePerformIO $ do
--   s <- create_basic_ptr
--   withBasicSym3 s a b f
--   return s
--
-- basic_unaryop :: (Ptr BasicStruct -> Ptr BasicStruct -> IO ()) -> BasicSym -> BasicSym
-- basic_unaryop f a = unsafePerformIO $ do
--   s <- create_basic_ptr
--   withBasicSym2 s a f
--   return s
--
-- basic_pow :: BasicSym -> BasicSym -> BasicSym
-- basic_pow = basic_binaryop basic_pow_ffi
--
-- -- |Create a rational number with numerator and denominator
-- rational :: BasicSym -> BasicSym -> BasicSym
-- rational = basic_binaryop rational_set_ffi
--
-- -- |Create a complex number a + b * im
-- complex :: BasicSym -> BasicSym -> BasicSym
-- complex a b = (basic_binaryop complex_set_ffi) a b
--
-- basic_rational_from_integer :: Integer -> Integer -> BasicSym
-- basic_rational_from_integer i j = unsafePerformIO $ do
--   s <- create_basic_ptr
--   withBasicSym s (\s -> rational_set_si_ffi s (integerToCLong i) (integerToCLong j))
--   return s
--
-- -- |Create a symbol with the given name
-- symbol :: String -> BasicSym
-- symbol name = unsafePerformIO $ do
--   s <- create_basic_ptr
--   cname <- newCString name
--   withBasicSym s (\s -> symbol_set_ffi s cname)
--   free cname
--   return s
--
-- -- |Differentiate an expression with respect to a symbol
-- diff :: BasicSym -> BasicSym -> BasicSym
-- diff expr symbol = (basic_binaryop basic_diff_ffi) expr symbol
--
-- instance Show BasicSym where
--   show = basic_str
--
-- instance Eq BasicSym where
--   (==) a b = unsafePerformIO $ do
--     i <- withBasicSym2 a b basic_eq_ffi
--     return $ i == 1
--
-- instance Num BasicSym where
--   (+) = basic_binaryop basic_add_ffi
--   (-) = basic_binaryop basic_sub_ffi
--   (*) = basic_binaryop basic_mul_ffi
--   negate = basic_unaryop basic_neg_ffi
--   abs = basic_unaryop basic_abs_ffi
--   signum = undefined
--   fromInteger = basic_from_integer
--
-- instance Fractional BasicSym where
--   (/) = basic_binaryop basic_div_ffi
--   fromRational (num :% denom) = basic_rational_from_integer num denom
--   recip r = one / r
--
-- instance Floating BasicSym where
--   pi = Symengine.pi
--   exp x = e ** x
--   log = undefined
--   sqrt x = x ** 1 / 2
--   (**) = basic_pow
--   logBase = undefined
--   sin = basic_unaryop basic_sin_ffi
--   cos = basic_unaryop basic_cos_ffi
--   tan = basic_unaryop basic_tan_ffi
--   asin = basic_unaryop basic_asin_ffi
--   acos = basic_unaryop basic_acos_ffi
--   atan = basic_unaryop basic_atan_ffi
--   sinh = basic_unaryop basic_sinh_ffi
--   cosh = basic_unaryop basic_cosh_ffi
--   tanh = basic_unaryop basic_tanh_ffi
--   asinh = basic_unaryop basic_asinh_ffi
--   acosh = basic_unaryop basic_acosh_ffi
--   atanh = basic_unaryop basic_atanh_ffi
--
-- foreign import ccall "symengine/cwrapper.h ascii_art_str" ascii_art_str_ffi :: IO CString
-- foreign import ccall "symengine/cwrapper.h basic_new_heap" basic_new_heap_ffi :: Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h &basic_free_heap" ptr_basic_free_heap_ffi :: FunPtr (Ptr BasicStruct -> IO ())
--
-- -- constants
-- foreign import ccall "symengine/cwrapper.h basic_const_zero" basic_const_zero_ffi :: Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_const_one" basic_const_one_ffi :: Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_const_minus_one" basic_const_minus_one_ffi :: Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_const_I" basic_const_I_ffi :: Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_const_pi" basic_const_pi_ffi :: Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_const_E" basic_const_E_ffi :: Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_const_EulerGamma" basic_const_EulerGamma_ffi :: Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_str" basic_str_ffi :: Ptr BasicStruct -> IO CString
-- foreign import ccall "symengine/cwrapper.h basic_eq" basic_eq_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO Int
--
-- foreign import ccall "symengine/cwrapper.h symbol_set" symbol_set_ffi :: Ptr BasicStruct -> CString -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_diff" basic_diff_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
--
-- foreign import ccall "symengine/cwrapper.h integer_set_si" integer_set_si_ffi :: Ptr BasicStruct -> CLong -> IO ()
--
-- foreign import ccall "symengine/cwrapper.h rational_set" rational_set_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h rational_set_si" rational_set_si_ffi :: Ptr BasicStruct -> CLong -> CLong -> IO ()
--
-- foreign import ccall "symengine/cwrapper.h complex_set" complex_set_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
--
-- foreign import ccall "symengine/cwrapper.h basic_expand" basic_expand_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
--
-- foreign import ccall "symengine/cwrapper.h basic_add" basic_add_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_sub" basic_sub_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_mul" basic_mul_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_div" basic_div_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_pow" basic_pow_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_neg" basic_neg_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_abs" basic_abs_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
--
-- foreign import ccall "symengine/cwrapper.h basic_sin" basic_sin_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_cos" basic_cos_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_tan" basic_tan_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
--
-- foreign import ccall "symengine/cwrapper.h basic_asin" basic_asin_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_acos" basic_acos_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_atan" basic_atan_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
--
-- foreign import ccall "symengine/cwrapper.h basic_sinh" basic_sinh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_cosh" basic_cosh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_tanh" basic_tanh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
--
-- foreign import ccall "symengine/cwrapper.h basic_asinh" basic_asinh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_acosh" basic_acosh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
-- foreign import ccall "symengine/cwrapper.h basic_atanh" basic_atanh_ffi :: Ptr BasicStruct -> Ptr BasicStruct -> IO ()
