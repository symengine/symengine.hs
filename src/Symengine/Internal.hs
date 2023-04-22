{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Symengine.Internal
-- Description : Symengine bindings to Haskell
module Symengine.Internal
  ( constructStringFrom
  , createDenseMatrixVia
  , mkUnaryFunction
  , mkBinaryFunction
  , unpackFunction
  ) where

import Control.Exception (bracket_)
import Control.Monad
import Data.Bits
import Data.ByteString (packCString)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal (allocaBytes, toBool)
import Foreign.Ptr
import GHC.Exts (IsString (..))
import GHC.Real (Ratio (..))
import Language.C.Inline qualified as C
import Language.C.Inline.Context (Context (ctxTypesTable))
import Language.C.Inline.Cpp qualified as Cpp
import Language.C.Inline.Cpp.Exception qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.C.Types (TypeSpecifier (..))
import Language.Haskell.TH (DecsQ, Exp, Q, TypeQ)
import Symengine.Context
import System.IO.Unsafe

constructStringFrom :: String -> Q Exp
constructStringFrom expr =
  C.substitute
    [("expr", const expr)]
    [|
      let size = fromIntegral [CU.pure| size_t { sizeof(std::string) } |]
          construct s =
            [CU.block| void { 
              using namespace SymEngine;
              new ($(std::string* s)) std::string{@expr()};
            } |]
          destruct s = [CU.exp| void { $(std::string* s)->~basic_string() } |]
       in allocaBytes size $ \s ->
            bracket_ (construct s) (destruct s) $
              fmap T.decodeUtf8 $
                packCString
                  =<< [CU.exp| char const* { $(const std::string* s)->c_str() } |]
      |]

createDenseMatrixVia :: String -> Q Exp
createDenseMatrixVia expr =
  C.substitute
    [("expr", const expr)]
    [|
      allocaDenseMatrix 0 0 $ \ptr -> do
        [CU.block| void {
          auto& out = *$(DenseMatrix* ptr);
          @expr()
        } |]
        peekDenseMatrix ptr
      |]

mkUnaryFunction :: String -> Q Exp
mkUnaryFunction expr =
  C.substitute
    [ ("expr", const expr)
    ]
    [|
      \a' ->
        unsafePerformIO $
          withBasic a' $ \a ->
            constructBasic $ \dest ->
              [CU.block| void {
                using namespace SymEngine;
                auto const& a = *$(const Object* a);
                new ($(Object* dest)) Object{@expr()};
              } |]
      |]

mkBinaryFunction :: String -> Q Exp
mkBinaryFunction expr =
  C.substitute
    [ ("expr", const expr)
    ]
    [|
      \a' b' ->
        unsafePerformIO $
          withBasic a' $ \a ->
            withBasic b' $ \b ->
              constructBasic $ \dest ->
                [CU.block| void {
                  using namespace SymEngine;
                  auto const& a = *$(const Object* a);
                  auto const& b = *$(const Object* b);
                  new ($(Object* dest)) Object{@expr()};
                } |]
      |]

unpackFunction :: String -> Q Exp
unpackFunction className =
  C.substitute
    [ ("class", const className)
    ]
    [|
      \f' ->
        withBasic f' $ \f ->
          allocaVector $ \v -> do
            [CU.block| void {
              using namespace SymEngine;
              auto const& f = down_cast<@class() const&>(**$(const Object* f));
              *$(Vector* v) = f.get_args();
            } |]
            peekVector v
      |]
