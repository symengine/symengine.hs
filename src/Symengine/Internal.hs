{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- |
-- Module      : Symengine.Internal
-- Description : Symengine bindings to Haskell
module Symengine.Internal
  ( constructBasicFrom
  , constructStringFrom
  , createDenseMatrixVia
  , mkUnaryFunction
  , mkBinaryFunction
  , unpackFunction
  ) where

import Control.Exception (bracket_)
import Data.ByteString (packCString)
import Data.Text.Encoding qualified as T
import Foreign.Marshal (allocaBytes)
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.Haskell.TH (Exp, Q)
import System.IO.Unsafe

constructBasicFrom :: String -> Q Exp
constructBasicFrom expr =
  C.substitute
    [("expr", const expr)]
    [|
      constructBasic $ \dest ->
        [CU.block| void {
          using namespace SymEngine;
          new ($(Object* dest)) Object{@expr()};
        } |]
      |]

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
