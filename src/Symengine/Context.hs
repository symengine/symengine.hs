{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Symengine.Context
-- Description : Helpers to setup inline-c for Symengine
-- Copyright   : (c) Tom Westerhout, 2023
--
-- This module defines a Template Haskell function 'importSymengine' that sets up everything you need
-- to call SymEngine functions from 'Language.C.Inline' quasiquotes.
module Symengine.Context
  ( Basic (..)
  , DenseMatrix (..)
  , CxxString
  , CxxBasic
  , CxxInteger
  , importSymengine
  , constructBasicFrom
  )
where

import Data.Kind (Type)
import Data.Map (Map, fromList)
import Data.Vector (Vector)
import Foreign.ForeignPtr
import Language.C.Inline qualified as C
import Language.C.Inline.Context (Context (ctxTypesTable))
import Language.C.Inline.Cpp qualified as Cpp
import Language.C.Inline.Cpp.Exception qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.C.Types (TypeSpecifier (..))
import Language.Haskell.TH (DecsQ, Exp, Q, TypeQ)

-- | Basic building block of SymEngine expressions.
newtype Basic = Basic (ForeignPtr CxxBasic)

data DenseMatrix a = DenseMatrix {dmRows :: !Int, dmCols :: !Int, dmData :: !(Vector a)}

data CxxBasic

data CxxInteger

data CxxString

-- | One stop function to include all the neccessary machinery to call SymEngine functions via
-- inline-c.
--
-- Put @importSymengine@ somewhere at the beginning of the file and enjoy using the C interface of
-- SymEngine via inline-c quasiquotes.
importSymengine :: DecsQ
importSymengine =
  concat
    <$> sequence
      [ C.context symengineCxt
      , C.include "<symengine/basic.h>"
      , C.include "<symengine/eval.h>"
      , C.include "<symengine/printers.h>"
      , C.include "<symengine/parser.h>"
      , C.include "<symengine/add.h>"
      , C.include "<symengine/mul.h>"
      , C.include "<symengine/pow.h>"
      , C.include "<symengine/matrix.h>"
      , C.include "<symengine/matrices/matrix_expr.h>"
      , C.include "<symengine/matrices/identity_matrix.h>"
      , C.include "<symengine/matrices/zero_matrix.h>"
      , defineCxxUtils
      ]

symengineCxt :: C.Context
symengineCxt =
  C.funCtx <> C.fptrCtx <> C.bsCtx <> Cpp.cppCtx <> C.baseCtx <> mempty {ctxTypesTable = symengineTypePairs}

symengineTypePairs :: Map TypeSpecifier TypeQ
symengineTypePairs =
  fromList
    [ (TypeName "Object", [t|CxxBasic|])
    , (TypeName "Vector", [t|Vector Basic|])
    , (TypeName "DenseMatrix", [t|DenseMatrix Basic|])
    , (TypeName "integer_class", [t|CxxInteger|])
    , (TypeName "std::string", [t|CxxString|])
    ]

defineCxxUtils :: DecsQ
defineCxxUtils =
  C.verbatim
    "\
    \using Object = SymEngine::RCP<const SymEngine::Basic>;                    \n\
    \using Vector = SymEngine::vec_basic;                                      \n\
    \using namespace SymEngine;                                                \n\
    \                                                                          \n\
    \#define CONSTRUCT_BASIC(dest, expr) new (dest) Object{expr}               \n\
    \                                                                          \n\
    \"

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
