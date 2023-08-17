{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Symengine.Context
-- Description : Helpers to setup inline-c for Symengine
-- Copyright   : (c) Tom Westerhout, 2023
--
-- This module defines a Template Haskell function 'importSymengine' that sets up everything you need
-- to call SymEngine functions from 'Language.C.Inline' quasiquotes.
module Symengine.Context
  ( importSymengine
  )
where

import Data.Map.Strict qualified as Map
import Language.C.Inline qualified as C
import Language.C.Inline.Context (Context (ctxTypesTable))
import Language.C.Inline.Cpp qualified as Cpp
import Language.C.Types (CIdentifier, TypeSpecifier (..))
import Language.Haskell.TH (DecsQ, Q, TypeQ, lookupTypeName)
import Language.Haskell.TH.Syntax (Type (..))

-- | One stop function to include all the neccessary machinery to call SymEngine functions via
-- inline-c.
--
-- Put @importSymengine@ somewhere at the beginning of the file and enjoy using the C interface of
-- SymEngine via inline-c quasiquotes.
importSymengine :: DecsQ
importSymengine =
  concat
    <$> sequence
      [ C.context =<< symengineCxt
      , C.include "<symengine/basic.h>"
      , C.include "<symengine/eval.h>"
      , C.include "<symengine/printers.h>"
      , C.include "<symengine/parser.h>"
      , C.include "<symengine/add.h>"
      , C.include "<symengine/mul.h>"
      , C.include "<symengine/pow.h>"
      , C.include "<symengine/subs.h>"
      , C.include "<symengine/matrix.h>"
      , C.include "<symengine/matrices/matrix_expr.h>"
      , C.include "<symengine/matrices/identity_matrix.h>"
      , C.include "<symengine/matrices/zero_matrix.h>"
      , defineCxxUtils
      ]

symengineCxt :: Q C.Context
symengineCxt = do
  typePairs <- Map.fromList <$> symengineTypePairs
  pure $
    C.funCtx <> C.fptrCtx <> C.bsCtx <> Cpp.cppCtx <> C.baseCtx <> mempty {ctxTypesTable = typePairs}

symengineTypePairs :: Q [(TypeSpecifier, TypeQ)]
symengineTypePairs =
  optionals
    [ ("Object", "CxxBasic")
    , ("Vector", "Vector Basic")
    , ("DenseMatrix", "DenseMatrix Basic")
    , ("integer_class", "CxxInteger")
    , ("std::string", "CxxString")
    , ("map_basic_basic", "CxxMapBasicBasic")
    ]
  where
    optional :: (CIdentifier, String) -> Q [(TypeSpecifier, TypeQ)]
    optional (cName, hsName) = do
      hsType <- case words hsName of
        [x] -> fmap ConT <$> lookupTypeName x
        -- TODO: generalize to multiple arguments
        [f, x] -> do
          con <- fmap ConT <$> lookupTypeName f
          arg <- fmap ConT <$> lookupTypeName x
          pure $ AppT <$> con <*> arg
        _ -> pure Nothing
      pure $ maybe [] (\x -> [(TypeName cName, pure x)]) hsType
    optionals :: [(CIdentifier, String)] -> Q [(TypeSpecifier, TypeQ)]
    optionals pairs = concat <$> mapM optional pairs

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
