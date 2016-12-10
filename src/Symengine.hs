{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Module      : Symengine
Description : Symengine bindings to Haskell
-}
module Symengine
  (module Symengine.Internal
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

import Symengine.Internal
