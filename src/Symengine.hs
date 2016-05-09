{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Symengine
Description : Symengine bindings to Haskell
-}
module Symengine
    (
     ascii_art_str,
     ascii_art_str_raw,
     basic_str,
     basic_str_raw,
     basic_const_zero,
     basic_const_zero_raw,
     BasicExternal,
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Control.Applicative


data BasicStruct = BasicStruct {
    data_ptr :: Ptr ()
}

instance Storable BasicStruct where
    alignment _ = 8
    sizeOf _ = sizeOf nullPtr
    peek basic_ptr = BasicStruct <$> peekByteOff basic_ptr 0
    poke basic_ptr BasicStruct{..} = pokeByteOff basic_ptr 0 data_ptr


-- |a smart pointer of a size 1 array of BasicStruct which has a finalizer attached
type BasicExternal = ForeignPtr BasicStruct
-- |a raw pointer of size 1 of type `BasicStruct` which does not have a finalizer attached
type BasicInternal = Ptr BasicStruct

-- |construct a 0
basic_const_zero :: IO BasicExternal
basic_const_zero = do
    basic <- create_basic
    withForeignPtr basic basic_const_zero_raw
    return basic

-- |construct an empty string
basic_str :: BasicExternal -> IO String
basic_str basic_external = withForeignPtr basic_external (\p -> basic_str_raw p >>= peekCString)

-- |The `ascii_art_str` function prints SymEngine in ASCII art.
-- this is useful as a sanity check
ascii_art_str :: IO String
ascii_art_str = ascii_art_str_raw >>= peekCString

-- Unexported raw functions------------------------

-- |Create a basic object that represents all other objects through
-- the FFI
create_basic :: IO BasicExternal
create_basic = do
    basic_ptr <- newArray [BasicStruct { data_ptr = nullPtr }]
    basic_new_heap_raw basic_ptr
    finalized_ptr <- newForeignPtr ptr_basic_free_heap_raw basic_ptr
    return finalized_ptr



foreign import ccall "symengine/cwrapper.h ascii_art_str" ascii_art_str_raw :: IO CString

foreign import ccall "symengine/cwrapper.h basic_new_heap" basic_new_heap_raw :: BasicInternal -> IO ()
foreign import ccall "symengine/cwrapper.h &basic_free_heap" ptr_basic_free_heap_raw :: FunPtr(BasicInternal -> IO ())


foreign import ccall "/cwrapper.h basic_const_zero" basic_const_zero_raw :: BasicInternal -> IO ()
foreign import ccall "symengine/cwrapper.h basic_str" basic_str_raw :: BasicInternal -> IO CString
