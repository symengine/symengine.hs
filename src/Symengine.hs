{-# LANGUAGE RecordWildCards #-}

module Symengine
    (
     ascii_art_str,
     basic_str,
     basic_const_zero,
     BasicPtr,
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import Control.Applicative
import System.IO.Unsafe


data BasicStruct = BasicStruct {
    data_ptr :: Ptr ()
}

instance Storable BasicStruct where
    alignment _ = 8
    sizeOf _ = sizeOf nullPtr
    peek basic_ptr = BasicStruct <$> peekByteOff basic_ptr 0
    poke basic_ptr BasicStruct{..} = pokeByteOff basic_ptr 0 data_ptr

data BasicPtr = BasicPtr { fptr :: ForeignPtr BasicStruct }

instance Show BasicPtr where
    show = basic_str 


basic_const_zero :: IO BasicPtr
basic_const_zero = do
    basic_ptr <- create_basic_ptr
    withForeignPtr (fptr basic_ptr) basic_const_zero_raw
    return $ basic_ptr

basic_str :: BasicPtr -> String
basic_str basic_ptr = unsafePerformIO $ withForeignPtr (fptr basic_ptr) (\p -> basic_str_raw p >>= peekCString)

-- |The `ascii_art_str` function prints SymEngine in ASCII art.
-- this is useful as a sanity check
ascii_art_str :: IO String
ascii_art_str = ascii_art_str_raw >>= peekCString

-- Unexported raw functions------------------------

-- |Create a basic object that represents all other objects through
-- the FFI
create_basic_ptr :: IO BasicPtr
create_basic_ptr = do
    basic_ptr <- newArray [BasicStruct { data_ptr = nullPtr }]
    basic_new_heap_raw basic_ptr
    finalized_ptr <- newForeignPtr ptr_basic_free_heap_raw basic_ptr
    return $ BasicPtr { fptr = finalized_ptr }



foreign import ccall "symengine/cwrapper.h ascii_art_str" ascii_art_str_raw :: IO CString
foreign import ccall "symengine/cwrapper.h basic_new_heap" basic_new_heap_raw :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h &basic_free_heap" ptr_basic_free_heap_raw :: FunPtr(Ptr BasicStruct -> IO ())
foreign import ccall "symengine/cwrapper.h basic_const_zero" basic_const_zero_raw :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_str" basic_str_raw :: Ptr BasicStruct -> IO CString
