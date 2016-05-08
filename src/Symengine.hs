{-# LANGUAGE RecordWildCards #-}

module Symengine
    (
     ascii_art_str,
     basic_str,
     basic_const_zero,
     basic_const_one,
     basic_const_minus_one,
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
basic_const_zero = basic_obj_constructor basic_const_zero_ffi


basic_const_one :: IO BasicPtr
basic_const_one = basic_obj_constructor basic_const_one_ffi

basic_const_minus_one :: IO BasicPtr
basic_const_minus_one = basic_obj_constructor basic_const_minus_one_ffi

basic_const_I :: IO BasicPtr
basic_const_I = basic_obj_constructor basic_const_I_ffi


basic_obj_constructor :: (Ptr BasicStruct -> IO ()) -> IO BasicPtr
basic_obj_constructor init_fn = do
    basic_ptr <- create_basic_ptr
    withForeignPtr (fptr basic_ptr) init_fn
    return $ basic_ptr

basic_str :: BasicPtr -> String
basic_str basic_ptr = unsafePerformIO $ withForeignPtr (fptr basic_ptr) (\p -> basic_str_ffi p >>= peekCString)

-- |The `ascii_art_str` function prints SymEngine in ASCII art.
-- this is useful as a sanity check
ascii_art_str :: IO String
ascii_art_str = ascii_art_str_ffi >>= peekCString

-- Unexported ffi functions------------------------

-- |Create a basic object that represents all other objects through
-- the FFI
create_basic_ptr :: IO BasicPtr
create_basic_ptr = do
    basic_ptr <- newArray [BasicStruct { data_ptr = nullPtr }]
    basic_new_heap_ffi basic_ptr
    finalized_ptr <- newForeignPtr ptr_basic_free_heap_ffi basic_ptr
    return $ BasicPtr { fptr = finalized_ptr }



foreign import ccall "symengine/cwrapper.h ascii_art_str" ascii_art_str_ffi :: IO CString
foreign import ccall "symengine/cwrapper.h basic_new_heap" basic_new_heap_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h &basic_free_heap" ptr_basic_free_heap_ffi :: FunPtr(Ptr BasicStruct -> IO ())
foreign import ccall "symengine/cwrapper.h basic_const_zero" basic_const_zero_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_one" basic_const_one_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_minus_one" basic_const_minus_one_ffi :: Ptr BasicStruct -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_I" basic_const_I_ffi :: Ptr BasicStruct -> IO ()

foreign import ccall "symengine/cwrapper.h basic_str" basic_str_ffi :: Ptr BasicStruct -> IO CString
