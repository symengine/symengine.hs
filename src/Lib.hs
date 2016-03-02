module Lib
    ( someFunc,
     ascii_art_str,
     ascii_art_str_iod
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

someFunc :: IO ()
someFunc = putStrLn "someFunc"


ascii_art_str_iod :: IO String
ascii_art_str_iod = ascii_art_str >>= peekCString

foreign import ccall "cwrapper.h ascii_art_str" ascii_art_str :: IO CString
