module Main where

import Lib
import Text.Printf

main :: IO ()
main = do
 str <- ascii_art_str_iod
 putStr str
