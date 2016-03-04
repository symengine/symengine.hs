module Main where

import Lib

main :: IO ()
main = do
 str <- ascii_art_str
 putStr str
 zero <- basic_const_zero

 zero_str <- basic_str zero
 putStr $ "\nbasic string: " ++ zero_str 
 return ()