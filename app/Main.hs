module Main where

import Lib

main :: IO ()
main = do
 str <- ascii_art_str
 putStr str
