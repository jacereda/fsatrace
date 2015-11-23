module Main where

import           System.Environment

main :: IO ()
main = getArgs >>= print
