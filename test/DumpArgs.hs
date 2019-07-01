
-- | Dump the arguments returned by Haskell
module DumpArgs(main) where

import System.Environment

main :: IO ()
main = getArgs >>= print
