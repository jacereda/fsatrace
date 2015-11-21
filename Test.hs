module Main where

import Data.Char
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import System.Process

newtype Arg = Arg String deriving (Show, Eq)

instance Arbitrary Arg where
  arbitrary = liftM Arg $ listOf1 validChars
    where validChars = chr <$> choose (32,255)

unarg :: Arg -> String
unarg (Arg x) = x

prop_args :: [Arg] -> Property
prop_args args = monadicIO $ do
  let fargs = ["-", "--", "dumpargs"] ++ map unarg args
  out <- run $ readProcess "fsatrace" fargs ""
  let l0 = head $ lines out
  assert $ args == (Arg <$> read l0)

main = quickCheck prop_args
