module Main where

import           Control.Monad
import           Data.Char
import           System.Process
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

newtype Arg = Arg String deriving (Show, Eq)

instance Arbitrary Arg where
  arbitrary = liftM Arg $ listOf1 validChars
    where validChars = chr <$> choose (1,255)

unarg :: Arg -> String
unarg (Arg x) = x

prop_args :: [Arg] -> Property
prop_args args = monadicIO $ do
  let fargs = ["-", "--", "dumpargs"] ++ map unarg args
  out <- run $ readProcess "fsatrace" fargs ""
  let l0 = head $ lines out
  assert $ args == (Arg <$> read l0)

main :: IO ()
main = quickCheckWith stdArgs{ maxSuccess = 100 } prop_args
