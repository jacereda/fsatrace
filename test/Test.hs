module Main where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

newtype Arg = Arg { unarg :: String } deriving (Show, Eq)

instance Arbitrary Arg where
  arbitrary = liftM Arg $ listOf1 validChars
    where validChars = chr <$> choose (1,255)

prop_args :: [Arg] -> Property
prop_args args = monadicIO $ do
  out <- run $ outputFrom $ "" : "dumpargs" : map unarg args
  let l0 = head $ lines out
  assert $ args == (Arg <$> read l0)

outputFrom :: [String] -> IO String
outputFrom args = do
  cd <- getCurrentDirectory
  readProcess (cd </> ".." </> "fsatrace") ([head args, "-", "--"] ++ tail args) ""

yields :: [String] -> [Access] -> Property
yields args res = monadicIO $ do
  r <- run $ outputFrom (traceShowId args)
  assert $ (traceShowId (filter valid $ parse r)) == (traceShowId res)
  where valid (W x) | "/dev/" `isPrefixOf` x = False
        valid _ = True


prop_cp :: FilePath -> FilePath -> Property
prop_cp src dst = ["rw", "cp", src, dst] `yields` [R src, W dst]

prop_mv :: FilePath -> FilePath -> Property
prop_mv src dst = ["m", "mv", src, dst] `yields` [M dst src]

prop_touch :: FilePath -> Property
prop_touch dst = ["wt", "touch", dst] `yields` [T dst]

prop_rm :: FilePath -> Property
prop_rm dst = ["d", "rm", dst] `yields` [D dst]

prop_shcp :: FilePath -> FilePath -> Property
prop_shcp src dst = sh ["rw", "cp", src, dst] `yields` [R src, W dst]

prop_shmv :: FilePath -> FilePath -> Property
prop_shmv src dst = sh ["m", "mv", src, dst] `yields` [M dst src]

prop_shtouch :: FilePath -> Property
prop_shtouch dst = sh ["wt", "touch", dst] `yields` [T dst]

prop_shrm :: FilePath -> Property
prop_shrm dst = sh ["d", "rm", dst] `yields` [D dst]

sh :: [String] -> [String]
sh (flags:rest) = flags:"sh":"-c":intercalate " " rest:[]
sh _ = undefined

main :: IO ()
main = do
  quickCheck prop_args
  withSystemTempDirectory "fsatrace" $ \tmp -> do
    ctmp <- canonicalizePath tmp
    let qc1 = quickCheckWith stdArgs {maxSuccess=1}
        tls = ctmp </> "LICENSE"
        tfoo = ctmp </> "foo"
    
    qc1 $ prop_cp (".." </> "LICENSE") tls
    qc1 $ prop_mv tls tfoo
    qc1 $ prop_touch tfoo
    qc1 $ prop_rm tfoo

    qc1 $ prop_shcp (".." </> "LICENSE") tls
    qc1 $ prop_shmv tls tfoo
    qc1 $ prop_shtouch tfoo
    qc1 $ prop_shrm tfoo


data Access
    = W FilePath
    | R FilePath
    | D FilePath
    | Q FilePath
    | T FilePath
    | M FilePath FilePath
    deriving (Show, Eq)

parse :: String -> [Access]
parse = mapMaybe f . lines
    where f ('w':'|':xs) = Just $ W xs
          f ('r':'|':xs) = Just $ R xs
          f ('d':'|':xs) = Just $ D xs
          f ('q':'|':xs) = Just $ Q xs
          f ('t':'|':xs) = Just $ T xs
          f ('m':'|':xs) | (xs','|':ys) <- break (== '|') xs = Just $ M xs' ys
          f _ = Nothing


