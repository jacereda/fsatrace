module Main where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.IO.Unsafe
import           System.Info
import           System.Process
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
--import           Debug.Trace

newtype Arg = Arg { unarg :: String } deriving (Show, Eq)

instance Arbitrary Arg where
  arbitrary = liftM Arg $ listOf1 validChars
    where validChars = arbitrary `suchThat` (`notElem` "\0")

inWin :: Bool
inWin = os == "mingw32"

chkargs :: [Arg] -> String -> PropertyM IO ()
chkargs args out = assert $ args == (Arg <$> read (head $ lines out))

prop_rawargs :: [Arg] -> Property
prop_rawargs args = monadicIO $ run (readProcess "dumpargs" (map unarg args) "") >>= chkargs args

prop_args :: [Arg] -> Property
prop_args args = monadicIO $ run (outputFrom $ fsatrace "x" ++ "dumpargs" : map unarg args) >>= chkargs args

outputFrom :: [String] -> IO String
outputFrom (cmd:args) = readProcess cmd args ""
outputFrom _ = undefined

fsatrace :: String -> [String]
fsatrace flags = [cd </> ".." </> "fsatrace", flags, "-", "--"]
  where cd = unsafePerformIO getCurrentDirectory

parsedOutputFrom :: [String] -> IO [Access]
parsedOutputFrom x = do
  cd <- getCurrentDirectory
  let valid (R p) = inParent p
      valid (Q p) = inParent p
      valid (W p) = not $ "/dev/" `isPrefixOf` p
      valid _ = True
      inParent = isPrefixOf pdir
      pdir = takeDirectory cd
  o <- outputFrom x
  return $ filter valid $ parse o

yields :: [String] -> [Access] -> Property
yields args res = monadicIO $ do
  r <- run $ parsedOutputFrom args
  let sr = nub $ sort r
      ok = sr == res
  unless ok $ do
    run $ putStrLn $ "Expecting " ++ show res
    run $ putStrLn $ "Got       " ++ show sr
  assert ok

data ShellMode = Unshelled | Shelled deriving (Show, Enum, Bounded)
data TraceMode = Untraced | Traced deriving (Show, Enum, Bounded)
data SpaceMode = Unspaced | Spaced deriving (Show, Enum, Bounded)

cp :: ShellMode -> String
cp Shelled | inWin = "copy"
cp _ = "cp"

rm :: ShellMode -> String
rm Shelled | inWin = "del"
rm _ = "rm"

mv :: ShellMode -> String
mv _ = "mv"

touch :: ShellMode -> String
touch _ = "touch"

quoted :: String -> String
quoted x = "\"" ++ x ++ "\""


command :: ShellMode -> TraceMode -> String -> [String] -> [String]
command sm Traced flags args = fsatrace flags ++ command sm Untraced flags args
command Unshelled _ _ args = args
command Shelled _ _ args | inWin = "cmd.exe" : "/c" : args
                         | otherwise = ["sh", "-c", unwords (map quoted args)]

whenTracing :: TraceMode -> [a] -> [a]
whenTracing Traced x = x
whenTracing _ _ = []

prop_echo :: ShellMode -> TraceMode -> FilePath -> Property
prop_echo sm tm src = command sm tm "rwmd" ["echo", src] `yields` []

prop_cp :: ShellMode -> TraceMode -> FilePath -> FilePath -> Property
prop_cp sm tm src dst = command sm tm "rwmd" ["cp", src, dst] `yields` whenTracing tm [R src, W dst]

prop_mv :: ShellMode -> TraceMode -> FilePath -> FilePath -> Property
prop_mv sm tm src dst = command sm tm "rwmd" ["mv", src, dst] `yields` whenTracing tm [M dst src]

prop_touch :: ShellMode -> TraceMode -> FilePath -> Property
prop_touch sm tm dst = command sm tm "t" ["touch", dst] `yields` whenTracing tm [T dst]

prop_rm :: ShellMode -> TraceMode -> FilePath -> Property
prop_rm sm tm dst = command sm tm "rwmd" ["rm", dst] `yields` whenTracing tm [D dst]

shelled :: [String] -> [String]
shelled args | inWin = "cmd.exe" : "/c" : args
             | otherwise = ["sh", "-c", unwords args]

main :: IO ()
main = do
--  qc "rawargs" prop_rawargs
--  qc "args" prop_args

  sequence_ [allTests sp sm tm | sp <- allValues, sm <- allValues, tm <- allValues]

  where qc1 s p = noisy s >> quickCheckWith stdArgs {maxSuccess=1} p
        qc s p = noisy s >> quickCheckWith stdArgs p
        noisy s = putStrLn ("Testing " ++ s)
        banner x = putStrLn $ "================ " ++ x ++ " ================"
        dirname Unspaced = "fsatrace"
        dirname Spaced = "fsatrace with spaces"
        allValues :: (Enum a, Bounded a) => [a]
        allValues = enumFrom minBound
        allTests sp sm tm = withSystemTempDirectory (dirname sp) $ \tmp -> do
          banner $ show sp ++ " " ++ show sm ++ " " ++ show tm
          lic <- canonicalizePath $ ".." </> "LICENSE"
          ctmp <- canonicalizePath tmp
          let tls = ctmp </> "LICENSE"
              tfoo = ctmp </> "foo"
          qc1 "echo" $ prop_echo sm tm tls
          qc1 "cp" $ prop_cp sm tm lic tls
          qc1 "mv" $ prop_mv sm tm tls tfoo
          qc1 "touch" $ prop_touch sm tm tfoo
          qc1 "rm" $ prop_rm sm tm tfoo

data Access = R FilePath
            | W FilePath
            | D FilePath
            | Q FilePath
            | T FilePath
            | M FilePath FilePath
            deriving (Show, Eq, Ord)

parse :: String -> [Access]
parse = mapMaybe f . lines
    where f ('w':'|':xs) = Just $ W xs
          f ('r':'|':xs) = Just $ R xs
          f ('d':'|':xs) = Just $ D xs
          f ('q':'|':xs) = Just $ Q xs
          f ('t':'|':xs) = Just $ T xs
          f ('m':'|':xs) | (xs','|':ys) <- break (== '|') xs = Just $ M xs' ys
          f _ = Nothing

