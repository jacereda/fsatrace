module Main where

import           Control.Monad
import           Data.List
import           Data.Char
import           Data.Maybe
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Info
import           System.IO.Temp
import           System.IO.Unsafe
import           System.Process
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Test
--import           Debug.Trace

newtype Arg = Arg { unarg :: String } deriving (Show, Eq)

instance Arbitrary Arg where
  arbitrary = liftM Arg $ listOf1 validChars
    where validChars = arbitrary `suchThat` (`notElem` "\0")

newtype Path = Path { unpath :: FilePath }

instance Eq Path where
  (==) (Path a) (Path b) = equalFilePath a b

instance Show Path where
  show (Path p) = show p

instance Ord Path where
  compare (Path x) (Path y) = compare (cased x) (cased y)


isWindows :: Bool
isWindows = os == "mingw32"

chkargs :: [Arg] -> String -> PropertyM IO ()
chkargs args out = assert $ args == (Arg <$> read (head $ lines out))

prop_rawargs :: [Arg] -> Property
prop_rawargs args = monadicIO $ run (readProcess "dumpargs" (map unarg args) "") >>= chkargs args

prop_args :: [Arg] -> Property
prop_args args = monadicIO $ run (outputFrom $ fsatrace "x" ++ "dumpargs" : map unarg args) >>= chkargs args

outputFrom :: [String] -> IO String
outputFrom (cmd:args) = do
  (_,out,_) <- readProcessWithExitCode cmd args ""
  return out
outputFrom _ = undefined

errorFrom :: [String] -> IO String
errorFrom (cmd:args) = do
  (_,_,err) <- readProcessWithExitCode cmd args ""
  return err
errorFrom _ = undefined


fsatrace :: String -> [String]
fsatrace flags = [cd </> ".." </> "fsatrace", flags, "-", "--"]

parsedOutputFrom :: [String] -> IO [Access]
parsedOutputFrom x = outputFrom x >>= return . filter valid . parse

toStandard :: FilePath -> FilePath
toStandard = if isWindows then map (\x -> if x == '\\' then '/' else x) else id

parseDeps :: String -> [FilePath]
parseDeps = filter (/= "\\") . words . drop 1 . dropWhile (/= ':')

parseClDeps :: String -> [FilePath]
parseClDeps = mapMaybe parseLine . lines
  where parseLine ('N':xs) = Just $ dropWhile (== ' ') $ skip ':' $ skip ':' xs
        parseLine _ = Nothing
        skip c = drop 1 . dropWhile (/= c)

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
cp Shelled | isWindows = "copy"
cp _ = "cp"

rm :: ShellMode -> String
rm Shelled | isWindows = "del"
rm _ = "rm"

quoted :: String -> String
quoted x = "\"" ++ x ++ "\""


command :: ShellMode -> TraceMode -> String -> [String] -> [String]
command sm Traced flags args = fsatrace flags ++ command sm Untraced flags args
command Unshelled _ _ args = args
command Shelled _ _ args | isWindows = "cmd.exe" : "/c" : args
                         | otherwise = ["sh", "-c", unwords (map quoted args)]

whenTracing :: TraceMode -> [a] -> [a]
whenTracing Traced x = x
whenTracing _ _ = []

prop_echo :: ShellMode -> TraceMode -> Path -> Property
prop_echo sm tm src = command sm tm "rwmd" ["echo", unpath src] `yields` []

prop_cp :: ShellMode -> TraceMode -> Path -> Path -> Property
prop_cp sm tm src dst = command sm tm "rwmd" [cp sm, unpath src, unpath dst] `yields` whenTracing tm [R src, W dst]

prop_mv :: ShellMode -> TraceMode -> Path -> Path -> Property
prop_mv sm tm src dst = command sm tm "rwmd" ["mv", unpath src, unpath dst] `yields` whenTracing tm [M dst src]

prop_touch :: ShellMode -> TraceMode -> Path -> Property
prop_touch sm tm dst = command sm tm "t" ["touch", unpath dst] `yields` whenTracing tm [T dst]

prop_rm :: ShellMode -> TraceMode -> Path -> Property
prop_rm sm tm dst = command sm tm "rwmd" [rm sm, unpath dst] `yields` whenTracing tm [D dst]

prop_gcc :: ShellMode -> TraceMode -> Path -> [Access] -> Property
prop_gcc sm tm src deps = command sm tm "r" ["gcc", "-E", unpath src] `yields` whenTracing tm deps

prop_cl :: ShellMode -> TraceMode -> Path -> [Access] -> Property
prop_cl sm tm src deps = command sm tm "r" ["cl", "/nologo", "/E", unpath src] `yields` whenTracing tm deps

shelled :: [String] -> [String]
shelled args | isWindows = "cmd.exe" : "/c" : args
             | otherwise = ["sh", "-c", unwords args]

main :: IO ()
main = sequence [allTests sp sm tm | sp <- allValues, sm <- allValues, tm <- allValues]
       >>= mapM_ (mapM_ chk)
  where qc n s p = noisy s >> quickCheckWithResult stdArgs {maxSuccess=n} p
        chk x = unless (isSuccess x) exitFailure
        noisy s = putStrLn ("Testing " ++ s)
        banner x = putStrLn $ "================ " ++ x ++ " ================"
        dirname Unspaced = "fsatrace"
        dirname Spaced = "fsatrace with spaces"
        allValues :: (Enum a, Bounded a) => [a]
        allValues = enumFrom minBound
        allTests :: SpaceMode -> ShellMode -> TraceMode -> IO [Result]
        allTests sp sm tm = withSystemTempDirectory (dirname sp) $ \tmp -> do
          banner $ show sp ++ " " ++ show sm ++ " " ++ show tm
          lic <- canonicalizePath $ ".." </> "LICENSE"
          ctmp <- canonicalizePath tmp
          csrc <- canonicalizePath $ ".." </> "src" </> "emit.c"
          deps <- outputFrom ["gcc", "-MM", csrc]
          ndeps <- mapM canonicalizePath (parseDeps deps)
          cl <- findExecutable "cl"
          let hascl = isJust cl
          clcsrc <- if hascl then canonicalizePath $ ".." </> "src" </> "win" </> "handle.c" else return ""
          cldeps <- if hascl then errorFrom ["cl", "/nologo", "/showIncludes", "/E", "/DPATH_MAX=4096", clcsrc] else return []
          ncldeps <- mapM canonicalizePath (clcsrc : parseClDeps cldeps)
          let tls = Path $ ctmp </> "LICENSE"
              tfoo = Path $ ctmp </> "foo"
              rvalid = sort . filter valid . map (R . Path)
          sequence $
            [ qc 10 "rawargs" prop_rawargs
            , qc 10 "args" prop_args
            , qc 1 "echo" $ prop_echo sm tm tls
            , qc 1 "cp" $ prop_cp sm tm (Path lic) tls
            , qc 1 "mv" $ prop_mv sm tm tls tfoo
            , qc 1 "touch" $ prop_touch sm tm tfoo
            , qc 1 "rm" $ prop_rm sm tm tfoo
            , qc 1 "gcc" $ prop_gcc sm tm (Path csrc) (rvalid ndeps)
            ] ++ if hascl then [ qc 1 "cl" $ prop_cl sm tm (Path clcsrc) (rvalid ncldeps) ] else []

data Access = R Path
            | W Path
            | D Path
            | Q Path
            | T Path
            | M Path Path
            | RR Path
            | RW Path
            | RD Path
            | RQ Path
            | RT Path
            | RM Path Path
            deriving (Show, Eq, Ord)

parse :: String -> [Access]
parse = mapMaybe f . lines
    where f ('w':'|':xs) = Just $ W $ Path xs
          f ('r':'|':xs) = Just $ R $ Path xs
          f ('d':'|':xs) = Just $ D $ Path xs
          f ('q':'|':xs) = Just $ Q $ Path xs
          f ('t':'|':xs) = Just $ T $ Path xs
          f ('m':'|':xs) | (xs','|':ys) <- break (== '|') xs = Just $ M (Path xs') (Path ys)
          f ('W':'|':xs) = Just $ RW $ Path xs
          f ('R':'|':xs) = Just $ RR $ Path xs
          f ('D':'|':xs) = Just $ RD $ Path xs
          f ('Q':'|':xs) = Just $ RQ $ Path xs
          f ('T':'|':xs) = Just $ RT $ Path xs
          f ('M':'|':xs) | (xs','|':ys) <- break (== '|') xs = Just $ RM (Path xs') (Path ys)
          f _ = Nothing

valid :: Access -> Bool
valid (R p) = inParent p
valid (Q p) = inParent p
valid (W p) = not $ "/dev/" `isPrefixOf` (unpath p)
valid _ = True

inParent :: Path -> Bool
inParent = isPrefixOf (takeDirectory $ cased cd) . cased . unpath

cased :: String -> String
cased | isWindows = map toLower
      | otherwise = id

cd :: FilePath
cd = unsafePerformIO $ (getCurrentDirectory >>= canonicalizePath)
