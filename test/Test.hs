module Main where

import           Control.Monad
import           Control.Monad.Reader
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

data Env = Env { shellMode :: ShellMode
               , traceMode :: TraceMode
               , spaceMode :: SpaceMode
               , tmpDir :: FilePath
               }

type Prop = Reader Env Property

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

chkargs :: [Arg] -> IO String -> Property
chkargs args kout = monadicIO $ do
  out <- run kout
  assert $ args == (Arg <$> read (head $ lines out))

prop_rawargs :: [Arg] -> Property
prop_rawargs args = chkargs args $ readProcess "dumpargs" (map unarg args) ""

prop_args :: [Arg] -> Property
prop_args args = chkargs args $ outputFrom $ fsatrace "x" ++ "dumpargs" : map unarg args

outputFrom :: [String] -> IO String
outputFrom (cmd:args) = do
  (_,out,err) <- readProcessWithExitCode cmd args ""
  when (err /= "") $ putStrLn err
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
parsedOutputFrom x = outputFrom x >>= return . parse

toStandard :: FilePath -> FilePath
toStandard = if isWindows then map (\x -> if x == '\\' then '/' else x) else id

parseDeps :: String -> [FilePath]
parseDeps = filter (/= " ") . map unhack . words . hack . drop 1 . dropWhile (/= ':')
  where hack ('\\':_:xs) = '^':hack xs
        hack (x:xs) = x:hack xs
        hack [] = []
        unhack = map (\x -> if x == '^' then ' ' else x)

parseClDeps :: String -> [FilePath]
parseClDeps = mapMaybe parseLine . lines
  where parseLine ('N':xs) = Just $ dropWhile (== ' ') $ skip ':' $ skip ':' xs
        parseLine _ = Nothing
        skip c = drop 1 . dropWhile (/= c)

yields :: Reader Env [String] -> Reader Env [Access] -> Prop
yields eargs eres = do
  e <- ask
  return $ monadicIO $ do
    let args = runReader eargs e
        res = runReader eres e
    r <- run $ parsedOutputFrom args
    let sr = nub $ sort $ filter (valid $ tmpDir e) r
        ok = sr == res
    unless ok $ run $ do
      putStrLn $ "Expecting " ++ show res
      putStrLn $ "Got       " ++ show sr
    assert ok

data ShellMode = Unshelled | Shelled deriving (Show, Eq, Enum, Bounded)
data TraceMode = Untraced | Traced deriving (Show, Eq, Enum, Bounded)
data SpaceMode = Unspaced | Spaced deriving (Show, Eq, Enum, Bounded)

isShelled :: Reader Env Bool
isShelled = do
  sm <- ask
  return $ shellMode sm == Shelled

quoted :: String -> String
quoted x = "\"" ++ x ++ "\""

command :: String -> [String] -> Reader Env [String]
command flags args = do
  e <- ask
  return $ cmd (shellMode e) (traceMode e)
  where cmd :: ShellMode -> TraceMode -> [String]
        cmd sm Traced = fsatrace flags ++ cmd sm Untraced 
        cmd Unshelled _ = args
        cmd Shelled _ | isWindows = "cmd.exe" : "/c" : args
                      | otherwise = ["sh", "-c", unwords (map quoted args)]

whenTracing :: [a] -> Reader Env [a]
whenTracing x = do
  e <- ask
  return $ if traceMode e == Traced then x else []

prop_echo :: Path -> Prop
prop_echo src = command "rwmd" ["echo", unpath src] `yields` return []

prop_cp :: Path -> Path -> Prop
prop_cp src dst = do
  cmd <- cp
  command "rwmd" [cmd, unpath src, unpath dst] `yields` whenTracing [R src, W dst]
  where cp :: Reader Env String
        cp = do
          s <- isShelled
          return $ if isWindows && s then "copy" else "cp"

prop_mv :: Path -> Path -> Prop
prop_mv src dst = command "rwmd" ["mv", unpath src, unpath dst] `yields` whenTracing [M dst src]

prop_touch :: Path -> Prop
prop_touch dst = command "t" ["touch", unpath dst] `yields` whenTracing [T dst]

prop_rm :: Path -> Prop
prop_rm dst = do
  cmd <- rm
  command "rwmd" [cmd, unpath dst] `yields` whenTracing [D dst]
  where rm :: Reader Env String
        rm = do
          s <- isShelled
          return $ if isWindows && s then "del" else "rm"

prop_gcc :: Path -> [Access] -> Prop
prop_gcc src deps = command "r" ["gcc", "-E", unpath src] `yields` whenTracing deps

prop_cl :: Path -> [Access] -> Prop 
prop_cl src deps = command "r" ["cl", "/nologo", "/E", unpath src] `yields` whenTracing deps

shelled :: [String] -> [String]
shelled args | isWindows = "cmd.exe" : "/c" : args
             | otherwise = ["sh", "-c", unwords args]

main :: IO ()
main = do
  sequence [allTests sp sm tm | sp <- allValues, sm <- allValues, tm <- allValues]
       >>= mapM_ (mapM_ chk)
  where chk x = unless (isSuccess x) exitFailure
        noisy s = putStrLn ("Testing " ++ s)
        banner x = putStrLn $ "================ " ++ x ++ " ================"
        dirname Unspaced = "fsatrace"
        dirname Spaced = "fsatrace with spaces"
        allValues :: (Enum a, Bounded a) => [a]
        allValues = enumFrom minBound
        allTests :: SpaceMode -> ShellMode -> TraceMode -> IO [Result]
        allTests sp sm tm = withSystemTempDirectory (dirname sp) $ \utmp -> do
          t <- canonicalizePath utmp
          banner $ show sp ++ " " ++ show sm ++ " " ++ show tm
          src <- canonicalizePath $ ".." </> "src"
          cl <- findExecutable "cl.exe"
          let hascl = isJust cl
              tsrc = t </> "src"
              emitc = Path $ tsrc </> "emit.c"
              srcc = Path $ tsrc </> "src.c"
              clcsrc = Path $ tsrc </> "win" </> "handle.c"
              rvalid = sort . filter (valid t) . map (R . Path)
              e = Env {shellMode = sm, traceMode = tm, spaceMode = sp, tmpDir = t}
              qc1 s p = noisy s >> quickCheckWithResult (stdArgs {maxSuccess=1}) (runReader p e)
              qc n s p = noisy s >> quickCheckWithResult (stdArgs {maxSuccess=n}) p
          _ <- outputFrom ["cp", "-R", src, tsrc]
          deps <- outputFrom ["gcc", "-MM", unpath emitc]
          ndeps <- mapM canonicalizePath (parseDeps deps)
          cldeps <- if hascl then errorFrom ["cl", "/nologo", "/showIncludes", "/E", "/DPATH_MAX=4096", unpath clcsrc] else return []
          ncldeps <- if hascl then mapM canonicalizePath (unpath clcsrc : parseClDeps cldeps) else return []
          sequence $
            [ qc 10 "rawargs" prop_rawargs
            , qc 10 "args" prop_args
            , qc1 "echo" $ prop_echo emitc
            , qc1 "cp" $ prop_cp emitc srcc
            , qc1 "touch" $ prop_touch srcc
            , qc1 "gcc" $ prop_gcc emitc (rvalid ndeps)
            , qc1 "mv" $ prop_mv emitc srcc
            , qc1 "rm" $ prop_rm srcc
            ] ++ if hascl then [ qc1 "cl" $ prop_cl clcsrc (rvalid ncldeps) ] else []

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

cased :: String -> String
cased | isWindows = map toLower
      | otherwise = id

cd :: FilePath
cd = unsafePerformIO (getCurrentDirectory >>= canonicalizePath)

valid :: FilePath -> Access -> Bool
valid t (R p) = inTmp t p
valid t (Q p) = inTmp t p
valid _ (W p) = not $ "/dev/" `isPrefixOf` (unpath p)
valid _ _ = True

inTmp :: FilePath -> Path -> Bool
inTmp t = isPrefixOf (cased t) . cased . unpath
