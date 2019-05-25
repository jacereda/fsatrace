
-- | A test of the FSATrace program
module Test(main) where

import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.List.Extra
import           Data.Char
import           Data.Maybe
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Info.Extra
import           System.IO.Temp
import           System.IO.Unsafe
import           System.Process
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
--import           Debug.Trace

data Env = Env
    { shellMode :: ShellMode
    , traceMode :: TraceMode
    , spaceMode :: SpaceMode
    , tmpDir :: FilePath
    }

type Prop = Reader Env Property

newtype Arg = Arg { unarg :: String } deriving (Show, Eq)

instance Arbitrary Arg where
  arbitrary = Arg <$> listOf1 validChars
    where validChars = arbitrary `suchThat` (`notElem` "\0")

newtype Path = Path { unpath :: FilePath }

instance Eq Path where
  (==) (Path a) (Path b) = equalFilePath a b

instance Show Path where
  show (Path p) = show p

instance Ord Path where
  compare (Path x) (Path y) = compare (cased x) (cased y)


prop_args :: [Arg] -> Prop
prop_args args = do
  c <- command "x" $ "dumpargs" : map unarg args
  return $ monadicIO $ do
    mout <- run $ outputFrom c
    assert $ case mout of
              Just out -> args == (Arg <$> read (head $ lines out))
              Nothing -> False

outputFrom :: [String] -> IO (Maybe String)
outputFrom (cmd:args) = do
  (rc,out,err) <- readProcessWithExitCode cmd args ""
  when (err /= "") $ putStrLn err
  return $ if rc == ExitSuccess then Just out else Nothing
outputFrom _ = undefined

errorFrom :: [String] -> IO String
errorFrom (cmd:args) = do
  (_,_,err) <- readProcessWithExitCode cmd args ""
  return err
errorFrom _ = undefined


fsatrace :: String -> [String]
fsatrace flags = [pwd </> ".." </> "fsatrace", flags, "-", "--"]

parsedOutputFrom :: [String] -> IO (Maybe [Access])
parsedOutputFrom x = do
  mout <- outputFrom x
  return $ case mout of
                Just out -> Just $ parse out
                Nothing -> Nothing

parseDeps :: Maybe String -> [FilePath]
parseDeps = filter (/= " ") . map unhack . words . hack . drop 1 . dropWhile (/= ':') . fromMaybe ""
  where hack ('\\':' ':xs) = '^':hack xs
        hack ('\\':'\n':xs) = ' ':hack xs
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
    let sr | isJust r = Just $ nubSort $ filter (valid $ tmpDir e) $ fromJust r
           | otherwise = Nothing
        ok = sr == Just res
    unless ok $ run $ do
      putStrLn $ "Expecting " ++ show res
      putStrLn $ "Got       " ++ show sr
    assert ok

data ShellMode = Unshelled | Shelled deriving (Show, Eq, Enum, Bounded)
data TraceMode = Traced deriving (Show, Eq, Enum, Bounded)
data SpaceMode = Unspaced | Spaced deriving (Show, Eq, Enum, Bounded)


command :: String -> [String] -> Reader Env [String]
command flags args = do
  e <- ask
  return $ fsatrace flags ++ cmd (shellMode e) (traceMode e)
  where cmd :: ShellMode -> TraceMode -> [String]
        cmd Unshelled _ = args
        cmd Shelled _ | isWindows = "cmd.exe" : "/C" : args
                      | otherwise = ["sh", "-c", unwords (map quoted args)]
        quoted :: String -> String
        quoted "|" = "|"
        quoted ">" = ">"
        quoted x = "\"" ++ x ++ "\""

whenTracing :: [a] -> Reader Env [a]
whenTracing x = do
  e <- ask
  return $ if traceMode e == Traced then x else []

prop_echo :: Path -> Path -> Prop
prop_echo src dst = command "rwmd" ["echo", unpath src, "|", "sort" , ">", unpath dst] `yields` return [W dst]

prop_cp :: Path -> Path -> Prop
prop_cp src dst = command "rwmd" ["cp", unpath src, unpath dst] `yields` whenTracing [R src, W dst]

prop_mv :: Path -> Path -> Prop
prop_mv src dst = command "rwmd" ["mv", unpath src, unpath dst] `yields` whenTracing [M dst src]

prop_touch :: Path -> Prop
prop_touch dst = command "t" ["touch", unpath dst] `yields` whenTracing [T dst]

prop_rm :: Path -> Prop
prop_rm dst = command "rwmd" ["rm", unpath dst] `yields` whenTracing [D dst]

prop_gcc :: Path -> [Access] -> Prop
prop_gcc src deps = command "r" ["gcc", "-E", unpath src] `yields` whenTracing deps

prop_cl :: Path -> [Access] -> Prop
prop_cl src deps = command "r" ["cl", "/nologo", "/E", unpath src] `yields` whenTracing deps

main :: IO ()
main = do
    results <- sequence [allTests sp sm tm | sp <- allValues, sm <- allValues, tm <- allValues]
    when (any (not . isSuccess) $ concat results) exitFailure
  where noisy s = putStrLn ("Testing " ++ s)
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
              qc s p = noisy s >> quickCheckWithResult (stdArgs {maxSuccess=1}) (runReader p e)
          _ <- outputFrom ["cp", "-R", src, tsrc]
          deps <- outputFrom ["gcc", "-MM", unpath emitc]
          ndeps <- mapM canonicalizePath (parseDeps deps)
          cldeps <- if hascl then errorFrom ["cl", "/nologo", "/showIncludes", "/E", "/DPATH_MAX=4096", unpath clcsrc] else return []
          ncldeps <- if hascl then mapM canonicalizePath (unpath clcsrc : parseClDeps cldeps) else return []
          sequence $
            [ noisy "args" >> quickCheckWithResult (stdArgs {maxSuccess=1}) (\x -> runReader (prop_args x) e) -- qc 10 "args" prop_args
            , qc "gcc" $ prop_gcc emitc (rvalid ndeps)
            , qc "cp" $ prop_cp emitc srcc
            , qc "touch" $ prop_touch srcc
            , qc "rm" $ prop_rm srcc
            , qc "mv" $ prop_mv emitc srcc
            ]
            ++ [qc "cl" $ prop_cl clcsrc (rvalid ncldeps) | hascl]
            ++ [qc "echo" $ prop_echo emitc srcc | sm == Shelled && tm == Traced]


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

pwd :: FilePath
{-# NOINLINE pwd #-}
pwd = unsafePerformIO (getCurrentDirectory >>= canonicalizePath)

valid :: FilePath -> Access -> Bool
valid t (R p) = inTmp t p
valid t (Q p) = inTmp t p
valid t (W p) | isWindows = inTmp t p
              | otherwise = not $ "/dev/" `isPrefixOf` unpath p
valid t (D p) = inTmp t p
valid t (T p) = inTmp t p
valid t (M p _) = inTmp t p
valid _ (RW _) = False -- sort on Windows produces this
valid _ _ = True

inTmp :: FilePath -> Path -> Bool
inTmp t = isPrefixOf (cased t) . cased . unpath
