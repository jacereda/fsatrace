
-- | A test of the FSATrace program
module Test(main) where

import           Utils
import           Parse

import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.List.Extra
import           Data.Maybe
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Info.Extra
import           System.IO.Temp
import           System.Process
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
--import           Debug.Trace

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


parsedOutputFrom :: [String] -> IO (Maybe [Access])
parsedOutputFrom x = do
  mout <- outputFrom x
  return $ case mout of
                Just out -> Just $ parse out
                Nothing -> Nothing

yields :: Reader Env [String] -> [Access] -> Prop
yields eargs res = do
  e <- ask
  return $ monadicIO $ do
    let args = runReader eargs e
    r <- run $ parsedOutputFrom args
    let sr | isJust r = Just $ nubSort $ filter (valid $ tmpDir e) $ fromJust r
           | otherwise = Nothing
        ok = sr == Just res
    unless ok $ run $ do
      putStrLn $ "Expecting " ++ show res
      putStrLn $ "Got       " ++ show sr
    assert ok


command :: String -> [String] -> Reader Env [String]
command flags args = do
  e <- ask
  return $ [pwdDir e </> ".." </> "fsatrace", flags, "-", "--"] ++ cmd (shellMode e)
  where cmd :: ShellMode -> [String]
        cmd Unshelled = args
        cmd Shelled | isWindows = "cmd.exe" : "/C" : args
                    | otherwise = ["sh", "-c", unwords (map quoted args)]
        quoted :: String -> String
        quoted "|" = "|"
        quoted ">" = ">"
        quoted x = "\"" ++ x ++ "\""

prop_echo :: Path -> Path -> Prop
prop_echo src dst = command "rwmd" ["echo", unpath src, "|", "sort" , ">", unpath dst] `yields` [W dst]

prop_cp :: Path -> Path -> Prop
prop_cp src dst = command "rwmd" ["cp", unpath src, unpath dst] `yields` [R src, W dst]

prop_mv :: Path -> Path -> Prop
prop_mv src dst = command "rwmd" ["mv", unpath src, unpath dst] `yields` [M dst src]

prop_touch :: Path -> Prop
prop_touch dst = command "t" ["touch", unpath dst] `yields` [T dst]

prop_rm :: Path -> Prop
prop_rm dst = command "rwmd" ["rm", unpath dst] `yields` [D dst]

prop_gcc :: Path -> [Access] -> Prop
prop_gcc src deps = command "r" ["gcc", "-E", unpath src] `yields` deps

prop_cl :: Path -> [Access] -> Prop
prop_cl src deps = command "r" ["cl", "/nologo", "/E", unpath src] `yields` deps

main :: IO ()
main = do
    results <- sequence [allTests sp sm | sp <- allValues, sm <- allValues]
    when (any (not . isSuccess) $ concat results) exitFailure
  where noisy s = putStrLn ("Testing " ++ s)
        banner x = putStrLn $ "================ " ++ x ++ " ================"
        dirname Unspaced = "fsatrace"
        dirname Spaced = "fsatrace with spaces"
        allValues :: (Enum a, Bounded a) => [a]
        allValues = enumFrom minBound
        allTests :: SpaceMode -> ShellMode -> IO [Result]
        allTests sp sm = withSystemTempDirectory (dirname sp) $ \utmp -> do
          t <- canonicalizePath utmp
          banner $ show sp ++ " " ++ show sm
          src <- canonicalizePath $ ".." </> "src"
          cl <- findExecutable "cl.exe"
          pwd <- canonicalizePath =<< getCurrentDirectory
          let hascl = isJust cl
              tsrc = t </> "src"
              emitc = Path $ tsrc </> "emit.c"
              srcc = Path $ tsrc </> "src.c"
              clcsrc = Path $ tsrc </> "win" </> "handle.c"
              rvalid = sort . filter (valid t) . map (R . Path)
              e = Env {shellMode = sm, tmpDir = t, pwdDir = pwd}
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
            ++ [qc "echo" $ prop_echo emitc srcc | sm == Shelled]


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
