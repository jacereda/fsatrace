
-- | A test of the FSATrace program
module Test(main) where

import           Utils
import           Parse

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.List.Extra
import           Data.Maybe
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
--import           Debug.Trace

-- Confirm that we pass the arguments through properly
-- using the helper executable @dumpargs@.
prop_args :: [Arg] -> Prop
prop_args args = do
  isShell <- asks shellMode
  let safe = if isShell == Shelled then filter (`notElem` "\n\r\"%$\\") else id
  c <- command "x" $ "dumpargs" : map (safe . unarg) args
  return $ monadicIO $ do
    mout <- liftIO $ systemStdout c
    assert $ case mout of
              Just out -> map (safe . unarg) args == read (head $ lines out)
              Nothing -> False

errorFrom :: [String] -> IO String
errorFrom (cmd:args) = do
  (_,_,err) <- readProcessWithExitCode cmd args ""
  return err
errorFrom _ = undefined



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

prop_gcc :: Path -> [Access Path] -> Prop
prop_gcc src deps = command "r" ["gcc", "-E", unpath src] `yields` deps

prop_cl :: Path -> [Access Path] -> Prop
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
          _ <- systemStdout ["cp", "-R", src, tsrc]
          deps <- systemStdout ["gcc", "-MM", unpath emitc]
          ndeps <- mapM canonicalizePath (parseMakefileDeps deps)
          cldeps <- if hascl then errorFrom ["cl", "/nologo", "/showIncludes", "/E", "/DPATH_MAX=4096", unpath clcsrc] else return []
          ncldeps <- if hascl then mapM canonicalizePath (unpath clcsrc : parseClDeps cldeps) else return []
          sequence $
            [ noisy "args" >> quickCheckWithResult (stdArgs {maxSuccess=10}) (\x -> runReader (prop_args x) e) -- qc 10 "args" prop_args
            , qc "gcc" $ prop_gcc emitc (rvalid ndeps)
            , qc "cp" $ prop_cp emitc srcc
            , qc "touch" $ prop_touch srcc
            , qc "rm" $ prop_rm srcc
            , qc "mv" $ prop_mv emitc srcc
            ]
            ++ [qc "cl" $ prop_cl clcsrc (rvalid ncldeps) | hascl]
            ++ [qc "echo" $ prop_echo emitc srcc | sm == Shelled]
