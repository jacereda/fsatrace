
-- | A test of the FSATrace program
module Test(main) where

import           Utils
import           Parse

import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Maybe
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Temp
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
--import           Debug.Trace

-- Confirm that we pass the arguments through properly
-- using the helper executable @dumpargs@.
prop_ArgsRoundtrip :: [Arg] -> Prop
prop_ArgsRoundtrip args = do
  isShell <- asks shellMode
  let safe = if isShell == Shelled then filter (`notElem` "\n\r\"%$\\") else id
  c <- command "x" $ "dumpargs" : map (safe . unarg) args
  return $ monadicIO $ do
    mout <- liftIO $ systemStdout c
    assert $ case mout of
              Just out -> map (safe . unarg) args == read (head $ lines out)
              Nothing -> False


prop_Tester :: (FSATest, [Act]) -> Prop
prop_Tester (p, actLong) = do
  e <- ask
  act <- return $ limitCmdLine 1500 e $ map (no32to64 p) actLong
  let ans = concatMap (predict e) act
  yieldsPrepare False
      (sequence_ [writeFile x "" | R (Path x) <- ans])
      (command "rwx" ((pwdDir e </> ".." </> show p) : showAct e act))
      ans
  where
    predict e (ActR x) = [R $ Path $ tmpDir e </> x]
    predict e (ActW x) = [W $ Path $ tmpDir e </> x]
    predict e (ActE _ x) = concatMap (predict e) x
    predict _ ActF = []

    limitCmdLine :: Int -> Env -> [Act] -> [Act]
    limitCmdLine n e (x:xs) | n2 > 0 = x : limitCmdLine n2 e xs
      where n2 = n - (length (unwords $ showAct e [x]) + 1)
    limitCmdLine _ _ _ = []


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
    results <- sequence [allTests sp sm | sp <- [minBound..], sm <- [minBound..]]
    when (any (not . isSuccess) $ concat results) exitFailure

noisy :: String -> IO ()
noisy s = putStrLn ("Testing " ++ s)


allTests :: SpaceMode -> ShellMode -> IO [Result]
allTests sp sm = withSystemTempDirectory (if sp == Spaced then "fsatrace with spaces" else "fsatrace") $ \utmp -> do
  putStrLn $ "================ " ++ show sp ++ " " ++ show sm ++ " ================"

  tmp <- canonicalizePath utmp
  src <- canonicalizePath $ ".." </> "src"
  pwd <- canonicalizePath =<< getCurrentDirectory

  let tsrc = tmp </> "src"
  void $ systemStdout ["cp", "-R", src, tsrc]

  let emitc = Path $ tsrc </> "emit.c"
      srcc = Path $ tsrc </> "src.c"
      rpaths = map (R . Path)
      e = Env {shellMode = sm, tmpDir = tmp, pwdDir = pwd}
      qc s p = noisy s >> quickCheckWithResult (stdArgs {maxSuccess=1}) (runReader p e)

  gccTests <- do
    deps <- systemStdout ["gcc", "-MM", unpath emitc]
    ndeps <- mapM canonicalizePath (parseMakefileDeps deps)
    return [qc "gcc" $ prop_gcc emitc (rpaths ndeps)]

  clTests <- ifM (isNothing <$> findExecutable "cl.exe") (return []) $ do
    let clcsrc = Path $ tsrc </> "win" </> "handle.c"
    cldeps <- fromMaybe [] <$> systemStderr ["cl", "/nologo", "/showIncludes", "/E", "/DPATH_MAX=4096", unpath clcsrc]
    ncldeps <- mapM canonicalizePath (unpath clcsrc : parseClDeps cldeps)
    return [qc "cl" $ prop_cl clcsrc (rpaths ncldeps)]

  sequence $
    [ noisy "args" >> quickCheckWithResult (stdArgs {maxSuccess=10}) (\x -> runReader (prop_ArgsRoundtrip x) e) -- qc 10 "args" prop_args
    ] ++ gccTests ++
    [ qc "cp" $ prop_cp emitc srcc
    , qc "touch" $ prop_touch srcc
    , qc "rm" $ prop_rm srcc
    , qc "mv" $ prop_mv emitc srcc
    ]
    ++ clTests
    ++ [qc "echo" $ prop_echo emitc srcc | sm == Shelled]
    -- FIXME: Remove the restriction on sm == Unshelled
    ++ [ noisy "random" >> quickCheckWithResult (stdArgs {maxSuccess=100}) (\x -> runReader (prop_Tester x) e) | sp == Unspaced, fixme (sm == Unshelled) True]
