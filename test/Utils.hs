{-# LANGUAGE LambdaCase #-}

-- | A test of the FSATrace program
module Utils(
    Env(..), ShellMode(..), SpaceMode(..),
    Prop,
    Path(..), Arg(..),
    Act(..), FSATest, showAct,
    cased, valid,
    command, yields, yieldsPrepare,
    systemStdout, systemStderr
) where

import           Parse

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Char
import           Data.List.Extra
import           System.Exit
import           System.IO
import           System.Process
import           System.FilePath
import           System.Info.Extra
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
--import           Debug.Trace

data Env = Env
    { shellMode :: ShellMode
    , tmpDir :: FilePath
    , pwdDir :: FilePath
    }

type Prop = Reader Env Property

newtype Arg = Arg { unarg :: String } deriving (Show, Eq)

instance Arbitrary Arg where
  arbitrary = Arg <$> listOf1 validChars
    where validChars = arbitrary `suchThat` \x -> isLatin1 x && x `notElem` "\0>|"
  shrink (Arg x) = map Arg $ shrink x

newtype Path = Path { unpath :: FilePath }

instance Eq Path where
  (==) (Path a) (Path b) = equalFilePath a b

instance Show Path where
  show (Path p) = show p

instance Ord Path where
  compare (Path x) (Path y) = compare (cased x) (cased y)


data ShellMode = Unshelled | Shelled deriving (Show, Eq, Enum, Bounded)
data SpaceMode = Unspaced | Spaced deriving (Show, Eq, Enum, Bounded)

cased :: String -> String
cased | isWindows = map toLower
      | otherwise = id



valid :: FilePath -> Access Path -> Bool
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

yields :: Reader Env [String] -> [Access Path] -> Prop
yields = yieldsPrepare True $ return ()

yieldsPrepare :: Bool -> IO () -> Reader Env [String] -> [Access Path] -> Prop
yieldsPrepare fatal prepare eargs res = do
  e <- ask
  return $ monadicIO $ do
    liftIO prepare
    let cmd = runReader eargs e
    if length (unwords cmd) > 2000 then
      fail "Excessively long command line"
    else do
      out <- liftIO $ if fatal then systemStdout cmd else Just <$> systemStdoutPass cmd
      let cleanup = nubSort . filter (valid $ tmpDir e)
      let r = fmap (cleanup . map (fmap Path) . parseFSATrace) out
      let ok = fmap cleanup r == Just (cleanup res)
      unless ok $ liftIO $ do
        putStrLn $ "Expecting " ++ show (cleanup res)
        putStrLn $ "Got       " ++ show r
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


systemStderr :: [String] -> IO (Maybe String)
systemStderr ~(cmd:args) = do
    (res,_out,err) <- readProcessWithExitCode cmd args ""
    return $ if res == ExitSuccess then Just err else Nothing

systemStdout :: [String] -> IO (Maybe String)
systemStdout ~(cmd:args) = do
    (res,out,err) <- readProcessWithExitCode cmd args ""
    when (err /= "") $ hPutStrLn stderr err
    return $ if res == ExitSuccess then Just out else Nothing

systemStdoutPass :: [String] -> IO String
systemStdoutPass ~(cmd:args) = do
    (_,out,err) <- readProcessWithExitCode cmd args ""
    when (err /= "") $ hPutStrLn stderr err
    return out

data FSATest = FSATest | FSATest32
    deriving Eq

instance Show FSATest where
  show FSATest = "fsatest"
  show FSATest32 = "fsatest32"

instance Arbitrary FSATest where
  arbitrary = elements [FSATest, FSATest32]
  shrink x = [FSATest | x /= FSATest]

data Act = ActR FilePath
         | ActW FilePath
         | ActE FSATest [Act]
         | ActF
           deriving Show

instance Arbitrary Act where
  arbitrary = sized $ \sz -> frequency
      [(8, ActR <$> name)
      ,(8, ActW <$> name)
      ,(1, return ActF)
      -- FIXME: The second 0 should be 1 - at the moment we avoid spawning children
      ,(if sz > 10 then 0 else 0, resize (min 20 $ sz-10) $ ActE <$> arbitrary <*> arbitrary)]
    where name = vectorOf 2 $ choose ('a', 'z')

  shrink (ActE a b) = (flip ActE b <$> (shrink a)) ++ (ActE a <$> shrink b)
  shrink _ = []

showAct :: Env -> [Act] -> [String]
showAct e = map f
  where
    f (ActR x) = "r" ++ tmpDir e </> x
    f (ActW x) = "w" ++ tmpDir e </> x
    f ActF = "f"
    f (ActE p xs) = "e" ++ escape (unwords $ (pwdDir e </> ".." </> show p) : showAct e xs)

    escape = concatMap $ \case
      '#' -> "##"
      ' ' -> "#"
      x   -> [x]
