
-- | A test of the FSATrace program
module Utils(
    Env(..), ShellMode(..), SpaceMode(..),
    Prop,
    Path(..), Arg(..),
    cased, valid,
    command, yields,
    systemStdout, systemStderr
) where

import           Parse

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Char
import           Data.List.Extra
import           Data.Maybe
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
    where validChars = arbitrary `suchThat` (`notElem` "\0")

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
yields eargs res = do
  e <- ask
  return $ monadicIO $ do
    out <- liftIO $ systemStdout $ runReader eargs e
    let r = fmap (map (fmap Path) . parseFSATrace) out
    let sr | isJust r = Just $ nubSort $ filter (valid $ tmpDir e) $ fromJust r
           | otherwise = Nothing
        ok = sr == Just res
    unless ok $ liftIO $ do
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


systemStderr :: [String] -> IO (Maybe String)
systemStderr ~(cmd:args) = do
    (res,_out,err) <- readProcessWithExitCode cmd args ""
    return $ if res == ExitSuccess then Just err else Nothing

systemStdout :: [String] -> IO (Maybe String)
systemStdout ~(cmd:args) = do
    (res,out,err) <- readProcessWithExitCode cmd args ""
    when (err /= "") $ hPutStrLn stderr err
    return $ if res == ExitSuccess then Just out else Nothing
