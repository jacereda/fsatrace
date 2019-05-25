
-- | A test of the FSATrace program
module Utils(
    Env(..), ShellMode(..), SpaceMode(..),
    Prop,
    Path(..), Arg(..),
    cased, valid,
    command, yields,
    outputFrom
) where

import           Parse

import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.Char
import           Data.List.Extra
import           Data.Maybe
import           System.Exit
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


parsedOutputFrom :: [String] -> IO (Maybe [Access Path])
parsedOutputFrom x = do
  mout <- outputFrom x
  return $ case mout of
                Just out -> Just $ map (fmap Path) $ parseFSATrace out
                Nothing -> Nothing

yields :: Reader Env [String] -> [Access Path] -> Prop
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

outputFrom :: [String] -> IO (Maybe String)
outputFrom (cmd:args) = do
  (rc,out,err) <- readProcessWithExitCode cmd args ""
  when (err /= "") $ putStrLn err
  return $ if rc == ExitSuccess then Just out else Nothing
outputFrom _ = undefined
