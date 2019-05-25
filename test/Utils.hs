
-- | A test of the FSATrace program
module Utils(
    Env(..), ShellMode(..), SpaceMode(..),
    Prop,
    Path(..), Arg(..),
    cased,

) where

import           Control.Monad.Trans.Reader
import           Data.Char
import           System.FilePath
import           System.Info.Extra
import           Test.QuickCheck
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
