
-- | A test of the FSATrace program
module Utils(
    Env(..), ShellMode(..), SpaceMode(..),
    Prop,
    Path(..), Arg(..),
    Access(..), parse, valid,

) where

import           Control.Monad.Trans.Reader
import           Data.List.Extra
import           Data.Char
import           Data.Maybe
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
