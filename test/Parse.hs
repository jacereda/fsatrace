
-- | A test of the FSATrace program
module Parse(
    Access(..), parse, parseClDeps, parseDeps
) where

import           Utils
import           Data.Maybe


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
