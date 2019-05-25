{-# LANGUAGE DeriveFunctor #-}

-- | A test of the FSATrace program
module Parse(
    Access(..), parseFSATrace, parseClDeps, parseMakefileDeps
) where

import           Data.Maybe


data Access a
    = R a
    | W a
    | D a
    | Q a
    | T a
    | M a a
    | RR a
    | RW a
    | RD a
    | RQ a
    | RT a
    | RM a a
      deriving (Show, Eq, Ord, Functor)

parseFSATrace :: String -> [Access FilePath]
parseFSATrace = mapMaybe f . lines
    where f ('w':'|':xs) = Just $ W xs
          f ('r':'|':xs) = Just $ R xs
          f ('d':'|':xs) = Just $ D xs
          f ('q':'|':xs) = Just $ Q xs
          f ('t':'|':xs) = Just $ T xs
          f ('m':'|':xs) | (xs','|':ys) <- break (== '|') xs = Just $ M xs' ys
          f ('W':'|':xs) = Just $ RW xs
          f ('R':'|':xs) = Just $ RR xs
          f ('D':'|':xs) = Just $ RD xs
          f ('Q':'|':xs) = Just $ RQ xs
          f ('T':'|':xs) = Just $ RT xs
          f ('M':'|':xs) | (xs','|':ys) <- break (== '|') xs = Just $ RM xs' ys
          f _ = Nothing


parseMakefileDeps :: Maybe String -> [FilePath]
parseMakefileDeps = filter (/= " ") . map unhack . words . hack . drop 1 . dropWhile (/= ':') . fromMaybe ""
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
