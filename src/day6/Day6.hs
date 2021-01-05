{-# LANGUAGE QuasiQuotes #-}

module Day6 where

import Text.RawString.QQ
import Data.List.Split (splitOn)
import qualified Data.Set as S

toy :: String
toy = [r|abc

a
b
c

ab
ac

a
a
a
a

b|]


union :: [String] -> S.Set Char
union [] = S.empty
union [xs] = S.fromList xs
union (xs:xss) = foldr (S.union . S.fromList) (S.fromList xs) xss

part1 :: [String] -> Int
part1 = sum . map (S.size . union . lines)


intersect :: [String] -> S.Set Char
intersect [] = S.empty
intersect [xs] = S.fromList xs
intersect (xs:xss) = foldr (S.intersection . S.fromList) (S.fromList xs) xss

part2 :: [String] -> Int
part2 = sum . map (S.size . intersect . lines)

-- doesn't account for repeated letters in a line (which don't occur anyway)
-- setIntersection :: [String] -> String
-- setIntersection [] = []
-- setIntersection [xs] = xs
-- setIntersection (xs:xss) = [x | x <- xs, x `elem` setIntersection xss]

-- part2 :: [String] -> Int
-- part2 = sum . map (length . setIntersection . lines)


day6 :: IO ()
day6 = do
  contents <- readFile "input.txt"

  let groups = splitOn "\n\n" contents

  print $ part1 groups
  print $ part2 groups
