module Day1 where

import Data.Maybe (fromMaybe)
import Data.List (tails)

-- pairs :: [Int] -> [[Int]]
-- pairs []     = []
-- pairs (x:xs) = [[x,y] | y <- xs] ++ pairs xs

-- triplets :: [Int] -> [[Int]]
-- triplets []     = []
-- triplets (x:xs) = map (x:) (pairs xs) ++ triplets xs

-- triplets [1,2,3,4]
-- map (1:) (pairs [2,3,4]) ++ triplets [2,3,4]
-- map (1:) (pairs [2,3,4]) ++ map (2:) (pairs [3,4]) ++ map (3:) (pairs [4]) ++ map (4:) (pairs [])

group :: Int -> [Int] -> [[Int]]
group 0 _      = [[]]
group _ []     = []
group n (x:xs) = map (x:) (group (n-1) xs) ++ group n xs

mult :: [[Int]] -> Maybe Int
mult []     = Nothing
mult (x:xs) = if sum x == 2020 then Just (product x) else mult xs

getProd :: ([Int] -> [[Int]]) -> [Int] -> Int
getProd = ((fromMaybe 0 . mult) .)


-- Andreas'
findPair :: [Int] -> Int
findPair xs = head [a*b | (a:bs) <- tails xs, b <- bs, a + b == 2020]
-- tails [1..4]
-- (a:bs) = (1:[2,3,4]), (2:[3,4]), (3:[4]), (4:[]), []

findTrip :: [Int] -> Int
findTrip xs = head [a*b*c | (a:bs) <- tails xs, (b:cs) <- tails bs, c <- cs, a + b + c == 2020]

toy :: [Int]
toy = [1721, 979, 366, 299, 675, 1456]

day1 :: IO ()
day1 = do
  contents <- readFile "input.txt"
  -- (print . getProd (group 2) . map (read :: String -> Int) . lines) contents
  -- (print . getProd (group 3) . map (read :: String -> Int) . lines) contents
  (print . findPair . map (read :: String -> Int) . lines) contents
  (print . findTrip . map (read :: String -> Int) . lines) contents
