module Day5 where

import Data.List (foldl', (\\))
import Data.Maybe (mapMaybe)
import Data.Char (digitToInt)

getHalves :: Char -> Int -> Int -> (Int, Int)
getHalves pos from to =
  let half = div (to-from) 2 + 1
  in case pos of
    'F' -> (from, from+half-1)
    'L' -> (from, from+half-1)
    'B' -> (to-half+1, to)
    _   -> (to-half+1, to)

getRC :: (Int, Int) -> String -> Int
getRC range inp =
  let (f, _) = foldl' rf range inp
        where rf (from, to) pos = getHalves pos from to
  in f

calcID :: String -> Int
calcID inp =
  let row = getRC (0, 127) (take 7 inp)
      col = getRC (0, 7) (drop 7 inp)
  in row * 8 + col


-- with binary
-- 1011 = 2^3 * 1 + 2^2 * 0 + 2^1 * 1 + 2^0 * 1
-- 2 * (2^2 * 1 + 2^1 * 0 + 1) + 1
-- 2 * (2 * (2^1 * 1 + 0) + 1) + 1
-- 2 * (2 * (2 * 1 + 0) + 1) + 1
-- 2 * (2 * (2 * (2 * 0 + 1) + 0) + 1) + 1

dirToBin :: Char -> Maybe Char
dirToBin 'F' = Just '0'
dirToBin 'B' = Just '1'
dirToBin 'L' = Just '0'
dirToBin 'R' = Just '1'
dirToBin _   = Nothing

binToDec :: String -> Int
binToDec xs =
  foldl' (\acc x -> 2*acc + digitToInt x) 0 (mapMaybe dirToBin xs)

day5 :: IO ()
day5 = do
  contents <- readFile "input.txt"

  -- let ids = map calcID $ lines contents
  let ids = map binToDec $ lines contents

  print $ maximum ids
  print $ head $ [minimum ids..maximum ids] \\ ids
