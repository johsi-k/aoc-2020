module Day2 where

import Text.Trifecta

toy :: [String]
toy = [ "1-3 a: abcde"
      , "1-3 b: cdefg"
      , "2-9 c: ccccccccc" ]

pComponents :: Parser ((Integer, Integer), Char, String)
pComponents = do
  [least, most] <- sepBy integer (char '-')
  spaces
  cmatch <- lower <* char ':'
  spaces
  pw <- some lower
  return ((least, most), cmatch, pw)

pValid :: Parser Bool
pValid = do
  ((least, most), cmatch, pw) <- pComponents
  let numMatch =
        toInteger $ foldr (\c acc -> fromEnum (c == cmatch) + acc) 0 pw
  return $ least <= numMatch && numMatch <= most

pValidPos :: Parser Bool
pValidPos = do
  ((pos1, pos2), cmatch, pw) <- pComponents
  return $ (pw !! fromInteger (pos1-1) == cmatch) /=
           (pw !! fromInteger (pos2-1) == cmatch)
-- xor == (/=)

numValid :: [String] -> Result Int
numValid inp = do
  bool <- traverse (parseString pValid mempty) inp
  return $ foldr ((+) . fromEnum) 0 bool

numValidPos :: [String] -> Result Int
numValidPos inp = do
  bool <- traverse (parseString pValidPos mempty) inp
  return $ foldr ((+) . fromEnum) 0 bool

day2 :: IO ()
day2 = do
  contents <- readFile "input.txt"
  (print . numValid . lines) contents
  (print . numValidPos . lines) contents
