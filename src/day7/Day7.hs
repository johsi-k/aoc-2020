{-# LANGUAGE QuasiQuotes #-}

module Day7 where

import Prelude hiding (getContents)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Applicative
import Data.Char (digitToInt)

import Text.RawString.QQ
import Text.Trifecta
-- import Debug.Trace

toy :: String
toy = [r|light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.|]

pColour :: Parser String
pColour = do
  desc <- some letter
  spc <- space
  colour <- some letter
  return $ desc ++ [spc] ++ colour

pContainer :: Parser String
pContainer = do
  colour <- pColour
  spaces
  _ <- string "bags"
  return colour

pContent :: Parser (String, Int)
pContent = do
  num <- digit <* space
  colour <- pColour
  spaces
  _ <- choice [string "bags", string "bag"]
  return (colour, digitToInt num)

pContents :: Parser [(String, Int)]
pContents = sepBy pContent (string ", ")

pEmpty :: Parser [(String, Int)]
pEmpty = string "no other bags" >> return []

pRule :: Parser (String, [(String, Int)])
pRule = do
  container <- pContainer
  spaces
  _ <- string "contain"
  spaces
  contents <- choice [pEmpty, pContents]
  return (container, contents)

foldy :: Result (String, [(String, Int)]) -> (String, [(String, Int)])
foldy = foldResult (const ("", [])) id

unwrap :: [String] -> [(String, [(String, Int)])]
unwrap = map (foldy . parseString pRule mempty)

toyMap :: Map String (Map String Int)
toyMap = Map.fromList $ (fmap . fmap) Map.fromList (unwrap (lines toy))

getAncestors :: String -> Map String (Map String Int) -> Set String
getAncestors c m = Set.fromList $ Map.foldrWithKey rf [] m
  where rf k v acc = if Map.member c v then k:acc else acc

-- toyAncestors :: Set String
-- toyAncestors =
--   let b1 = Set.singleton "shiny gold"
--       ancestorSet bs = Set.fromList $ Set.foldr (\b acc -> getAncestors b toyMap <> acc) [] bs
--       b2 = ancestorSet b1
--       b3 = ancestorSet b2
--       b4 = ancestorSet b3 -- stop here

--       loopy b =
--         case (Set.toList $ ancestorSet b) of
--           [] -> b
--           _ -> loopy (ancestorSet b) <> ancestorSet b
--   in loopy b1

allAncestors :: String -> Map String (Map String Int) -> Set String
allAncestors bag bagMap =
  let getAncestorSet :: Set String -> Set String
      -- set concatMap; Set is a Foldable
      getAncestorSet bs = foldr rf Set.empty bs
        where rf b acc = getAncestors b bagMap <> acc
      go b =
        -- starting from the ancestors of given bag ensures that
        -- the bag itself is not counted
        let ancestorSet = getAncestorSet b
        in if (null ancestorSet) then b
           else go ancestorSet <> ancestorSet
  in go (Set.singleton bag)

allChildren :: String -> Map String (Map String Int) -> Maybe Int
allChildren bag bagMap = do
  c <- Map.lookup bag bagMap
  let countChildren :: Map String Int -> Maybe Int
      countChildren = Map.foldrWithKey rf (Just 1)
        where rf b n acc = do
                 acc' <- acc
                 c'   <- allChildren b bagMap
                 return $ n * c' + acc'
  if null c then Just 1 else countChildren c

-- pretty printing
pp :: (Show k, Show a) => Map k a -> String
pp = Map.foldMapWithKey (\k v -> show k ++ ": " ++ show v ++ "\n")

ppn :: (Show k, Show k', Show a) => Map k (Map k' a) -> String
ppn = Map.foldMapWithKey (\k m -> show k ++ ":\n  " ++ pp m)


day7 :: IO ()
day7 = do
  contents <- readFile "input.txt"
  let bagMap = Map.fromList $ (fmap . fmap) Map.fromList (unwrap (lines contents))

  print $ Set.size $ allAncestors "shiny gold" bagMap

  -- decrement for off-by-one since original bag is not counted
  print $ pred <$> allChildren "shiny gold" bagMap
