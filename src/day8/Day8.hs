{-# LANGUAGE QuasiQuotes #-}

module Day8 where

import qualified Data.Vector as V
import qualified Data.Set as Set
import Text.RawString.QQ
import Text.Trifecta

toy :: String
toy = [r|nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6|]

toyFinite :: String
toyFinite = [r|nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
nop -4
acc +6|]

data Opcode = Acc | Jmp | Nop deriving (Show, Eq)
type Instr = (Opcode, Int)

pOpcode :: Parser Opcode
pOpcode = choice [ Acc <$ string "acc"
                 , Jmp <$ string "jmp"
                 , Nop <$ string "nop" ]

pInstr :: Parser Instr
pInstr = do
  opc <- pOpcode
  spaces
  opr <- integer
  return (opc, fromInteger opr)

unwrapR :: Result Instr -> Instr
unwrapR = foldResult (const (Nop, 0)) id

pInput :: [String] -> [Instr]
pInput = map (unwrapR . parseString pInstr mempty)

getNext :: Maybe Instr -> (Int, Int) -> Maybe (Int, Int)
getNext Nothing _ = Nothing
getNext (Just (Acc, i)) (ptr, acc) = Just (ptr + 1, acc + i)
getNext (Just (Jmp, i)) (ptr, acc) = Just (ptr + i, acc)
getNext (Just (Nop, _)) (ptr, acc) = Just (ptr + 1, acc)

toyVec :: V.Vector Instr
toyVec = V.fromList $ pInput $ lines toy

firstRep :: [(Int, Int)] -> Maybe Int
firstRep states = go states Set.empty
  where go [] _                = Nothing
        go ((pos, acc):xs) poses =
          if Set.member pos poses then Just acc
          else go xs (Set.insert pos poses)

nexts :: V.Vector Instr -> [(Int, Int)]
nexts vec =
  let run :: (Int, Int) -> Maybe (Int, Int)
      run (pos, acc) = getNext (vec V.!? pos) (pos, acc)
  in iterate' run (0, 0)

iterate' :: (a -> Maybe a) -> a -> [a]
iterate' f = go
  where go x' = x' : case f x' of
          Nothing -> []
          Just y -> go y

part1 :: [String] ->  Maybe Int
part1 = firstRep . nexts . V.fromList . pInput


toyVecFinite :: V.Vector Instr
toyVecFinite = V.fromList $ pInput $ lines toyFinite

terminates :: V.Vector Instr -> Bool
terminates vec =
  case firstRep (nexts vec) of
    Nothing -> True
    Just _  -> False

flipper :: Opcode -> Opcode
flipper Nop = Jmp
flipper Jmp = Nop
flipper Acc = Acc

flips :: V.Vector Instr -> V.Vector (V.Vector Instr)
flips vec = V.imap mf vec
  where mf idx (opc, int) = V.update vec (V.singleton (idx, (flipper opc, int)))

part2 :: [String] -> Int
part2 inp = V.foldr rf 0 (flips (V.fromList (pInput inp)))
  where
    rf prog val =
      let (_, acc) = last (nexts prog)
      in if terminates prog then acc else val

day8 :: IO ()
day8 = do
  contents <- readFile "input.txt"
  print $ part1 $ lines contents
  print $ part2 $ lines contents
