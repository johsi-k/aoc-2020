{-# LANGUAGE QuasiQuotes #-}

module Day8 where

import qualified Data.Vector as V
import qualified Data.Set as Set
import Text.RawString.QQ
import Text.Trifecta
-- import Debug.Trace

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

data Opcode = Acc | Jmp | Nop
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

getNext :: Instr -> (Int, Int) -> (Int, Int)
getNext (Acc, i) (ptr, acc) = (ptr + 1, acc + i)
getNext (Jmp, i) (ptr, acc) = (ptr + i, acc)
getNext (Nop, _) (ptr, acc) = (ptr + 1, acc)

toyVec :: V.Vector Instr
toyVec = V.fromList $ pInput $ lines toy

firstRep :: [(Int, Int)] -> Maybe Int
firstRep states = go states Set.empty
  where go [] _                = Nothing
        go ((pos, acc):xs) set =
          if Set.member pos set then Just acc
          else go xs (Set.insert pos set)

nexts :: V.Vector Instr -> [(Int, Int)]
nexts arr =
  let run :: (Int, Int) -> (Int, Int)
      run (pos, acc) = getNext (arr V.! pos) (pos, acc)
      -- iterate :: ((Int, Int) -> (Int, Int)) -> (Int, Int) -> [(Int, Int)]
      -- iterate f = x : f (f x)
  in iterate run (0, 0)

part1 :: [String] ->  Maybe Int
part1 = firstRep . nexts . V.fromList . pInput


day8 :: IO ()
day8 = do
  contents <- readFile "input.txt"
  print $ part1 $ lines contents
