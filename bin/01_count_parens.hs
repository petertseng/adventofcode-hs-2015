import AdventOfCode (readInputFile)

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Prelude hiding (floor)

move :: Int -> Char -> Int
move floor '(' = floor + 1
move floor ')' = floor - 1
move floor '\n' = floor
move _ c = error (c : ": illegal character")

main :: IO ()
main = do
  s <- readInputFile
  let floors = scanl move 0 s
  print (last floors)
  print (fromJust (findIndex (< 0) floors))
