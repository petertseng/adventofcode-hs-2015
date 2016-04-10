import AdventOfCode (readInputFile)

import qualified Data.Set as Set

type Pos = (Int, Int)

houses :: String -> Set.Set Pos
houses = Set.fromList . scanl move (0, 0)

move :: Pos -> Char -> Pos
move (x, y) c = case c of
  '^' -> (x, y + 1)
  'v' -> (x, y - 1)
  '<' -> (x - 1, y)
  '>' -> (x + 1, y)
  '\n' -> (x, y)
  _ -> error (c : ": illegal character")

-- foldr because must preserve order
-- ~ lazy pattern match: https://stackoverflow.com/a/49743871
-- (doesn't really matter for this input, but keep for education)
split :: [a] -> ([a], [a])
split = foldr (\a ~(x, y) -> (a : y, x)) ([], [])

main :: IO ()
main = do
  s <- readInputFile
  let (s1, s2) = split s
  print (Set.size (houses s))
  print (Set.size (Set.union (houses s1) (houses s2)))
