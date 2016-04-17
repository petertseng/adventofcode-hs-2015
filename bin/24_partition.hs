import AdventOfCode (readInputFile)
import Control.Monad (foldM)
import Data.Foldable (for_)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)

minNeeded :: [Int] -> Int -> Int
minNeeded weights target = case foldM add (0, 0) sorted of
  Left n -> n
  Right (_, n) -> n + 1 -- not even these weights was enough?
  where sorted = sortBy (flip compare) weights
        add (w, n) x =
          if w + x >= target then Left (n + 1) else Right (w + x, n + 1)

-- http://stackoverflow.com/questions/21265454
combinations :: Int -> [a] -> [[a]]
combinations n l = let len = length l
                   in if n > len then [] else bySize l !! (len - n)
 where
   bySize [] = [[[]]]
   bySize (x:xs) = let next = bySize xs
                   in zipWith (++) ([] : next) (map (map (x:)) next ++ [[]])

partition :: [Int] -> Int -> Int
partition weights groups =
  head (mapMaybe (partitionN weights eachGroup) [minSize..])
  where eachGroup = sum weights `div` groups
        minSize = minNeeded weights eachGroup

-- Wrong code: Only checks the first group.
-- Doesn't check the remainder for partitionability.
partitionN :: [Int] -> Int -> Int -> Maybe Int
partitionN weights eachGroup n = case candidates of
  [] -> Nothing
  l -> Just (minimum (map product l))
  where candidates = filter ((== eachGroup) . sum) (combinations n weights)

main :: IO ()
main = do
  s <- readInputFile
  let weights = map read (lines s)
  for_ [3, 4] (print . partition weights)
