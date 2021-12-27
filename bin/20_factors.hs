import Data.List (find)
import Data.Maybe (fromJust)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

primes :: [Int]
primes = [47, 43, 41, 37, 31, 29, 23, 19, 17, 13, 11, 7, 5, 3, 2]

-- askalski's tip:
-- https://www.reddit.com/r/adventofcode/comments/po1zel/comment/hd1esc2
sumExceeds :: Int -> [Int] -> Int
sumExceeds goal [] = goal
sumExceeds goal (prime:rest) = minimum (sumExceeds goal rest : candidates)
  where candidates = map sub (takeWhile (\(primePow, primeSum) -> primeSum - primePow < goal) sumsAndPowers)
        sumsAndPowers = drop 1 (iterate (\(primePow, primeSum) -> (primePow * prime, primeSum + primePow * prime)) (1, 1))
        sub (primePow, primeSum) = primePow * sumExceeds ((goal + primeSum - 1) `quot` primeSum) rest

good2 :: Int -> Int -> Bool
good2 target house = 11 * sum ([house `quot` d | d <- [1 .. 50], house `rem` d == 0]) >= target

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
  let target = read input
      house1 = sumExceeds (target `quot` 10) primes
  print house1
  let lower = if good2 target house1 then 0 else house1
      step = 2 * 3 * 5 * 7
  print (fromJust (find (good2 target) [lower, lower + step..]))
