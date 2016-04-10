import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (for_)
import Data.List (find)
import Data.Maybe (fromJust)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

hasZeroes :: Int -> B.ByteString -> Bool
hasZeroes n s = fullZeroes && (even n || halfZero)
  where fullZeroes = all (== 0) (take half bytes)
        halfZero = bytes !! half < 16
        half = n `div` 2
        bytes = B.unpack s

findZeroes :: String -> Int -> Int -> Int
findZeroes input start zeroes =
  fromJust (find (hasZeroes zeroes . h) [start..])
  where h = hash . BC.append bcin . BC.pack . show
        bcin = BC.pack input

findZeroesUpTo :: String -> Int -> [Int]
findZeroesUpTo input maxZeroes = scanl (findZeroes input) 0 [1..maxZeroes]

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
  for_ (drop 5 (findZeroesUpTo input 6)) print
