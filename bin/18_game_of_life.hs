{-# LANGUAGE FlexibleContexts #-}

import AdventOfCode (readInputFile)

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (getElems, modifyArray, newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Bits (shiftL)
import Data.Foldable (for_)
import Data.Int (Int8)
import Data.Maybe (catMaybes)
import Data.Traversable (for)

gameOfLife :: [[Bool]] -> Int -> Bool -> [Int8]
gameOfLife lights n cornersStuck = runST $ do
  let len = length lights
  let maxXY = len - 1
  a <- newListArray ((0, 0), (maxXY, maxXY)) (pack lights) :: ST s (STUArray s (Int, Int) Int8)
  when cornersStuck $
    for_ [(y, x) | y <- [0, maxXY], x <- [0, maxXY]] $ \(y, x) ->
      writeArray a (y, x) 1

  let allCells = [(y, x) | y <- [0..maxXY], x <- [0..maxXY]]

  -- Initial setup: Count neighbours.
  for_ allCells $ \(y, x) -> do
    neighbourVals <- traverse (readArray a) (neighbours maxXY (y, x))
    val <- readArray a (y, x)
    writeArray a (y, x) (val + shiftL (fromIntegral (countAlive neighbourVals)) 1)

  for_ [1..n] $ \_ -> do
    changed <- for allCells $ \(y, x) -> do
      val <- readArray a (y, x)
      let isCorner = (x == 0 || x == maxXY) && (y == 0 || y == maxXY)
          isAlive = cornersStuck && isCorner || 5 <= val && val <= 7
      if isAlive /= alive val
        then return (Just (y, x))
        else return Nothing

    for_ (catMaybes changed) $ \(y, x) -> do
      val <- readArray a (y, x)
      let delta = if alive val then -2 else 2
      for_ (neighboursAndSelf maxXY (y, x)) $ \(ny, nx) -> do
        modifyArray a (ny, nx) (+ delta)

      modifyArray a (y, x) (subtract (delta `div` 2))

  getElems a

neighbours :: Int -> (Int, Int) -> [(Int, Int)]
neighbours maxXY (y, x) =
  [(ny, nx) | ny <- yrange, nx <- xrange, (ny, nx) /= (y, x)]
  where (yrange, xrange) = ranges maxXY y x

-- The duplication is regrettable, but does save some time.
neighboursAndSelf :: Int -> (Int, Int) -> [(Int, Int)]
neighboursAndSelf maxXY (y, x) = [(ny, nx) | ny <- yrange, nx <- xrange]
  where (yrange, xrange) = ranges maxXY y x

ranges :: Int -> Int -> Int -> ([Int], [Int])
ranges maxXY y x = ([yMin..yMax], [xMin..xMax])
  where yMin = max (y - 1) 0
        yMax = min (y + 1) maxXY
        xMin = max (x - 1) 0
        xMax = min (x + 1) maxXY

-- Packs lights into their byte form:
-- bit 0 is alive/dead
-- bits 1+ are neighbour count
pack :: [[Bool]] -> [Int8]
pack = concatMap (map (\b -> if b then 1 else 0))

alive :: Int8 -> Bool
alive = odd

countAlive :: [Int8] -> Int
countAlive = length . filter alive

toLight :: Char -> Bool
toLight '#' = True
toLight '.' = False
toLight c = error (c : ": invalid character")

main :: IO ()
main = do
  s <- readInputFile
  let rows = map (map toLight) (lines s)
  print (countAlive (gameOfLife rows 100 False))
  print (countAlive (gameOfLife rows 100 True))
