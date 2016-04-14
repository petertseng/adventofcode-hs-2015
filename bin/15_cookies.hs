import AdventOfCode (readInputFile)

import Data.Maybe (fromJust)

type Ingredient = ([Int], Int)

cookie :: [Ingredient] -> (Int, Int, Int) -> (Int, Int)
cookie ingredients (a, b, c) = cookie' (zipWith scaleBy [a, b, c, d] ingredients)
  where d = 100 - a - b - c

cookie' :: [Ingredient] -> (Int, Int)
cookie' ingredients = (score traits, sum (map calories ingredients))
  where traits = foldr1 (zipWith (+)) (map otherTraits ingredients)

score :: [Int] -> Int
score traits = if any (< 0) traits then 0 else product traits

scaleBy :: Int -> Ingredient -> Ingredient
scaleBy n (traits, cals) = (map (* n) traits, cals * n)

otherTraits :: Ingredient -> [Int]
otherTraits = fst

calories :: Ingredient -> Int
calories = snd

-- Assumes that traits come in the same order in all ingredients.
parseIngredient :: String -> Ingredient
parseIngredient s = (otherTraits', calories')
  where calories' = fromJust (lookup "calories" traitsAndNames)
        otherTraits' = map snd (filter ((/= "calories") . fst) traitsAndNames)
        traitsAndNames = case splitOn ':' s of
          [_, traits] -> map parseTrait (splitOn ',' traits)
          _ -> error ("bad cookie: " ++ s)

parseTrait :: String -> (String, Int)
parseTrait t = case words t of
  [name, value] -> (name, read value)
  _ -> error ("bad trait: " ++ t)

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'

main :: IO ()
main = do
  s <- readInputFile
  let ingredients = map parseIngredient (lines s)
      tries = case length ingredients of
        4 -> [(a, b, c) | a <- [0..100], b <- [0..(100-a)], c <- [0..(100-a-b)]]
        3 -> [(a, b, 100 - a - b) | a <- [0..100], b <- [0..(100-a)]]
        2 -> [(a, 100 - a, 0) | a <- [0..100]]
        _ -> error "can't handle ingredient list of size other than 2 through 4."
      cookies = map (cookie ingredients) tries
      best = maximum . map fst
  print (best cookies)
  print (best (filter ((== 500) . snd) cookies))