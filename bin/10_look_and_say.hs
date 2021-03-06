import Data.Array.Unboxed ((!), UArray, listArray)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

tuples :: [(String, [Int])]
tuples = [
  ("1112"                                      , [62]                    ),
  ("1112133"                                   , [63, 61]                ),
  ("111213322112"                              , [64]                    ),
  ("111213322113"                              , [65]                    ),
  ("1113"                                      , [67]                    ),
  ("11131"                                     , [68]                    ),
  ("111311222112"                              , [83, 54]                ),
  ("111312"                                    , [69]                    ),
  ("11131221"                                  , [70]                    ),
  ("1113122112"                                , [75]                    ),
  ("1113122113"                                , [76]                    ),
  ("11131221131112"                            , [81]                    ),
  ("111312211312"                              , [77]                    ),
  ("11131221131211"                            , [78]                    ),
  ("111312211312113211"                        , [79]                    ),
  ("111312211312113221133211322112211213322112", [80, 28, 89]            ),
  ("111312211312113221133211322112211213322113", [80, 28, 90]            ),
  ("11131221131211322113322112"                , [80, 29]                ),
  ("11131221133112"                            , [74, 28, 91]            ),
  ("1113122113322113111221131221"              , [74, 31]                ),
  ("11131221222112"                            , [71]                    ),
  ("111312212221121123222112"                  , [72]                    ),
  ("111312212221121123222113"                  , [73]                    ),
  ("11132"                                     , [82]                    ),
  ("1113222"                                   , [85]                    ),
  ("1113222112"                                , [86]                    ),
  ("1113222113"                                , [87]                    ),
  ("11133112"                                  , [88, 91]                ),
  ("12"                                        , [0]                     ),
  ("123222112"                                 , [2]                     ),
  ("123222113"                                 , [3]                     ),
  ("12322211331222113112211"                   , [1, 60, 28, 84]         ),
  ("13"                                        , [4]                     ),
  ("131112"                                    , [27]                    ),
  ("13112221133211322112211213322112"          , [23, 32, 60, 28, 89]    ),
  ("13112221133211322112211213322113"          , [23, 32, 60, 28, 90]    ),
  ("13122112"                                  , [6]                     ),
  ("132"                                       , [7]                     ),
  ("13211"                                     , [8]                     ),
  ("132112"                                    , [9]                     ),
  ("1321122112"                                , [20]                    ),
  ("132112211213322112"                        , [21]                    ),
  ("132112211213322113"                        , [22]                    ),
  ("132113"                                    , [10]                    ),
  ("1321131112"                                , [18]                    ),
  ("13211312"                                  , [11]                    ),
  ("1321132"                                   , [12]                    ),
  ("13211321"                                  , [13]                    ),
  ("132113212221"                              , [14]                    ),
  ("13211321222113222112"                      , [17]                    ),
  ("1321132122211322212221121123222112"        , [15]                    ),
  ("1321132122211322212221121123222113"        , [16]                    ),
  ("13211322211312113211"                      , [19]                    ),
  ("1321133112"                                , [5, 60, 28, 91]         ),
  ("1322112"                                   , [25]                    ),
  ("1322113"                                   , [26]                    ),
  ("13221133112"                               , [24, 28, 91]            ),
  ("1322113312211"                             , [24, 28, 66]            ),
  ("132211331222113112211"                     , [24, 28, 84]            ),
  ("13221133122211332"                         , [24, 28, 67, 60, 28, 88]),
  ("22"                                        , [60]                    ),
  ("3"                                         , [32]                    ),
  ("3112"                                      , [39]                    ),
  ("3112112"                                   , [40]                    ),
  ("31121123222112"                            , [41]                    ),
  ("31121123222113"                            , [42]                    ),
  ("3112221"                                   , [37, 38]                ),
  ("3113"                                      , [43]                    ),
  ("311311"                                    , [47]                    ),
  ("31131112"                                  , [53]                    ),
  ("3113112211"                                , [48]                    ),
  ("3113112211322112"                          , [49]                    ),
  ("3113112211322112211213322112"              , [50]                    ),
  ("3113112211322112211213322113"              , [51]                    ),
  ("311311222"                                 , [46, 37]                ),
  ("311311222112"                              , [46, 54]                ),
  ("311311222113"                              , [46, 55]                ),
  ("3113112221131112"                          , [46, 56]                ),
  ("311311222113111221"                        , [46, 57]                ),
  ("311311222113111221131221"                  , [46, 58]                ),
  ("31131122211311122113222"                   , [46, 59]                ),
  ("3113112221133112"                          , [46, 32, 60, 28, 91]    ),
  ("311312"                                    , [44]                    ),
  ("31132"                                     , [45]                    ),
  ("311322113212221"                           , [52]                    ),
  ("311332"                                    , [37, 28, 88]            ),
  ("3113322112"                                , [37, 29]                ),
  ("3113322113"                                , [37, 30]                ),
  ("312"                                       , [33]                    ),
  ("312211322212221121123222112"               , [34]                    ),
  ("312211322212221121123222113"               , [35]                    ),
  ("32112"                                     , [36]                    )
  ]

lengths :: UArray Int Int
lengths = listArray (0, 91) (map (length . fst) tuples)

next :: UArray Int Int -> UArray Int Int
next prev = listArray (0, 91) (map (sum . map (prev !) . snd) tuples)

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> readFile "/dev/stdin"
    a:_ -> do
      exist <- doesFileExist a
      if exist then readFile a else return a
  let maybeIndex = findIndex ((== input) . fst) tuples
      index = fromMaybe (error ("invalid starting value " ++ input)) maybeIndex
  let lengths40 = iterate next lengths !! 40
  let lengths50 = iterate next lengths40 !! 10
  -- 156 is the highest we can go without exceeding 2**63-1
  -- let lengths156 = iterate next lengths50 !! 106
  print (lengths40 ! index)
  print (lengths50 ! index)
  -- print (lengths156 ! index)
