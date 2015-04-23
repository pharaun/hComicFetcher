module Parser.Words
    ( wordToNumber
    ) where

import qualified Data.List as DL
import qualified Data.Text as T


-- Process a sequence of english number into an integer value
-- If invalid/incomplete/what so not it will return a 0
wordToNumber :: String -> Integer
wordToNumber = DL.foldl' compute 0 . map T.unpack . T.words . T.toLower . T.pack
    where
        compute :: Integer -> String -> Integer
        compute prior word =
            let v = DL.lookup word numberWordTable
            in case v of
                Just x  -> x + prior
                Nothing ->
                    let w = DL.lookup word multiplerWordTable
                    in case w of
                        Just y  -> y * prior
                        Nothing -> prior

numberWordTable :: [(String, Integer)]
numberWordTable =
    [ ("zero", 0)
    , ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    , ("ten", 10)
    , ("eleven", 11)
    , ("twelve", 12)
    , ("thirteen", 13)
    , ("fourteen", 14)
    , ("fifteen", 15)
    , ("sixteen", 16)
    , ("seventeen", 17)
    , ("eighteen", 18)
    , ("nineteen", 19)
    , ("twenty", 20)
    , ("thirty", 30)
    , ("fourty", 40)
    , ("fifty", 50)
    , ("sixty", 60)
    , ("seventy", 70)
    , ("eighty", 80)
    , ("ninety", 90)
    ]

multiplerWordTable :: [(String, Integer)]
multiplerWordTable =
    [ ("hundred", 100)
    , ("thousand", 1000)
    , ("million", 1000000)
    , ("billion", 1000000000)
    , ("trillion", 1000000000000)
    , ("quadrillion", 1000000000000000)
    , ("quintillion", 1000000000000000000)
    , ("sexillion", 1000000000000000000000)
    , ("septillion", 1000000000000000000000000)
    , ("octillion", 1000000000000000000000000000)
    , ("nonillion", 1000000000000000000000000000000)
    , ("decillion", 1000000000000000000000000000000000)
    ]
