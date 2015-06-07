module Sites.Util
    ( fixChapter
    , fixVolume
    , filterAny

    , module Parser.VolChpParser
    , module Parser.Words
    ) where

import Types
import Parser.VolChpParser
import Parser.Words

fixChapter :: String -> Maybe UnitTag
fixChapter = ctChapter . volChpParse "" Nothing

fixVolume :: String -> Maybe UnitTag
fixVolume = ctVolume . volChpParse "" Nothing

filterAny :: [a -> Bool] -> [a] -> [a]
filterAny [] [] = []
filterAny [] xs = xs
filterAny xs ys = filter (or . flip map xs . flip id) ys
