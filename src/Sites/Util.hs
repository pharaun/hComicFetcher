module Sites.Util
    ( fixChapter
    , fixVolume

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
