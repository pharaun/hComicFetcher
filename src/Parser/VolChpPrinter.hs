module Parser.VolChpPrinter
    ( comicTagToFilePath
    , unitTagToFilePath
    , unitTagString
    , formatListDigits
    , formatDigits
    , formatDigit
    , formatSubDigit
    , formatVersion
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FPO


import Data.Maybe

import qualified Data.List as DL
import qualified Data.Text as T

-- Local types
import Types

-- Debugging
import Debug.Trace


data UnitTagType = UnitTagVolume | UnitTagChapter

-- TODO: Find a better home for this
comicTagToFilePath :: ComicTag -> FPO.FilePath
comicTagToFilePath ct = traceShow ct $ DL.foldl (</>) (FPO.decodeString "./") (DL.filter (not . FP.null)
    -- Site Name
    [ FPO.fromText (ctSiteName ct)

    -- Story Name
    , case ctStoryName ct of
        Nothing -> FP.empty
        Just x  -> FPO.fromText x

    -- Volume
    , case ctVolume ct of
        Nothing -> FP.empty
        Just x  -> unitTagToFilePath UnitTagVolume x

    -- Chapter
    , case ctChapter ct of
        Nothing -> FP.empty
        Just x  -> unitTagToFilePath UnitTagChapter x

    -- TODO: this fromJust is bad news
    , FPO.fromText (fromJust $ ctFileName ct)
    ])

unitTagToFilePath :: UnitTagType -> UnitTag -> FPO.FilePath
unitTagToFilePath UnitTagVolume ut  = unitTagString (T.pack "Volume ") ut
unitTagToFilePath UnitTagChapter ut = unitTagString (T.pack "Chapter ") ut

-- TODO: need to add zero padding
unitTagString :: T.Text -> UnitTag -> FPO.FilePath
unitTagString s UnitTag{utNumber=issue, utTitle=name} = FPO.fromText (s `T.append` (formatListDigits issue) `T.append` (
    case name of
        Nothing -> T.empty
        Just x  -> T.pack ": " `T.append` x
    ))

formatListDigits :: [Digits] -> T.Text
formatListDigits x = T.intercalate (T.pack ",") (map formatDigits x)

formatDigits :: Digits -> T.Text
formatDigits (RangeDigit a b) = T.concat [formatDigit a, T.pack "-", formatDigit b]
formatDigits (StandAlone a)   = formatDigit a

formatDigit :: Digit -> T.Text
formatDigit (Digit i (Just s) (Just v) (Just t)) = T.concat [zeroPad i, formatSubDigit s, formatVersion v, t]
formatDigit (Digit i (Just s) (Just v) Nothing)  = T.concat [zeroPad i, formatSubDigit s, formatVersion v]
formatDigit (Digit i (Just s) Nothing (Just t))  = T.concat [zeroPad i, formatSubDigit s, t]
formatDigit (Digit i (Just s) Nothing Nothing)   = T.concat [zeroPad i, formatSubDigit s]
formatDigit (Digit i Nothing (Just v) (Just t))  = T.concat [zeroPad i, formatVersion v, t]
formatDigit (Digit i Nothing (Just v) Nothing)   = T.concat [zeroPad i, formatVersion v]
formatDigit (Digit i Nothing Nothing (Just t))   = T.concat [zeroPad i, t]
formatDigit (Digit i Nothing Nothing Nothing)    = T.concat [zeroPad i]

-- TODO: Formatting is a bit debatable but its directly concat
formatSubDigit :: SubDigit -> T.Text
formatSubDigit (DotSubDigit (Just i) t) = T.concat [T.pack ".", T.pack $ show i, t]
formatSubDigit (DotSubDigit Nothing t)
    | T.null t  = T.empty
    | otherwise = T.concat [T.pack ".", t]

formatVersion :: Integer -> T.Text
formatVersion v = T.pack "v" `T.append` (T.pack $ show v)

-- TODO: adjust but let's start with 3 digits for now
zeroPad :: Integer -> T.Text
zeroPad = T.justifyRight 3 '0' . T.pack . show
