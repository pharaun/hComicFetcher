module Sites.TryingHuman
    ( tryingHuman
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes)

import qualified Data.List as DL

import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy as BL

-- Local imports
import Types
import Parser.Words
import Sites.Util

-- Tagsoup
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast

-- Tags
data CTag = Initial -- The initial page
         | Page ComicTag Integer

--
-- TryingHuman
--
rootUrl = "http://tryinghuman.com/"
tryingHuman = Comic
    { comicName = "Trying Human"
    , seedPage = rootUrl ++ "archive.php"
    , seedType = Initial
    , pageParse = toPipeline tryingHumanPageParse
    , cookies = []
    }

tryingHumanPageParse :: ReplyType CTag -> IO [FetchType CTag]
tryingHumanPageParse (WebpageReply html Initial) = do
    let page = parseTagsT $ BL.toStrict html

    let subset = (
                filterAny
                    [ (\a -> isTagText a && T.isPrefixOf (T.pack "VOLUME") (fromTagText a))
                    , (\a -> isTagText a && T.isPrefixOf (T.pack "Chapter") (fromTagText a))
                    , (\a -> isTagText a && T.isPrefixOf (T.pack "Prologue") (fromTagText a))
                    , (\a -> a ~== "<a>" && T.isPrefixOf (T.pack rootUrl) (fromAttrib (T.pack "href") a))
                    ] $
                (
                takeWhile (~/= "<select>") $
                dropWhile (~/= "<div id=maincontent>") page)
                ++ (
                dropWhile (~/= "</select>") $
                dropWhile (~/= "<div id=maincontent>") page))

    return $ buildTreeUrl subset

tryingHumanPageParse (WebpageReply html (Page ct pg)) = do
    let page = parseTagsT $ BL.toStrict html

    let img = (
            (fromAttrib $ T.pack "src") $
            head $
            filter (~== "<img id=comic>") page)

    print img
    putStrLn ""

    return [Image (rootUrl ++ T.unpack img) (toPage ct pg img)]


toPage :: ComicTag -> Integer -> T.Text -> ComicTag
toPage ct page url = ct{ctFileName = Just $ T.justifyRight 8 '0' $ T.pack (show page ++ (T.unpack $ T.dropWhile (/= '.') $ last $ decodePathSegments $ US.fromString $ T.unpack url))}

buildTreeUrl :: [Tag T.Text] -> [FetchType CTag]
buildTreeUrl xs = catMaybes $ snd $ DL.mapAccumL accum (T.pack "", T.pack "", 1) xs
  where
    accum (vol, chp, pg) x
        | isTagText x && T.isPrefixOf (T.pack "VOLUME") (fromTagText x)     = ((fromTagText x, chp, pg), Nothing)
        | isTagText x && T.isPrefixOf (T.pack "Chapter") (fromTagText x)    = ((vol, fromTagText x, pg), Nothing)
        | isTagText x && T.isPrefixOf (T.pack "Prologue") (fromTagText x)   = ((vol, fromTagText x, pg), Nothing)
        | otherwise                                                         = ((vol, chp, pg + 1), Just (toCT vol chp pg $ fromAttrib (T.pack "href") x))

toCT :: T.Text -> T.Text -> Integer -> T.Text -> FetchType CTag
toCT vol chp pg url = Webpage (T.unpack url) (Page (ComicTag (T.pack "Trying Human") Nothing (Just $ UnitTag [StandAlone $ Digit (parseVol vol) Nothing Nothing Nothing] Nothing) (Just $ UnitTag [StandAlone $ Digit (parseChp chp) Nothing Nothing Nothing] Nothing) Nothing) pg)

parseVol :: T.Text -> Integer
parseVol t = wordToNumber $ T.unpack $ T.drop (length "VOLUME ") t

parseChp :: T.Text -> Integer
parseChp t
    | (t == T.pack "Prologue")  = 0
    | otherwise                 = wordToNumber $ T.unpack $ T.drop (length "Chapter ") t
