module Sites.FeyWinds
    ( feyWinds
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe (catMaybes)
import Network.HTTP.Types.URI (decodePathSegments)
import Pipes
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as US
import qualified Data.List as DL
import qualified Data.Text as T

-- Local imports
import Types
import Sites.Util
import Interpreter

-- Tagsoup
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast

--
-- Fey Winds
--
rootPage = "http://feywinds.com/"
feyWinds = Comic
    { comicName = "Fey Winds"
    , seedPage = rootPage ++ "comic.html"
    , seedType = undefined
    , pageParse = feyWindsProxy
    }


feyWindsProxy :: Pipe (ReplyType t) (FetchType t) IO ()
feyWindsProxy = runWebFetchT $ do
    debug "Parsing index page"
    idx <- parseTagsT . BL.toStrict <$> fetchSeedpage

    -- Filtered down what we want
    let idx' = filterAny
            [ (~== "<a>")
            , (\a -> isTagText a &&
                not (T.isPrefixOf (T.pack "\n") (fromTagText a)) &&
                not (T.isPrefixOf (T.pack "- ") (fromTagText a)) &&
                not ((T.isPrefixOf (T.pack " ") (fromTagText a)))
                )
            ] $
            takeWhile (~/= "</blockquote>") $
            DL.tail $
            dropWhile (~/= "<blockquote>") $
            DL.tail $
            dropWhile (~/= "<blockquote>") idx

    let pages = buildTreeUrl idx'

    forM_ pages (\(url, ct) -> do
        debug "Parsing image page"
        debug url
        page <- parseTagsT . BL.toStrict <$> fetchWebpage [rootPage ++ url]

        let img =
                fromAttrib (T.pack "src") $
                head $
                filter (~== "<img>") $
                takeWhile (~/= "</blockquote>") $
                DL.tail $
                dropWhile (~/= "<blockquote>") page

        debug img
        fetchImage (rootPage ++ "comic/" ++ T.unpack img) (ct{ctFileName = Just $ last $ decodePathSegments $ US.fromString $ T.unpack img})
        )

buildTreeUrl :: [Tag T.Text] -> [(Url, ComicTag)]
buildTreeUrl xs = catMaybes $ snd $ DL.mapAccumL accum (T.pack "", 0, T.pack "") xs
  where
    accum (vol, chpNum, chp) x
        | isTagText x && T.isPrefixOf (T.pack "Archive") (fromTagText x)        = ((T.pack "Fey Winds", chpNum, chp), Nothing)
        | isTagText x && T.isPrefixOf (T.pack "Prologue") (fromTagText x)       = ((vol, 0, snd $ parseChp $ fromTagText x), Nothing)
        | isTagText x && T.isPrefixOf (T.pack "Chapter") (fromTagText x)        = ((vol, fst $ parseChp $ fromTagText x, snd $ parseChp $ fromTagText x), Nothing)

        | isTagText x && T.isPrefixOf (T.pack "Mini Comics") (fromTagText x)    = ((T.init $ fromTagText x, 0, T.pack ""), Nothing)
        | isTagText x && T.isPrefixOf (T.pack "Other Comics") (fromTagText x)   = ((T.init $ T.init $ fromTagText x, 0, T.pack ""), Nothing)

        | x ~== "<a>"                                                           = ((vol, chpNum, chp), Just (toCT vol chpNum chp $ fromAttrib (T.pack "href") x))
        | otherwise                                                             = ((vol, chpNum, chp), Nothing)

toCT :: T.Text -> Integer -> T.Text -> T.Text -> (Url, ComicTag)
toCT vol chpNum chp url
    | chp == (T.pack "") = (T.unpack url, ComicTag (T.pack "Fey Winds") (Just vol) Nothing Nothing Nothing)
    | otherwise          = (T.unpack url, ComicTag (T.pack "Fey Winds") (Just vol) Nothing (Just $ UnitTag [StandAlone $ Digit chpNum Nothing Nothing  Nothing] (Just chp)) Nothing)

parseChp :: T.Text -> (Integer, T.Text)
parseChp t
    | (T.isPrefixOf (T.pack "Prologue") t) = (0, T.strip $ DL.last $ T.split (== ':') t)
    | otherwise                            = (read $ T.unpack $ T.takeWhile (/= ':') $ T.drop (length "Chapter ") t, T.strip $ DL.last $ T.split (== ':') t)
