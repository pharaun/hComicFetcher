module Sites.DenizensAttention
    ( denizensAttention
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

-- Tagsoup
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast

import Control.Monad
import Control.Monad.IO.Class
import Pipes (Pipe)

-- Local imports
import Types
import Interpreter

rootUrl = "http://denizensattention.smackjeeves.com"
firstChapter = "/chapters/81551/part-one-a-dark-beginning/"

--
-- Denizen Attention - Testing fast tagsoup
--
denizensAttention = Comic
    { comicName = "Denizens Attention"
    , seedPage = rootUrl ++ firstChapter
    , seedCache = Always
    , pageParse = denizensAttentionPageParse
    , cookies = []
    }

denizensAttentionPageParse :: Pipe ReplyType FetchType IO ()
denizensAttentionPageParse = runWebFetchT $ do
    pg <- fetchSeedpage
    let page = parseTagsT $ BL.toStrict pg

    -- Parse out a list of chapters
    --  option.jumpbox_chapter:nth-child(3)
    let chp = (1, T.pack firstChapter) : (
            zip [2..] $
            map (fromAttrib $ T.pack "value") $
            filter (~== "<option>") $
            takeWhile (~/= "</select>") $
            head $
            sections (~== "<option class=jumpbox_chapter>") page)

    (liftIO . print) chp
    debug ""

    -- TODO: add in the chapter name here as well
    forM_ (map toChp chp) (\(url, ct) -> do

        pg' <- fetchWebpage [(url, Always)]
        let page' = parseTagsT $ BL.toStrict pg'

        -- Parse out a list of pages
        let pages = (
                zip [1..] $
                map (fromAttrib $ T.pack "value") $
                filter (~== "<option>") $
                takeWhile (~/= "</optgroup>") $
                dropWhile (~/= "<option class=jumpbox_page>") page')

        (liftIO . print) pages
        debug ""

        forM_ (map (toPage ct) pages) (\(url', ct') -> do
            pg'' <- fetchWebpage [(url', Always)]
            let page'' = parseTagsT $ BL.toStrict pg''

            let img = (
                    (fromAttrib $ T.pack "src") $
                    head $
                    filter (~== "<img id=comic_image>") page'')

            (liftIO . print) img
            debug ""

            fetchImage (T.unpack img) ct'
            )
        )


toChp :: (Integer, T.Text) -> (String, ComicTag)
toChp (chp, url) = ((rootUrl ++ T.unpack url), (ComicTag (T.pack "Denizens Attention") Nothing Nothing (Just $ UnitTag [StandAlone $ Digit chp Nothing Nothing Nothing] Nothing) Nothing))

toPage :: ComicTag -> (Integer, T.Text) -> (String, ComicTag)
toPage ct (page, url) = ((rootUrl ++ T.unpack url), (ct{ctFileName = Just $ T.justifyRight 7 '0' $ T.pack (show page ++ ".png")}))
