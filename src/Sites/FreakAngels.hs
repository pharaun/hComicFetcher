{-# LANGUAGE OverloadedStrings #-}
module Sites.FreakAngels
    ( freakAngels
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import qualified Data.List as DL

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Control.Monad
import Control.Monad.IO.Class
import Pipes (Pipe)

-- Hexpat
import Control.Lens hiding (children)
import Text.XML.Expat.Lens hiding (name)
import Text.XML.Expat.Tree

-- Local imports
import Types
import Sites.Util (toPipeline)
import Interpreter

--
-- Freak Angel - Testing hexpat lens
--
freakAngels = Comic
    { comicName = "Freak Angels"
    , seedPage = "http://www.freakangels.com"
    , seedCache = Always
    , pageParse = freakAngelsPageParse
    , cookies = []
    }

freakAngelsPageParse :: Pipe ReplyType FetchType IO ()
freakAngelsPageParse = runWebFetchT $ do
    pg <- fetchSeedpage
    let page = BL.toStrict pg

    -- Fetch the ul with the Chapter/Book listing
    --  div.menu_sidebar:nth-child(2) > div:nth-child(2) > ul:nth-child(2)
    let ul = page ^.. typedHTML . to allNodes
           . traverse . named "div" . parameterized "class" "menu_sidebar"
           . children . ix 3 . named "div" . children . ix 2
           . children . traverse . named "li"
           . children . traverse . to urlAndName . _Just

    debug "Parsing books"
    liftIO . print $ ul
    debug ""

    forM_ (map bookNameToComicTag $ DL.filter (BS.isInfixOf "cat" . snd) ul) (\(url, notes, ct) -> do
        pg <- fetchWebpage [(url, Always)]

        let page = BL.toStrict pg

        -- Fetch the link to each Episode (filter out notes)
        --  div.archive_comic:nth-child(7) > a:nth-child(1)
        let name = page ^.. typedHTML . to allNodes
                 . traverse . named "div" . parameterized "class" "archive_comic"
                 . children . ix 1 . children . ix 0 . text

        let href = page ^.. typedHTML . to allNodes
                 . traverse . named "div" . parameterized "class" "archive_comic"
                 . children . traverse . named "p"
                 . children . traverse . named "a"
                 . attributes . to (lookup "href") . _Just

        -- TODO: implement something to handle non-comic page page (notes)
        let titleHref = filter (BS.isPrefixOf "FreakAngels: Episode" . fst) $ zip name href

        debug "Parsing Episode"
        liftIO . print $ titleHref
        debug ""

        forM_ (map (bookEpisodeToComicTag ct) titleHref) (\(url, page1, ct) -> do
            pg <- fetchWebpage [(url, Always)]

            let page = BL.toStrict pg

            -- Find the url to each page
            --  .navpagenav > li:nth-child(2)
            let pages = page ^.. typedHTML . to allNodes
                      . traverse . named "ul" . parameterized "class" "navpagenav"
                      . children . ix 1
                      . children . traverse . named "span"
                      . children . traverse . named "a"
                      . attributes . to (lookup "href") . _Just

            -- TODO: Apparently page 7 on some comic can be special
            let pages' = page1 : pages

            debug "Parsing Pages"
            liftIO . print $ pages
            debug ""

            forM_ pages' (\url -> do
                pg <- fetchWebpage [(US.toString url, Always)]

                let page = BL.toStrict pg

                -- Fetch the image to download to disk
                --  .entry_page > p:nth-child(1) > img:nth-child(1)
                let img = page ^. typedHTML . to allNodes
                        . traverse . named "div" . parameterized "class" "entry_page"
                        . children . ix 1
                        . children . traverse . named "img"
                        . attributes . to (lookup "src") . _Just

                debug "Parsing Image"
                liftIO . print $ img
                debug ""

                -- TODO: implement something to handle non-comic page page (notes)
                if BS.null img then (debug "Dropping") else (fetchImage (US.toString img) ct{ctFileName = Just $ last $ decodePathSegments $ img})
                )
            )
        )



typedHTML :: Iso' US.ByteString (UNode US.ByteString)
typedHTML = _HTML'


urlAndName :: (UNode US.ByteString) -> Maybe (US.ByteString, US.ByteString)
urlAndName (Element _ a c) = case lookup "href" a of
    Nothing  -> Nothing
    Just url -> case c of
        [Text x]  -> Just (x, url)
        _         -> Nothing


-- TODO clean this up more nicely (Using my Text to number parsing)
bookNameToComicTag :: (US.ByteString, US.ByteString) -> (String, Bool, ComicTag)
bookNameToComicTag ("Visual Archive: Book One", url) = ((US.toString url), False, ComicTag "Freak Angels" Nothing (Just $ UnitTag [StandAlone $ Digit 1 Nothing Nothing Nothing] Nothing) Nothing Nothing)
bookNameToComicTag ("Visual Archive: Book Two", url) = ((US.toString url), False, ComicTag "Freak Angels" Nothing (Just $ UnitTag [StandAlone $ Digit 2 Nothing Nothing Nothing] Nothing) Nothing Nothing)
bookNameToComicTag ("Visual Archive: Book Three", url) = ((US.toString url), False, ComicTag "Freak Angels" Nothing (Just $ UnitTag [StandAlone $ Digit 3 Nothing Nothing Nothing] Nothing) Nothing Nothing)
bookNameToComicTag ("Visual Archive: Book Four", url) = ((US.toString url), False, ComicTag "Freak Angels" Nothing (Just $ UnitTag [StandAlone $ Digit 4 Nothing Nothing Nothing] Nothing) Nothing Nothing)
bookNameToComicTag ("Visual Archive: Book Five", url) = ((US.toString url), False, ComicTag "Freak Angels" Nothing (Just $ UnitTag [StandAlone $ Digit 5 Nothing Nothing Nothing] Nothing) Nothing Nothing)
bookNameToComicTag ("Visual Archive: Book Six", url) = ((US.toString url), False, ComicTag "Freak Angels" Nothing (Just $ UnitTag [StandAlone $ Digit 6 Nothing Nothing Nothing] Nothing) Nothing Nothing)
bookNameToComicTag (name, url) = ((US.toString url), True, ComicTag "Freak Angels" Nothing (Just $ UnitTag [] $ Just $ TE.decodeUtf8 name) Nothing Nothing)


-- Title, url
bookEpisodeToComicTag :: ComicTag -> (US.ByteString, US.ByteString) -> (String, US.ByteString, ComicTag)
bookEpisodeToComicTag ct (title, url) = ((US.toString url), url, ct{ctChapter = Just $ UnitTag [StandAlone $ Digit (parseDigit title) Nothing Nothing Nothing] Nothing})
  where
    parseDigit t = read $ US.toString $ BS.reverse $ BS.take 4 $ BS.reverse t
