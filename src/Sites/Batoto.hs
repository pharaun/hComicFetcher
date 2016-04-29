module Sites.Batoto
    ( batoto
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.List (isInfixOf)

import Text.XML.HXT.Core

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US
import qualified Network.HTTP.Conduit as CH
import Data.Time.Clock
import Data.Time.Calendar

-- Local imports
import Types
import Sites.Util

-- Tagsoup
import Text.XML.HXT.TagSoup

-- Safehead
import Safe


data Tag = Index              -- Volume/Chapter Index page
         | FirstPage ComicTag -- First page (fetch the list of pages)
         | Page ComicTag      -- Each single page to be fetched

--
-- Batoto
--
batoto = Comic
    { comicName = "Batoto"
    , seedPage = error "Please specify a Batoto comic."
    , seedType = Index
    , seedCache = Always
    , pageParse = toPipeline batotoPageParse
    , cookies = [batotoCookie]
    }

batotoPageParse :: ReplyType Tag -> IO [FetchType Tag]
batotoPageParse (WebpageReply html Index) = do
    let doc = readString [withParseHTML yes, withWarnings no, withTagSoup] $ UL.toString html
    story <- runX $ doc //> storyName
    volChpPageP <- runX $ doc //> volChpPage

    -- Do we have any comic we want to store to disk?
    putStrLn "Story"
    mapM_ print story

    putStrLn "Vol Chp Pages"
    mapM_ print volChpPageP

    -- Parse the Vol/Chp
    let next = map (\(a, b) -> (a, volChpParse "batoto" (headMay story) b)) volChpPageP
    return $ map (\(a, b) -> Webpage a Always (FirstPage b)) next

   where
    storyName = hasName "h1" >>> hasAttrValue "class" ((==) "ipsType_pagetitle") /> getText >>> arr textStrip

    volChpPage =
        hasName "table"
        >>> hasAttrValue "class" (isInfixOf "chapters_list")
        //> hasName "tr"
        >>> hasAttrValue "class" (isInfixOf "lang_English")
        //> (hasName "a" `containing` (getChildren >>> hasName "img"))
        >>> (
                (getAttrValue "href")
                &&&
                (getChildren >>> getText >>> arr textStrip)
            )

    textStrip :: String -> String
    textStrip = T.unpack . T.strip . T.pack

batotoPageParse (WebpageReply html (FirstPage ct)) = do
    let doc = readString [withParseHTML yes, withWarnings no, withTagSoup] $ UL.toString html
    img <- runX $ doc //> comic
    otherPagesP <- runX $ doc >>> otherPages

    -- Do we have any comic we want to store to disk?
    putStrLn "img url"
    mapM_ print img

    putStrLn "Next pages"
    mapM_ print otherPagesP

    return $ map (\a -> Webpage a Always (Page ct)) otherPagesP ++ map (\a -> Image a $ comicTagFileName ct a) img

   where
    otherPages =
        (getChildren
        //> hasName "select"
        >>> hasAttrValue "id" ((==) "page_select"))

        -- Drop the rest
        >. head

        >>> getChildren
        >>> hasName "option"
        >>> ifA (hasAttr "selected") none (getAttrValue "value")

batotoPageParse (WebpageReply html (Page ct)) = do
    let doc = readString [withParseHTML yes, withWarnings no, withTagSoup] $ UL.toString html
    img <- runX $ doc //> comic

    -- Do we have any comic we want to store to disk?
    putStrLn "img url"
    mapM_ print img

    return $ map (\a -> Image a $ comicTagFileName ct a) img

comic = hasName "img" >>> hasAttrValue "id" ((==) "comic_page") >>> getAttrValue "src"
comicTagFileName ct url = ct{ctFileName = Just $ last $ decodePathSegments $ US.fromString url}


--
-- Custom cookie jar for Batoto to only display english
--
-- TODO: Create a way for us to have per site rules for (auth/cookies/etc)
past :: UTCTime
past = UTCTime (ModifiedJulianDay 56000) (secondsToDiffTime 0) -- 2012-03-14

future :: UTCTime
future = UTCTime (ModifiedJulianDay 60000) (secondsToDiffTime 0) -- 2023-02-25

batotoCookie :: CH.Cookie
batotoCookie = CH.Cookie
    { CH.cookie_name   = US.fromString "lang_option"
    , CH.cookie_value  = US.fromString "English"
    , CH.cookie_domain = US.fromString ".batoto.net"
    , CH.cookie_path   = US.fromString "/"

    , CH.cookie_expiry_time = future
    , CH.cookie_creation_time = past
    , CH.cookie_last_access_time = past

    , CH.cookie_persistent = True
    , CH.cookie_host_only = False
    , CH.cookie_secure_only = False
    , CH.cookie_http_only = False
    }
