module Sites.Batoto
    ( batoto
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes)

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List as DL
import qualified Data.List.Split as SL

import Text.XML.HXT.Core

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

-- Local imports
import Types
import Sites.Util

-- Tagsoup
import Text.XML.HXT.TagSoup


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

    , pageParse = batotoPageParse
    }

batotoPageParse :: ReplyType Tag -> IO [FetchType Tag]
batotoPageParse (WebpageReply html Index) = do
    let doc = readString [withParseHTML yes, withWarnings no, withTagSoup] $ UL.toString html
    story <- runX $ doc //> storyName
    volChpPage <- runX $ doc //> volChpPage

    -- Do we have any comic we want to store to disk?
    putStrLn "Story"
    mapM_ print story

    putStrLn "Vol Chp Pages"
    mapM_ print volChpPage

    -- Parse the Vol/Chp
    -- TODO: SafeHead
    let next = map (\(a, b) -> (a, volChpParse "batoto" (head story) b)) volChpPage
    return $ map (\(a, b) -> Webpage a (FirstPage b)) next

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
    otherPages <- runX $ doc >>> otherPages

    -- Do we have any comic we want to store to disk?
    putStrLn "img url"
    mapM_ print img

    putStrLn "Next pages"
    mapM_ print otherPages

    return $ map (\a -> Webpage a (Page ct)) otherPages ++ map (\a -> Image a $ comicTagFileName ct a) img

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
