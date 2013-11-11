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

    return []
--    return $ map (\a -> Webpage a undefined) next ++ map (\a -> Image a $ comicFileName vol a) img

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
    otherPages <- runX $ doc //> otherPages

    -- Do we have any comic we want to store to disk?
    putStrLn "img url"
    mapM_ print img

    putStrLn "Next pages"
    mapM_ print otherPages

    return []
--    return $ map (\a -> Webpage a undefined) next ++ map (\a -> Image a $ comicFileName vol a) img

   where
    otherPages = hasName "h1"


batotoPageParse (WebpageReply html (Page ct)) = undefined


comic = hasName "img"
