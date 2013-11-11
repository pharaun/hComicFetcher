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

    -- Do we have any comic we want to store to disk?
    putStrLn "Story"
    mapM_ print story


    return []
--    return $ map (\a -> Webpage a undefined) next ++ map (\a -> Image a $ comicFileName vol a) img

   where
    storyName = hasName "h1" >>> hasAttrValue "class" ((==) "ipsType_pagetitle")

--    indexList =
--        hasName "select"
--        >>> hasAttrValue "id" (isInfixOf "cat")
--        >>> getChildren
--        >>> hasName "option"
--        >>> hasAttrValue "class" (/= "level-0") -- Filter first level
--        >>> hasAttrValue "value" (/= "131") -- History
--        >>> hasAttrValue "value" (/= "9") -- Commentary
--        >>> hasAttrValue "value" (/= "137") -- Guest Comics


batotoPageParse (WebpageReply html (FirstPage ct)) = undefined
batotoPageParse (WebpageReply html (Page ct)) = undefined
