module Sites.DoesNotPlayWellWithOthers
    ( doesNotPlayWellWithOthers
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.List (isInfixOf)

import Text.XML.HXT.Core

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

-- Local imports
import Types
import Sites.Util (toPipeline)

--
-- Does Not Play Well With Others
--
doesNotPlayWellWithOthers = Comic
    { comicName = "Does Not Play Well With Others"
    , seedPage = "http://www.doesnotplaywellwithothers.com/comics/pwc-000f"
    , seedCache = Always
    , pageParse = toPipeline dnpwwoPageParse
    , cookies = []
    }

dnpwwoPageParse :: ReplyType -> IO [FetchType]
dnpwwoPageParse (WebpageReply html) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    next <- runX $ doc //> nextPage
    img <- runX $ doc //> comic

    -- Do we have any comic we want to store to disk?
    putStrLn "Fetched Urls:"
    mapM_ putStrLn img
    mapM_ putStrLn next

    return $ map (\a -> Webpage a Always) next ++ map (\a -> Image a $ comicFileName a) img

   where
    nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"
    comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    comicFileName url = ComicTag (T.pack "does_not_play_well_with_others") Nothing Nothing Nothing (Just $ last $ decodePathSegments $ US.fromString url)
