module Sites.DoesNotPlayWellWithOthers
    ( doesNotPlayWellWithOthers
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes)

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List as DL
import qualified Data.List.Split as SL

import Text.XML.HXT.Core

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TBMChan
import Control.Monad.STM (atomically)
import qualified Control.Monad as CM

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

import Control.Exception (throw)

-- Local imports
import Types
import Sites.Util

--
-- Does Not Play Well With Others
--
doesNotPlayWellWithOthers = Comic
    { comicName = "Does Not Play Well With Others"
    , seedPage = "http://www.doesnotplaywellwithothers.com/comics/pwc-000f"
    , seedType = undefined

    , pageParse = dnpwwoPageParse
    }

dnpwwoPageParse :: ReplyType t -> IO [FetchType t]
dnpwwoPageParse (WebpageReply html _) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    next <- runX $ doc //> nextPage
    img <- runX $ doc //> comic

    -- Do we have any comic we want to store to disk?
    putStrLn "Fetched Urls:"
    mapM_ putStrLn img
    mapM_ putStrLn next

    return $ (map (\a -> Webpage a undefined) next) ++ (map (\a -> Image a $ comicFileName a) img)

   where
    nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"
    comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    comicFileName url = ComicTag (T.pack "does_not_play_well_with_others") Nothing Nothing Nothing (Just $ last $ decodePathSegments $ US.fromString url)
