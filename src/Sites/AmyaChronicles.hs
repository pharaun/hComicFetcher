module Sites.AmyaChronicles
    ( amyaChronicles
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes)

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List as DL
import qualified Data.List.Split as SL

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

-- Safehead
import Safe

-- Taggy
import Text.Taggy
import Text.Taggy.Lens

-- Local imports
import Types
import Sites.Util


data CTag = Index              -- Volume/Chapter Index page
          | Page ComicTag      -- Each single page to be fetched

--
-- Amya Chronicles - Testing Taggy parsing
--
amyaChronicles = Comic
    { comicName = "Amya Chronicles"
    , seedPage = "http://www.amyachronicles.com/archives"
    , seedType = Index

    , pageParse = amyaChroniclesPageParse
    }

amyaChroniclesPageParse :: ReplyType CTag -> IO [FetchType CTag]
amyaChroniclesPageParse (WebpageReply html Index) = do
    return []

amyaChroniclesPageParse (WebpageReply html (Page ct)) = do
    return []
