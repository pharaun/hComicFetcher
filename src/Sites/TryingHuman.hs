module Sites.TryingHuman
    ( tryingHuman
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

-- Tagchup
import Text.HTML.Tagchup.Parser

--
-- TryingHuman
--
tryingHuman = Comic
    { comicName = "Trying Human"
    , seedPage = "http://tryinghuman.com/comic.php?id=1"
    , seedType = undefined

    , pageParse = tryingHumanPageParse
    }

tryingHumanPageParse :: ReplyType t -> IO [FetchType t]
tryingHumanPageParse (WebpageReply html _) = do
    return []
