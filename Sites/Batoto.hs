module Sites.Batoto
    (
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

----
---- Batoto
----
--batoto = Comic
--    { comicName = "Batoto"
--    , seedPage = "http://www.batoto.net/comic/_/comics/yotsubato-r311"
--    , seedType = VolChpIndex
--
--    , nextPage = undefined
--
--    , comic = undefined
--
--    , whichVolChp = undefined
--    , indexList = undefined
--    , chapterList = undefined
--    , chapterNextPage = undefined
--    , chapterPage = undefined
--
--    , comicTagFileName = undefined
--    , comicFileName = \_ url ->
--        ComicTag (T.pack "batoto") (Just $ T.pack "yotsubato") Nothing Nothing Nothing
--    }
