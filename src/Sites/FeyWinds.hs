module Sites.FeyWinds
    ( feyWinds
    ) where

import Pipes
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy as BL

-- Local imports
import Types
import Interpreter

-- Tagsoup
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast

--
-- Fey Winds
--
rootPage = "http://feywinds.com/comic.html"
feyWinds = Comic
    { comicName = "Fey Winds"
    , seedPage = rootPage
    , seedType = undefined
    , pageParse = PipelineParser feyWindsProxy
    }


feyWindsProxy :: Pipe (ReplyType t) (FetchType t) IO ()
feyWindsProxy = runWebFetchT $ do
    debug "Fetching initial page"
    idx <- fetchWebpage [rootPage]
    debug idx
    debug "Parsing index page"

    return ()
