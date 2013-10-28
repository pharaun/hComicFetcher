module Types
    ( Url(..)
    , FetchType(..)
    , ReplyType(..)
    , Tag(..)
    ) where

import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Filesystem.Path.CurrentOS as FPO

-- Data type of the url and any additional info needed
type Url = String

data FetchType  = Webpage Url Tag
                | Image Url FPO.FilePath

data ReplyType  = WebpageReply UL.ByteString Tag

-- Additional information tags to tag on a webpage Request
data Tag = Serial  -- Page by page fetching
         | VolChpIndex   -- Volume Chp Index page
         | VolIndex   -- Volume Index page
         | ChpIndex   -- Chp Index page
         | Chapter FPO.FilePath -- Entire chapters page
         | Page FPO.FilePath   -- single comic page
