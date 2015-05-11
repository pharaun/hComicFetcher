{-# LANGUAGE OverloadedStrings #-}
module Sites.Disenchanted
    ( disenchanted
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes, maybeToList, listToMaybe)

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List as DL
import qualified Data.List.Split as SL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

-- xml-conduit


-- Parser
import Text.Parsec
import Text.Parsec.Text
import Data.Functor.Identity (Identity)
import Control.Applicative ((*>), (<*), (<*>), (<$>), (<$), pure, liftA)
import Data.Monoid (mconcat)


-- Local imports
import Types


--
-- disenchanted - testing xml-conduit
--
disenchanted = Comic
    { comicName = "Disenchanted"
    , seedPage = "http://www.disenchantedcomic.com/webcomic/1/"
    , seedType = undefined

    , pageParse = disenchantedPageParse
    }

disenchantedPageParse :: ReplyType t -> IO [FetchType t]
disenchantedPageParse (WebpageReply pg _) = do
    return []
