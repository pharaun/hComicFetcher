{-# LANGUAGE OverloadedStrings #-}
module Sites.FreakAngels
    ( freakAngels
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes, maybeToList, listToMaybe)

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List as DL
import qualified Data.List.Split as SL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy as BL

-- Hexpat
import Control.Lens hiding (children)
import Text.XML.Expat.Lens
import Text.XML.Expat.Tree
import Text.XML.Expat.TagSoup


-- Parser
import Text.Parsec
import Text.Parsec.Text
import Data.Functor.Identity (Identity)
import Control.Applicative ((*>), (<*), (<*>), (<$>), (<$), pure, liftA)
import Data.Monoid (mconcat)


-- Local imports
import Types
import Sites.Util

-- Tags
data Tag = Initial -- The initial page
         | Book ComicTag -- The specific Book (Book 1 -> 6 + notes)
         | BookEpisode ComicTag -- The specific book + episode (Episode 0015)
         | Page ComicTag -- Single comic page

--
-- Freak Angel - Testing hexpat lens
--
freakAngels = Comic
    { comicName = "Freak Angels"
    , seedPage = "http://www.freakangels.com"
    , seedType = Initial

    , pageParse = freakAngelsPageParse
    }

freakAngelsPageParse :: ReplyType Tag -> IO [FetchType Tag]
freakAngelsPageParse (WebpageReply pg Initial) = do
    let page = BL.toStrict pg

    -- Fetch the ul with the Chapter/Book listing
    --  div.menu_sidebar:nth-child(2) > div:nth-child(2) > ul:nth-child(2)
    let ul = page ^.. typedHTML . to allNodes
           . traverse . named "div" . parameterized "class" "menu_sidebar"
           . children . ix 3 . named "div" . children . ix 2
           . children . traverse . named "li"
           . children . traverse . id -- TODO: replace with urlAndName

    print ul

    return []

freakAngelsPageParse (WebpageReply pg _) = undefined

typedHTML :: Iso' US.ByteString (UNode US.ByteString)
typedHTML = _HTML'
