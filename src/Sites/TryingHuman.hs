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

import Control.Monad

-- Local imports
import Types

-- Tagchup
import qualified Text.HTML.Tagchup.Format as Format
import qualified Text.HTML.Tagchup.Parser as Parser
import qualified Text.HTML.Tagchup.Process as Process
import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.HTML.Tagchup.Tag.Match as MTag
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.Attribute as Attribute
import qualified Text.XML.Basic.Name.LowerCase as NameLC
import qualified Text.XML.Basic.Name.MixedCase as Name



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
    let doc = Parser.runSoup $ UL.toString html

    -- Comic title
    --  #comictitle
    let title = titleParse $ doc

    print title

    return []


titleParse :: [Tag.T NameLC.T String] -> String
titleParse =
    flip Format.htmlOrXhtml "" .
    tail .
    takeWhile (not . MTag.close (Name.match "div")) .
    dropWhile (not . MTag.openAttrNameLit "div" "id" (== "comictitle"))
