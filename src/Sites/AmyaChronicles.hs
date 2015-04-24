{-# LANGUAGE OverloadedStrings #-}
module Sites.AmyaChronicles
    ( amyaChronicles
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes, maybeToList)

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List as DL
import qualified Data.List.Split as SL

import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

-- Safehead
import Safe

-- Taggy
import Control.Lens (to, only,(^?),ix, toListOf, folded, _Just, traverse, (.~), universe, (^..), (^.))
import Text.Taggy
import Text.Taggy.Lens



-- Local imports
import Types
import Sites.Util

--
-- Amya Chronicles - Testing Taggy parsing
--
amyaChronicles = Comic
    { comicName = "Amya Chronicles"
--    , seedPage = "http://www.amyachronicles.com/archives/comic/09292009"
    , seedPage = "http://www.amyachronicles.com/archives/comic/1-43"
    , seedType = undefined

    , pageParse = amyaChroniclesPageParse
    }

amyaChroniclesPageParse :: ReplyType t -> IO [FetchType t]
amyaChroniclesPageParse (WebpageReply pg _) = do
    let text = TLE.decodeUtf8With lenientDecode pg

    putStrLn ""

    -- Next Page Link
    -- TODO: make it so it can match on only 'comic-nav-next'
    -- TODO: make it so that it only matches "class" attr
--    let next = text ^? html . allAttributed (folded . only "comic-nav-base comic-nav-next") . attr "href" . _Just

    let next = run True text
    print next

    -- Page Name
    -- TODO: make it not be a Maybe (always needs a title)
    let name = text ^. html . allAttributed (folded . only "post-title") . contents
    print name

    -- TODO: make it not be a Maybe (always needs a image)
    let img = text ^. html . allAttributed (folded . only "comic") . allNamed (only "img") . attr "src"
    print img

    -- Search for double page image link
    -- TODO: make it actually verify that its a link to
    --      "THIS IS A DOUBLE PAGE SPREAD, CLICK HERE FOR FULL IMAGE!"
    let spread = text ^? html . allNamed (only "strong") . elements . attr "href" . _Just
    print spread

    -- Fetching next page
--    return $ map (\url -> Webpage (T.unpack url) undefined) $ maybeToList next
    return []


-- Type of pages:
--  Single page - 1.00 - Chp.Page
--  Short Story - Lenna 06 - Name Page
--  Double page - 4.86 – 4.87 - Chp.Page-Chp.Page
--  Artwork - Artwork by Andrew Gregoire - Artwork/Andrew_Gregoire
