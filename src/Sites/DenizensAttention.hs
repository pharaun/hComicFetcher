module Sites.DenizensAttention
    ( denizensAttention
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


-- Tagsoup
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast


-- Parser
import Text.Parsec
import Text.Parsec.Text
import Data.Functor.Identity (Identity)
import Control.Applicative ((*>), (<*), (<*>), (<$>), (<$), pure, liftA)
import Data.Monoid (mconcat)


-- Local imports
import Types

-- Tags
data CTag = Initial -- The initial page
         | Chp ComicTag -- Specific chapter
         | Page ComicTag

rootUrl = "http://denizensattention.smackjeeves.com"
firstChapter = "/chapters/81551/part-one-a-dark-beginning/"

--
-- Denizen Attention - Testing fast tagsoup
--
denizensAttention = Comic
    { comicName = "Denizens Attention"
    , seedPage = rootUrl ++ firstChapter
    , seedType = Initial

    , pageParse = denizensAttentionPageParse
    }

denizensAttentionPageParse :: ReplyType CTag -> IO [FetchType CTag]
denizensAttentionPageParse (WebpageReply pg Initial) = do
    let page = parseTagsT $ BL.toStrict pg

    -- Parse out a list of chapters
    --  option.jumpbox_chapter:nth-child(3)
    let chp = (1, T.pack firstChapter) : (
            zip [2..] $
            map (fromAttrib $ T.pack "value") $
            filter (~== "<option>") $
            takeWhile (~/= "</select>") $
            head $
            sections (~== "<option class=jumpbox_chapter>") page)

    print chp
    putStrLn ""

    return $ map toChp chp

denizensAttentionPageParse (WebpageReply pg (Chp ct)) = do
    let page = parseTagsT $ BL.toStrict pg

    -- Parse out a list of pages
    let pages = (
            zip [1..] $
            map (fromAttrib $ T.pack "value") $
            filter (~== "<option>") $
            takeWhile (~/= "</optgroup>") $
            dropWhile (~/= "<option class=jumpbox_page>") page)

    print pages
    putStrLn ""

    return $ map (toPage ct) pages

denizensAttentionPageParse (WebpageReply pg (Page ct)) = do
    let page = parseTagsT $ BL.toStrict pg

    let img = (
            (fromAttrib $ T.pack "src") $
            head $
            filter (~== "<img id=comic_image>") page)

    print img
    putStrLn ""

    return [Image (T.unpack img) ct]


toChp :: (Integer, T.Text) -> FetchType CTag
toChp (chp, url) = Webpage (rootUrl ++ T.unpack url) (Chp $ ComicTag (T.pack "Denizens Attention") Nothing Nothing (Just $ UnitTag [StandAlone $ Digit chp Nothing Nothing Nothing] Nothing) Nothing)

toPage :: ComicTag -> (Integer, T.Text) -> FetchType CTag
toPage ct (page, url) = Webpage (rootUrl ++ T.unpack url) (Page ct{ctFileName = Just $ T.justifyRight 7 '0' $ T.pack (show page ++ ".png")})
