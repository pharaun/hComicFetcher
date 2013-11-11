module Sites.GunnerkrigCourt
    ( gunnerkrigCourt
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes)

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List as DL
import qualified Data.List.Split as SL

import Text.XML.HXT.Core

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TBMChan
import Control.Monad.STM (atomically)
import qualified Control.Monad as CM

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

import Control.Exception (throw)

-- Local imports
import Types
import Sites.Util

--
-- Gunnerkrigg Court
--
gunnerkrigCourt = Comic
    { comicName = "Gunnerkrigg Court"
    , seedPage = "http://www.gunnerkrigg.com/archives/"
    , seedType = undefined

    , pageParse = gunnerkrigCourtPageParse
    }

gunnerkrigCourtPageParse :: ReplyType t -> IO [FetchType t]
gunnerkrigCourtPageParse (WebpageReply html _) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    chpPages <- runX $ chapterPage doc

    -- Dump list of Comic page fetched
    putStrLn "Chp list:"
    mapM_ (putStrLn . fst) chpPages

    return (DL.concatMap (\(chp, img) -> map (\i -> Image i (comicFileName chp i)) img) chpPages)

   where
    -- TODO: this can probably be made more specific/nicer
    comicFileName filepath url =
        let (chp, name) = fixChp filepath
        in ComicTag (T.pack "gunnerkrigg_court") Nothing Nothing (Just $ UnitTag chp name) (Just $ last $ decodePathSegments $ US.fromString url)

    chapterPage doc = listA (doc
        //> hasAttrValue "class" (== "chapters")
        >>> hasName "div"
        >>> getChildren
        >>> (
                (
                    hasName "a"
                    >>> hasAttrValue "class" (== "chapter_button")
                    /> hasName "h4"
                )
                `Text.XML.HXT.Core.orElse`
                (
                    hasName "select"
                    >>> hasAttrValue "name" (== "page")
                    >>> getChildren
                    >>> hasName "option"
                )
            )
        >>> ifA (hasName "h4") (getChildren >>> getText) (getAttrValue "value" >>> arr linkComic)
        ) >>> arr ((SL.split . SL.keepDelimsL . SL.whenElt) (isPrefixOf "Chapter"))
        >>> unlistA
        >>> arr tupleComic
        >>. arr catMaybes

    linkComic :: String -> String
    linkComic u = "http://www.gunnerkrigg.com/comics/" ++ padNum u ++ ".jpg"
        where
            padNum n = DL.concat (replicate (8 - DL.length u) "0") ++ n

    tupleComic :: [String] -> Maybe (String, [String])
    tupleComic [] = Nothing
    tupleComic (x:xs) = Just (x, xs)