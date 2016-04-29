module Sites.ErrantStory
    ( errantStory
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.List (isInfixOf)
import qualified Data.List as DL

import Text.XML.HXT.Core hiding (root)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

import Control.Exception (throw)

-- Local imports
import Types
import Sites.Util


-- TODO: errant story only
data Tag = VolIndex -- Volume Index page
         | Chapter ComicTag -- Entire chapters page
         | Page ComicTag -- single comic page

-- Errant Story
errantStory :: Comic Tag
errantStory = Comic
    { comicName = "Errant Story"
    , seedPage = "http://www.errantstory.com"
    , seedType = VolIndex
    , pageParse = toPipeline errantStoryPageParse
    , cookies = []
    }

errantStoryPageParse :: ReplyType Tag -> IO [FetchType Tag]
errantStoryPageParse (WebpageReply html VolIndex) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    index <- runX $ doc //> indexList

    -- TODO: have a pre-process step for processing the parsing results into a useful list for dumping into TBMChans
    let defaultErrantStory = ComicTag (T.pack "errant_story") Nothing Nothing Nothing Nothing
    let list = buildUrlAndComicTagMapping defaultErrantStory index

    -- Dump list of Comic page fetched
    putStrLn "Chp list:"
    mapM_ print list

    return $ map (\(u, p) -> Webpage u (Chapter p)) list

  where
    indexList =
        hasName "select"
        >>> hasAttrValue "id" (isInfixOf "cat")
        >>> getChildren
        >>> hasName "option"
        >>> hasAttrValue "class" (/= "level-0") -- Filter first level
        >>> hasAttrValue "value" (/= "131") -- History
        >>> hasAttrValue "value" (/= "9") -- Commentary
        >>> hasAttrValue "value" (/= "137") -- Guest Comics
        >>> (
            (getAttrValue "value" >>> arr ("http://www.errantstory.com/?cat=" ++))
            &&&
                (
                getAttrValue "class"
                &&&
                (getChildren >>> getText >>> arr (filter (/= '\160')))
                )
            )

    buildUrlAndComicTagMapping :: ComicTag -> [(Url, (String, String))] -> [(Url, ComicTag)]
    buildUrlAndComicTagMapping _ [] = []
    buildUrlAndComicTagMapping root (x@(_, (level, name)):xs)
         | level == "level-3" = map (levelToComicTagMapping root) (x:xs)
         | otherwise          =
             let ours = DL.takeWhile (notSameLevel level) xs
                 rest = DL.dropWhile (notSameLevel level) xs
             in
                 buildUrlAndComicTagMapping (snd $ levelToComicTagMapping root x) ours ++ buildUrlAndComicTagMapping root rest
        where
            notSameLevel :: String -> (Url, (String, String)) -> Bool
            notSameLevel targetLevel (_, (tLevel, _)) = targetLevel /= tLevel

    levelToComicTagMapping :: ComicTag -> (Url, (String, String)) -> (Url, ComicTag)
    levelToComicTagMapping parent (url, ("level-3", name)) = (url, parent {ctChapter = fixChapter name})
    levelToComicTagMapping parent (url, ("level-2", name)) = (url, parent {ctVolume = fixVolume name})
    levelToComicTagMapping parent (url, ("level-1", name)) = (url, parent {ctStoryName = Just $ T.pack name})
    levelToComicTagMapping parent content = throw $ DebugException "levelToComicTagMapping" ("Parent: " ++ show parent ++ " - Content: " ++ show content)

errantStoryPageParse (WebpageReply html (Chapter ct)) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    chp <- runX $ doc //> chapterList
    next <- runX $ doc //> chapterNextPage

    -- Do we have any comic we want to store to disk?
    putStrLn "Chp list:"
    mapM_ print chp

    putStrLn "Next archive:"
    mapM_ print next

    -- TODO:
    --  1. Do something with the name of the page (Chapter 42: foobar) (not on all pages unfortunately)
    return $ map (\a -> Webpage a (Chapter ct)) next ++ map (\a -> Webpage (fst a) (Page ct)) chp

   where
    chapterList =
        hasName "div"
        >>> hasAttrValue "class" (isInfixOf "comicarchiveframe")
        >>> getChildren
        >>> hasName "a"
        >>> hasAttr "href"
        >>> (
            getAttrValue "href"
            &&&
                (
                getChildren
                >>> hasName "img"
                >>> hasAttr "alt"
                >>> getAttrValue "alt"
                )
            )

    chapterNextPage =
        hasName "div"
        >>> hasAttrValue "class" (== "pagenav-right")
        >>> getChildren
        >>> hasName "a"
        >>> hasAttr "href"
        >>> getAttrValue "href"

errantStoryPageParse (WebpageReply html (Page ct)) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    img <- runX $ doc //> comic

    -- Do we have any comic we want to store to disk?
    putStrLn "Fetched Urls:"
    mapM_ putStrLn img

    return (map (\a -> Image a $ comicTagFileName ct a) img)

   where
    comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    comicTagFileName ctt url = ctt{ctFileName = Just $ last $ decodePathSegments $ US.fromString url}


-- Test data
testTag = ComicTag {ctSiteName = T.pack "errant_story", ctStoryName = Nothing, ctVolume = Nothing, ctChapter = Nothing, ctFileName = Nothing}
testUrl =  [
    ("http://www.errantstory.com/?cat=129",("level-1","Errant Story")),
        ("http://www.errantstory.com/?cat=59",("level-2","Volume 1")),
            ("http://www.errantstory.com/?cat=25",("level-3","Chapter 00 (Prologue)")),
            ("http://www.errantstory.com/?cat=24",("level-3","Chapter 01")),
        ("http://www.errantstory.com/?cat=59",("level-2","Volume 2")),
            ("http://www.errantstory.com/?cat=25",("level-3","Chapter 02")),
            ("http://www.errantstory.com/?cat=24",("level-3","Chapter 03")),
    ("http://www.errantstory.com/?cat=129",("level-1","Errant Story CT")),
        ("http://www.errantstory.com/?cat=59",("level-2","Volume 1")),
            ("http://www.errantstory.com/?cat=25",("level-3","Chapter 00 (Prologue)")),
            ("http://www.errantstory.com/?cat=24",("level-3","Chapter 01")),
        ("http://www.errantstory.com/?cat=59",("level-2","Volume 2")),
            ("http://www.errantstory.com/?cat=25",("level-3","Chapter 02")),
            ("http://www.errantstory.com/?cat=24",("level-3","Chapter 03"))
    ]
