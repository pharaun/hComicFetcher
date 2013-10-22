{-# LANGUAGE FlexibleContexts, Rank2Types #-}
{-
    1. Basic structure
        a. Fetch a comic page
        b. Store it on disk in sequence order

        c. Close each volume/chapter such as
            Errant Story
                Vol 1
                    Chp 1
                        Pages
                    Chp 2
                        Pages
                Vol 2
                    ...
            Story Two

    2. Advanced features
        a. parallel download of different site/source
        b. automated downloading of a series to disk
        c. Storing them into cbz latter on or extracting to that
        d. Ability to restart/queue from any point in the download process
        e. Store any additional metadata (alt text, notes, etc)
        f. Ability to choice at runtime which comic to download and check for updates, etc
        g. Expire the cache or check that there's no updates

    3. Requirements?
        a. Identify what qualifies as a Volume, Chapter, Page
        b. Identify how to process/fetch "next" page, chapter, volume
        c. Identify how to download and store each to disk in a scheme that makes sense
        d. How to specifiy what to be fetched (Url of the said series?) (Site?)

    4. TODO:
        a. If cancel/exit, should delete/re-download the corrupt file
-}

import Data.Conduit (($=), ($$), ($$+), ($$+-), MonadBaseControl, MonadResource)
import qualified Data.Conduit as C
import qualified Data.Conduit.Filesystem as CF
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TMChan as CT

import Network (withSocketsDo)
import Network.HTTP.Types.URI (decodePathSegments)
import Network.HTTP.Conduit (HttpException, Manager)
import qualified Network.HTTP.Conduit as CH

import Data.Maybe (catMaybes)

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List as DL
import qualified Data.List.Split as SL

import Text.XML.HXT.Core

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM.TBMChan
import Control.Failure (Failure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import qualified Control.Exception as E
import qualified Control.Monad as CM

import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

import Filesystem (createTree, isFile)
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FPO

import Crypto.Hash (digestToHexByteString, hash, Digest, SHA512)


-- Seconds to wait between each request to this site
fetchWaitTime :: Int
fetchWaitTime = 1


-- Records for all of the site to scrap from
data Comic = Comic
    { comicName :: String

    -- Seed page/type for kickstarting the parser/fetcher
    , seedPage :: String
    , seedType :: Tag

    , nextPage :: (ArrowXml a) => a XmlTree String
    , comic :: (ArrowXml a) => a XmlTree String
    -- Add support for FPO.FilePath instead of only strings
    , comicFileName :: String -> String -> FPO.FilePath
    -- Identify act (vol 1, 2) via body (class) - single-category-act-four ...
    , whichVolChp :: (ArrowXml a) => a b XmlTree -> a b String

    -- Indexing parser
    , indexList :: (ArrowXml a) => a XmlTree (String, (String, String))
    , chapterList :: (ArrowXml a) => a XmlTree (String, String)
    , chapterNextPage :: (ArrowXml a) => a XmlTree String

    , chapterPage :: (ArrowXml a) => a b XmlTree -> a b (String, [String])
    }


--
-- SEQUAL SCAN COMICS
--

-- Exploitation Now
exploitationNow = Comic
    { comicName = "Exploitation Now"
    , seedPage = "http://www.exploitationnow.com/2000-07-07/9"
    , seedType = Serial
    , nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"
    , comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    , comicFileName = \vol url ->
        let base = FPO.decodeString "./exploitation_now"
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
            dirs = FPO.fromText $ T.pack $ exploitationNowVol vol
        in base </> dirs </> file
    , whichVolChp = \doc -> doc
        //> hasName "body"
        >>> hasAttr "class"
        >>> getAttrValue "class"
        >>> arr words
        >>> arr (filter (isPrefixOf "single-category"))
        >>> arr (filter (not . isSuffixOf "comic"))
        >>> arr (filter (not . isSuffixOf "comics"))
        >>> arr (filter (not . isSuffixOf "uncategorized"))
        >>> arr concat

    -- TODO: NOOP
    , indexList = undefined
    , chapterList = undefined
    , chapterNextPage = undefined
    , chapterPage = undefined
    }

exploitationNowVol :: String -> String
exploitationNowVol "single-category-act-one"         = "Volume 1: Act One"
exploitationNowVol "single-category-act-two"         = "Volume 2: Act Two"
exploitationNowVol "single-category-intermission-i"  = "Volume 3: Intermission I"
exploitationNowVol "single-category-act-three"       = "Volume 4: Act Three"
exploitationNowVol "single-category-intermission-ii" = "Volume 5: Intermission II"
exploitationNowVol "single-category-act-four"        = "Volume 6: Act Four"
exploitationNowVol _ = "Unknown"


-- Does Not Play Well With Others
doesNotPlayWellWithOthers = exploitationNow
    { comicName = "Does Not Play Well With Others"
    , seedPage = "http://www.doesnotplaywellwithothers.com/comics/pwc-000f"
    , seedType = Serial
    , comicFileName = \_ url ->
        let base = FPO.decodeString "./does_not_play_well_with_others"
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
        in base </> file

    -- TODO: this is a no-op because its not used, need to find a way to make it do nothing
    --  This can't be undefined because its still invokved
    , whichVolChp = \doc -> doc //> hasName "body" >>> hasAttr "class" >>> getAttrValue "class"
    }


-- Girl Genius
girlGenius = Comic
    { comicName = "Girl Genius"
    , seedPage = "http://www.girlgeniusonline.com/comic.php?date=20021104"
    , seedType = Serial

    , nextPage = hasName "td" >>> hasAttrValue "valign" (== "top") //> (hasName "a" </ (hasName "img" >>> hasAttrValue "alt" (== "The Next Comic"))) >>> getAttrValue "href"

    , comic =
        hasName "td"
        >>> hasAttrValue "valign" (== "middle")
        //> hasName "img"
        >>> hasAttr "src"
        >>> getAttrValue "src"
        >>. arr (filter (isPrefixOf "http"))


    , comicFileName = \vol url ->
        let base = FPO.decodeString "./girl_genius"
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
            dirs = FPO.fromText $ T.pack $ girlGeniusVol vol
        in base </> dirs </> file

    -- TODO: this returns a single string (cuz its concating all of this), we worked around this but this is very much non-ideal
    --  For workaround see girlGeniusVol
    , whichVolChp = \doc ->
        (doc //>
        hasName "form"
        >>> hasAttrValue "name" (== "storyline")
        //> (hasName "option" `notContaining` (getChildren >>> hasText (== "---\"ADVANCED CLASS\" BEGINS---")))
        >>> (
                (withDefault (getAttrValue0 "selected") "no" >>> arr (/= "no"))
                &&&
                (getChildren >>> getText)
            )
        >>. filter (\(a, b) -> a || ((("VOLUME" `isInfixOf` b) || ("Volume" `isInfixOf` b)) && (not (("Final Page" `isPrefixOf` b) || ("Wallpaper" `isInfixOf` b)))))
        ) >. (fst . DL.break fst)
        >>> unlistA
        >>> arr snd
        -- ) >>> arr ((SL.split . SL.keepDelimsL . SL.whenElt) (isPrefixOf "Chapter")) -- TODO: this is for splitting things up


    -- TODO: NOOP
    , indexList = undefined
    , chapterList = undefined
    , chapterNextPage = undefined
    , chapterPage = undefined
    }

girlGeniusVol :: String -> String
girlGeniusVol a = fixVolChp $ "Volume " ++ show (wordToNumber (DL.reverse $ DL.head $ DL.words $ DL.drop 3 $ DL.reverse a))


--
-- INDEX BASED COMIC
--

-- Errant Story
errantStory = Comic
    { comicName = "Errant Story"
    , seedPage = "http://www.errantstory.com"
    , seedType = VolIndex

    -- TODO: this is no-op because its a indexed/volume type of comic not sequal
    , nextPage = undefined

    , comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    , comicFileName = \filepath url ->
        let base = FPO.decodeString "./errant_story"
            fp   = FPO.decodeString filepath
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
        in base </> fp </> file

    -- TODO: this is a no-op because its not used, need to find a way to make it do nothing
    , whichVolChp = undefined

    , indexList =
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

    , chapterList =
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

    , chapterNextPage =
        hasName "div"
        >>> hasAttrValue "class" (== "pagenav-right")
        >>> getChildren
        >>> hasName "a"
        >>> hasAttr "href"
        >>> getAttrValue "href"

    , chapterPage = undefined
    }

-- TODO:
--  1. Mangle it to filter out the Commentary track (in an easy/useful way?)
--  2. Get rid of "Errant Story" part, and lower case/mabye shorten volume to vol-7/chp-2/files
buildUrlAndFilePathMapping :: FPO.FilePath -> [(Url, (String, String))] -> [(Url, FPO.FilePath)]
buildUrlAndFilePathMapping _ [] = []
buildUrlAndFilePathMapping root all@((_, (level, name)):xs)
    | level == "level-3" = map (chapterMapping root) all
    | otherwise          =
        let ours = DL.takeWhile (\a -> (fst $ snd a) /= level) xs
            rest = filter (not . flip elem ours) xs
        in buildUrlAndFilePathMapping (root </> (FPO.decodeString name)) ours ++ buildUrlAndFilePathMapping (root) rest
    where
        chapterMapping root (url, (_, name)) = (url, root </> (FPO.decodeString name))


-- Gunnerkrigg Court
gunnerkrigCourt = Comic
    { comicName = "Gunnerkrigg Court"

    , seedPage = "http://www.gunnerkrigg.com/archives/"
    , seedType = ChpIndex

    -- TODO: this is no-op because its a indexed/volume type of comic not sequal
    , nextPage = undefined

    , comic = undefined
    , comicFileName = \filepath url ->
        let base = FPO.decodeString "./gunnerkrigg_court"
            fp   = FPO.decodeString $ fixVolChp filepath
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
        in base </> fp </> file

    -- TODO: this is a no-op because its not used, need to find a way to make it do nothing
    , whichVolChp = undefined

    , indexList = undefined
    , chapterList = undefined
    , chapterNextPage = undefined

    , chapterPage = \doc -> listA (doc
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
    }

-- TODO: Bit cheap but this works for fixing up "Chapter 1" -> "Chapter 01"
-- TODO: Bit cheap but this works for fixing up "Volume 1" -> "Volume 01"
fixVolChp :: String -> String
fixVolChp ('V':'o':'l':'u':'m':'e':' ':x:'0':xs) = "Volume " ++ [x] ++ "0" ++ xs
fixVolChp ('V':'o':'l':'u':'m':'e':' ':x:'1':xs) = "Volume " ++ [x] ++ "1" ++ xs
fixVolChp ('V':'o':'l':'u':'m':'e':' ':x:'2':xs) = "Volume " ++ [x] ++ "2" ++ xs
fixVolChp ('V':'o':'l':'u':'m':'e':' ':x:'3':xs) = "Volume " ++ [x] ++ "3" ++ xs
fixVolChp ('V':'o':'l':'u':'m':'e':' ':x:'4':xs) = "Volume " ++ [x] ++ "4" ++ xs
fixVolChp ('V':'o':'l':'u':'m':'e':' ':x:'5':xs) = "Volume " ++ [x] ++ "5" ++ xs
fixVolChp ('V':'o':'l':'u':'m':'e':' ':x:'6':xs) = "Volume " ++ [x] ++ "6" ++ xs
fixVolChp ('V':'o':'l':'u':'m':'e':' ':x:'7':xs) = "Volume " ++ [x] ++ "7" ++ xs
fixVolChp ('V':'o':'l':'u':'m':'e':' ':x:'8':xs) = "Volume " ++ [x] ++ "8" ++ xs
fixVolChp ('V':'o':'l':'u':'m':'e':' ':x:'9':xs) = "Volume " ++ [x] ++ "9" ++ xs
fixVolChp ('V':'o':'l':'u':'m':'e':' ':xs) = "Volume 0" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':x:'0':xs) = "Chapter " ++ [x] ++ "0" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':x:'1':xs) = "Chapter " ++ [x] ++ "1" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':x:'2':xs) = "Chapter " ++ [x] ++ "2" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':x:'3':xs) = "Chapter " ++ [x] ++ "3" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':x:'4':xs) = "Chapter " ++ [x] ++ "4" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':x:'5':xs) = "Chapter " ++ [x] ++ "5" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':x:'6':xs) = "Chapter " ++ [x] ++ "6" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':x:'7':xs) = "Chapter " ++ [x] ++ "7" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':x:'8':xs) = "Chapter " ++ [x] ++ "8" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':x:'9':xs) = "Chapter " ++ [x] ++ "9" ++ xs
fixVolChp ('C':'h':'a':'p':'t':'e':'r':' ':xs) = "Chapter 0" ++ xs
fixVolChp xs = xs


-- Filesystem format - SiteName/StoryName/Volume/Chapter/Page.*
--
-- Other stuff - webcomic
-- 1. Sequal scan (no vol/chp/etc) - Px
-- 2. Chp - CxPx
-- 3. Vol - VxPx
-- 4. (Story/Track) Vol Chp - SxVxCxPx
-- 5. Vol Act Intermission - VxPx - Act/Intermission can act as a title
--
-- 6. Manga - CxPx, VxCxPx, V(TBD)xCxPx etc... (Probably has story track
--      too for multi story mangas)
--
-- 7. There can be named volumes, named chapters. Don't think i've seen
--      named pages (special pages such as cover, etc)
--
--
-- There is probably some common logic that can be employed for this stuff
-- ^ but majority of it is going to be figuring out how to extract useful
-- semantics, There seems to be several format/manner of indexing/paging.
--
-- 1. Sequal Paging
-- 2. Sequal Volume/Chapter paging
-- 3. Chunked/indexed volume/chapter pages
-- 4.



-- Seems like we can just hit the main page to find a chapter/volume to go
-- to then parse it out of the dropdown.
--
-- Format:
-- Vol 12 Ch 079: Name &amp; V
-- Vol TBD Ch 353: Foobar stuff
-- Ch 087: weird
-- Ch 000
--
-- Need to special case the case in which its Licensed and thus not
-- available for downloading
--
-- Most other sites seems to be mainly:
-- Chp 342
--
-- Flow
--  - Get index page (to make pick up new manga easy)
--  - Make sure its not already licensed (re index page or vol/chp page)
--  - Pick first page/chp/vol and load that to get drop down list, submit
--      one chp fetcher per line in that list


-- MangaFox
mangaFox = Comic
    { comicName = "MangaFox"
    , seedPage = "http://mangafox.me/manga/yotsubato/"
    , seedType = VolChpIndex

    , nextPage = undefined

    , comic = undefined
    , comicFileName = \_ url ->
        let base = FPO.decodeString "./manga_fox/yotsubato"
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
        in base </> file

    , whichVolChp = undefined
    , indexList = undefined
    , chapterList = undefined
    , chapterNextPage = undefined
    , chapterPage = undefined
    }





-- TODO:
--  - Defined stop point, Errant Story
--  - Some command line arg for picking which comic to run
main = do
--    let target = exploitationNow
--    let target = doesNotPlayWellWithOthers
--    let target = errantStory
--    let target = girlGenius
--    let target = gunnerkrigCourt
    let target = mangaFox

    -- Queues for processing stuff
    -- TODO: look into tweaking this and making the indexed parser not deadlock the whole thing... if there's more to add to the queue than can be processed
    toFetch <- atomically $ newTBMChan 10000
    toReturn <- atomically $ newTBMChan 10000

    -- Seed with an initial page
    atomically $ writeTBMChan toFetch $ Webpage (seedPage target) (seedType target)

    -- Start the fetcher
    threadId <- forkIO $ fetch toFetch toReturn

    -- Do processing by pulling off each entry off the toReturn and submitting more
    untilM_ (indexedParser target toReturn toFetch) id

    -- We're done kill it
    killThread threadId



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

linkComic :: String -> String
linkComic u = "http://www.gunnerkrigg.com/comics/" ++ padNum u ++ ".jpg"
    where
        padNum n = (DL.concat $ DL.take (8 - DL.length u) (DL.repeat "0")) ++ n

tupleComic :: [String] -> Maybe (String, [String])
tupleComic [] = Nothing
tupleComic (x:xs) = Just (x, xs)

-- Indexer parser,
-- The mother of all parser, it parses various Tagged pages and then go from there
-- TODO:
--  * look into some form of state transformer monad for tracking state between parse run if needed
--  * This ^ is probably the Tag, which let us tag specific page with additional information if its needed
indexedParser :: Comic -> TBMChan ReplyType -> TBMChan FetchType -> IO Bool
indexedParser c i o = do
    r <- atomically $ readTBMChan i
    case r of
        Nothing -> return False
        (Just (WebpageReply html ChpIndex)) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html

            chpPages <- runX $ (chapterPage c) doc
            -- HXT

            -- TODO: this step isn't working correctly. validate what its doing.
            --  1. Probably being limited by the queue length and blocking here, tho fetcher should work?
            atomically $ mapM_ (writeTBMChan o) (DL.concatMap (\(chp, img) -> map (\i -> Image i ((comicFileName c) chp i)) img) chpPages)

            -- Terminate since we know we are done.
            -- This does not work if there's multiple parser/worker going but it'll be ok for this poc
            atomically $ closeTBMChan o

            -- Dump list of Comic page fetched
            putStrLn "Chp list:"
            mapM_ (putStrLn . fst) chpPages

            -- We do want to keep going cos we just submitted another page to fetch
            return True

        (Just (WebpageReply html VolIndex)) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html

            index <- runX $ doc //> (indexList c)
            -- HXT

            -- TODO: have a pre-process step for processing the parsing results into a useful list for dumping into TBMChans
            let list = buildUrlAndFilePathMapping (FP.empty) index

            -- Dump list of Comic page fetched
            putStrLn "Chp list:"
            mapM_ print list

            -- TODO: this step isn't working correctly. validate what its doing.
            --  1. Probably being limited by the queue length and blocking here, tho fetcher should work?
            atomically $ mapM_ (writeTBMChan o) (map (\(u, p) -> Webpage u (Chapter p)) list)

            -- We do want to keep going cos we just submitted another page to fetch
            return True

        (Just (WebpageReply html (Chapter fp))) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html

            chp <- runX $ doc //> (chapterList c)
            next <- runX $ doc //> (chapterNextPage c)
            -- HXT

            -- Dump the next pages into the queue
            atomically $ mapM_ (writeTBMChan o) (map (\a -> Webpage a (Chapter fp)) next)

            -- Dump the single page into the queue
            -- TODO:
            --  1. Do something with the name of the page (Chapter 42: foobar) (not on all pages unfortunately)
            atomically $ mapM_ (writeTBMChan o) (map (\a -> Webpage (fst a) (Page fp)) chp)

            -- Do we have any comic we want to store to disk?
            putStrLn "Chp list:"
            mapM_ print chp

            putStrLn "Next archive:"
            mapM_ print next

            -- We do want to keep going cos we just submitted another page to fetch
            return True

        (Just (WebpageReply html (Page fp))) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html

            img <- runX $ doc //> (comic c)
            -- HXT

            atomically $ mapM_ (writeTBMChan o) (map (\a -> Image a $ (comicFileName c) (FPO.encodeString fp) a) img)

            -- TODO: probably want to have a way to identify when all of the fetching is done and parsing is done and die
            --  1. But it needs to allow for *long* parsing pauses and long fetching pauses in case of network issues or slow parser step
            -- Terminate if we decide there's no more nextPage to fetch
            -- This does not work if there's multiple parser/worker going but it'll be ok for this poc
--            CM.when (null filteredNext) $ atomically $ closeTBMChan o

            -- Do we have any comic we want to store to disk?
            putStrLn "Fetched Urls:"
            mapM_ putStrLn img

            -- We do want to keep going cos we just submitted another page to fetch
            return True

        (Just (WebpageReply html Serial)) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
            next <- runX $ doc //> (nextPage c)
            img <- runX $ doc //> (comic c)
            vol <- runX $ (whichVolChp c) doc
            -- HXT

            atomically $ mapM_ (writeTBMChan o) (map (\a -> Webpage a Serial) next)
            atomically $ mapM_ (writeTBMChan o) (map (\a -> Image a $ (comicFileName c) (concat vol) a) img)

            -- Terminate if we decide there's no more nextPage to fetch
            -- This does not work if there's multiple parser/worker going but it'll be ok for this poc
            CM.when (null next) $ atomically $ closeTBMChan o
--            CM.when (null []) $ atomically $ closeTBMChan o

            -- Do we have any comic we want to store to disk?
            putStrLn "Fetched Urls:"
            mapM_ putStrLn img
            mapM_ putStrLn next

            putStrLn "vol:"
            mapM_ putStrLn vol


            -- We do want to keep going cos we just submitted another page to fetch
            return True





-- TODO: restart if the exception kills -- main.hs: InvalidUrlException "/ggmain/doublespreads/extrabits/Gil.jpg" "Invalid URL"
fetch :: TBMChan FetchType -> TBMChan ReplyType -> IO ()
fetch i o = withSocketsDo $ E.bracket
    (CH.newManager CH.def)
    CH.closeManager
    (\manager ->
        -- Forever loop (probably don't need the forever at all)
        CM.forever $ C.runResourceT $ conduitFetcher manager i o
    )




conduitFetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> TBMChan FetchType -> TBMChan ReplyType -> m ()
conduitFetcher m i o = CT.sourceTBMChan i $= CL.mapMaybeM (fetcher m) $$ CT.sinkTBMChan o


conduitFetcherList :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> [FetchType] -> m [ReplyType]
conduitFetcherList m i = CL.sourceList i $= CL.mapMaybeM (fetcher m) $$ CL.consume


fetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> FetchType -> m (Maybe ReplyType)
fetcher m (Webpage u t) = do
    reply <- fetchSource m u
    return $ Just (WebpageReply reply t)
fetcher m (Image u f) = do
    -- Stream to disk
    fetchToDisk m u f
    return $ Nothing



fetchSource :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> String -> m UL.ByteString
fetchSource m url = do
    response <- fetchStream m url
    chunk <- response $$+- CL.consume
    return $ L.fromChunks chunk


fetchToDisk :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> String -> FPO.FilePath -> m ()
fetchToDisk m url file = do
    -- TODO: Replace this with Network.HTTP.Conduit.Downloader probably for streaming file to disk
    response <- fetchStream m url

    -- Let's create the directory tree if it does not exist first
    liftIO $ createTree $ FP.directory file

    response $$+- CF.sinkFile file


fetchStream :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> String -> m (C.ResumableSource m S.ByteString)
fetchStream m url = do
    req' <- CH.parseUrl url
    let req = req' { CH.checkStatus = \_ _ _ -> Nothing }

    -- Caching hook here
    --  1. Check for cache value
    --  2. Return cached value if any exists, otherwise
    --  3. Fetch the http request then stream it to the cache
    --  4. Return cached value

    exists <- liftIO $ cacheExists url
    CM.unless exists $ do
        response <- CH.http req m
        CH.responseBody response $$+- cacheSink url

        -- Stall the read for the prerequest wait time before moving ahead
        liftIO $ threadDelay $ 1000000 * fetchWaitTime

    cacheSource url

cacheExists :: String -> IO Bool
cacheExists = isFile . cacheFile

cacheSource :: MonadResource m => String -> m (C.ResumableSource m S.ByteString)
cacheSource url = do
    (a, b) <- (CF.sourceFile $ cacheFile url) $$+ CL.take 0
    return a

cacheSink :: MonadResource m => String -> C.Sink S.ByteString m ()
cacheSink url = do
    let fp = cacheFile url

    -- Let's create the cache if it does not exist.
    liftIO $ createTree $ FP.directory fp

    CF.sinkFile fp

cacheFile :: String -> FPO.FilePath
cacheFile url = FPO.decodeString "./cache" </> (FPO.decode $ digestToHexByteString $ (hash $ US.fromString url :: Digest SHA512))







-- Execute till result is false
untilM_ :: (Monad m) => m a -> (a -> Bool) -> m ()
untilM_ f p = do
    x <- f
    if p x
        then untilM_ f p
        else return ()


-- Process a sequence of english number into an integer value
-- If invalid/incomplete/what so not it will return a 0
wordToNumber :: String -> Integer
wordToNumber = DL.foldl' compute 0 . (map T.unpack) . T.words . T.toLower . T.pack
    where
        compute :: Integer -> String -> Integer
        compute prior word =
            let v = DL.lookup word numberWordTable
            in case v of
                (Just x) -> (x + prior)
                Nothing  ->
                    let w = DL.lookup word multiplerWordTable
                    in case w of
                        (Just y) -> (y * prior)
                        Nothing  -> prior

numberWordTable :: [(String, Integer)]
numberWordTable =
    [ ("zero", 0)
    , ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    , ("ten", 10)
    , ("eleven", 11)
    , ("twelve", 12)
    , ("thirteen", 13)
    , ("fourteen", 14)
    , ("fifteen", 15)
    , ("sixteen", 16)
    , ("seventeen", 17)
    , ("eighteen", 18)
    , ("nineteen", 19)
    , ("twenty", 20)
    , ("thirty", 30)
    , ("fourty", 40)
    , ("fifty", 50)
    , ("sixty", 60)
    , ("seventy", 70)
    , ("eighty", 80)
    , ("ninety", 90)
    ]

multiplerWordTable :: [(String, Integer)]
multiplerWordTable =
    [ ("hundred", 100)
    , ("thousand", 1000)
    , ("million", 1000000)
    , ("billion", 1000000000)
    , ("trillion", 1000000000000)
    , ("quadrillion", 1000000000000000)
    , ("quintillion", 1000000000000000000)
    , ("sexillion", 1000000000000000000000)
    , ("septillion", 1000000000000000000000000)
    , ("octillion", 1000000000000000000000000000)
    , ("nonillion", 1000000000000000000000000000000)
    , ("decillion", 1000000000000000000000000000000000)
    ]
