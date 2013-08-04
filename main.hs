{-# LANGUAGE FlexibleContexts, Rank2Types #-}
{-
    1. Basic structure
        a. Fetch a comic page
        b. Store it on disk in sequence order

        c. Close each volume/chapter such as -- TODO
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

    3. Requirements?
        a. Identify what qualifies as a Volume, Chapter, Page
        b. Identify how to process/fetch "next" page, chapter, volume
        c. Identify how to download and store each to disk in a scheme that makes sense
        d. How to specifiy what to be fetched (Url of the said series?) (Site?)
            a. Typeclass
                - nextPage
                - comic
                - volume, chapter

    4. TODO:
        a. If cancel/exit, should delete/re-download the corrupt file
-}
import Data.Maybe
import Data.List as DL
import Control.Arrow.ArrowTree
import Text.XML.HXT.Core

import Network

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Concurrent.STM.TBMChan
import Control.Concurrent

import Data.Conduit.TMChan

import Data.Conduit
import qualified Network.HTTP.Conduit as H
import qualified Data.Conduit as C
import Data.Conduit.Binary hiding (sourceFile, sinkFile)
import Data.Conduit.Filesystem

import qualified Data.Conduit.List as CL

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

import Data.Maybe

import qualified Control.Exception as E

import Control.Failure

import Prelude hiding (FilePath)
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FPO
import qualified Data.Conduit.Internal as IC

import Filesystem

import Network.HTTP.Types.URI

import qualified Data.Text as T

-- Cache hash
import Crypto.Hash

-- Seconds to wait between each request to this site
fetchWaitTime :: Int
fetchWaitTime = 1

-- Records for all of the site to scrap from
data Comic = Comic
    { comicName :: String
    , seedPage :: String
    , nextPage :: (ArrowXml a) => a XmlTree String
    , comic :: (ArrowXml a) => a XmlTree String
    -- Add support for FPO.FilePath instead of only strings
    , comicFileName :: String -> String -> FPO.FilePath
    -- Identify act (vol 1, 2) via body (class) - single-category-act-four ...
    , whichVolChp :: (ArrowXml a) => a XmlTree String
    }


--
-- SEQUAL SCAN COMICS
--

-- Exploitation Now
exploitationNow = Comic
    { comicName = "Exploitation Now"
    , seedPage = "http://www.exploitationnow.com/2000-07-07/9"
    , nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"
    , comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    , comicFileName = \vol url ->
        let base = FPO.decodeString "./exploitation_now"
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
            dirs = FPO.fromText $ T.pack $ exploitationNowVol vol
        in base FPO.</> dirs FPO.</> file
    , whichVolChp =
        hasName "body"
        >>> hasAttr "class"
        >>> getAttrValue "class"
        >>> arr words
        >>> arr (filter (isPrefixOf "single-category"))
        >>> arr (filter (not . isSuffixOf "comic"))
        >>> arr (filter (not . isSuffixOf "comics"))
        >>> arr (filter (not . isSuffixOf "uncategorized"))
        >>> arr concat
    }

-- Does Not Play Well With Others
doesNotPlayWellWithOthers = exploitationNow
    { comicName = "Does Not Play Well With Others"
    , seedPage = "http://www.doesnotplaywellwithothers.com/comics/pwc-000f"
    , comicFileName = \vol url ->
        let base = FPO.decodeString "./does_not_play_well_with_others"
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
        in base FPO.</> file
    }

exploitationNowVol :: String -> String
exploitationNowVol "single-category-act-one"         = "vol-1_act-one"
exploitationNowVol "single-category-act-two"         = "vol-2_act-two"
exploitationNowVol ""                                = "vol-3"
exploitationNowVol "single-category-intermission-i"  = "vol-4_intermission-I"
exploitationNowVol "single-category-act-three"       = "vol-5_act-three"
exploitationNowVol "single-category-intermission-ii" = "vol-6_intermission-II"
exploitationNowVol "single-category-act-four"        = "vol-7_act-four"
exploitationNowVol _ = "Unknown"


--
-- INDEX BASED COMIC
--


--    { comicName :: String
--    , seedPage :: String
--    , nextPage :: (ArrowXml a) => a XmlTree String
--    , comic :: (ArrowXml a) => a XmlTree String
--    , comicFileName :: String -> String -> FPO.FilePath
--    -- Identify act (vol 1, 2) via body (class) - single-category-act-four ...
--    , whichVolChp :: (ArrowXml a) => a XmlTree String
--    }
-- TODO:
--  * The page by page works for a raw dump but it does not work for vol/chp/etc
--  * Need to work/create an indexer that will walk the vol/chp tree of the archive
errantStory = Comic
    { comicName = "Errant Story"
--    , seedPage = "http://www.errantstory.com/2002-11-04/15"
--    , seedPage = "http://www.errantstory.com/category/comics/errant-story/v-01/prologue"
    , seedPage = "http://www.errantstory.com/category/comics/errant-story/v-01/chapter-04"
    , nextPage =
        hasName "h4"
        >>> hasAttrValue "class" (isInfixOf "nav-next")
        >>> getChildren
        >>> hasName "a"
        >>> hasAttr "href"
        >>> getAttrValue "href"
    , comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    , comicFileName = \filepath url ->
        let base = FPO.decodeString "./errant_story"
            fp   = FPO.decodeString filepath
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
        in base FPO.</> fp FPO.</> file
    , whichVolChp =
        hasName "body"
        >>> hasAttr "class"
        >>> getAttrValue "class"
        >>> arr words
        >>> arr (filter (isPrefixOf "single-category"))
        >>> arr (filter (not . isSuffixOf "comic"))
        >>> arr (filter (not . isSuffixOf "comics"))
        >>> arr (filter (not . isSuffixOf "uncategorized"))
        >>> arr concat
    }


-- TODO:
--  - Defined stop point, Errant Story
main = do
    let target = errantStory

    -- Queues for processing stuff
    -- TODO: look into tweaking this and making the indexed parser not deadlock the whole thing... if there's more to add to the queue than can be processed
    toFetch <- atomically $ newTBMChan 10000
    toReturn <- atomically $ newTBMChan 10000

    -- Seed with an initial page
    atomically $ writeTBMChan toFetch $ Webpage (seedPage target) Index -- Exploitation now/others are Serial

    -- Start the fetcher
    threadId <- forkIO $ fetch toFetch toReturn

    -- Do processing by pulling off each entry off the toReturn and submitting more
--    untilM_ (parser target toReturn toFetch) id
    untilM_ (indexedParser target toReturn toFetch) id

    -- We're done kill it
    killThread threadId



indexList :: (ArrowXml a) => a XmlTree (String, (String, String))
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


--chapterList :: (ArrowXml a) => a XmlTree String
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


indexNextPage =
    hasName "div"
    >>> hasAttrValue "class" (== "pagenav-right")
    >>> getChildren
    >>> hasName "a"
    >>> hasAttr "href"
    >>> getAttrValue "href"


-- TODO:
--  1. Data structure!
--  2. I think i want to tag the type of page that it is when i send it to the fetcher
--  3. Fetcher should either dump to disk if its a "file/image" type, otherwise return back the content with the tagged type
--  4. this means that we will be able to have multiple stage/multiple types of parsers and in theory if the tags are done right
--  5. they could be done in parallel.

-- Data type of the url and any additional info needed
type Url = String

data FetchType  = Webpage Url Tag
                | Image Url FPO.FilePath

data ReplyType  = WebpageReply UL.ByteString Tag


-- Additional information tags to tag on a webpage Request
data Tag = Serial  -- Page by page fetching
         | Index   -- Index page
         | Volume  -- Entire volume page
         | Page FPO.FilePath   -- single comic page
         | Chapter FPO.FilePath -- Entire chapters page


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
        in buildUrlAndFilePathMapping (root FPO.</> (FPO.decodeString name)) ours ++ buildUrlAndFilePathMapping (root) rest
    where
        chapterMapping root (url, (_, name)) = (url, root FPO.</> (FPO.decodeString name))


indexedParser :: Comic -> TBMChan ReplyType -> TBMChan FetchType -> IO Bool
indexedParser c i o = do
    r <- atomically $ readTBMChan i
    case r of
        Nothing -> return False
        (Just (WebpageReply html Index)) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html

            index <- runX $ doc //> indexList
            -- HXT

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

            chp <- runX $ doc //> chapterList
            next <- runX $ doc //> indexNextPage
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

            -- Terminate if we decide there's no more nextPage to fetch
            -- This does not work if there's multiple parser/worker going but it'll be ok for this poc
--            Control.Monad.when (null filteredNext) $ atomically $ closeTBMChan o

            -- Do we have any comic we want to store to disk?
            putStrLn "Fetched Urls:"
            mapM_ putStrLn img

            -- We do want to keep going cos we just submitted another page to fetch
            return True


        (Just (WebpageReply html _)) -> do
            -- We do want to keep going cos we just submitted another page to fetch
            return False



-- LINEAR comic parser/scanner
-- TODO:
--  * Make this work with indexed comic, if not need a second type
--  * look into some form of state transformer monad for tracking state between parse run if needed
parser :: Comic -> TBMChan ReplyType -> TBMChan FetchType -> IO Bool
parser c i o = do
    r <- atomically $ readTBMChan i
    case r of
        Nothing -> return False
        (Just (WebpageReply html _)) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
            next <- runX $ doc //> (nextPage c)

            img <- runX $ doc //> (comic c)

            vol <- runX $ doc //> (whichVolChp c)
            -- HXT

            -- Errant Story (Bail out when the next webpage matches this)
            let filteredNext = filter (/= "http://www.errantstory.com/2012-03-23/5460") next

            atomically $ mapM_ (writeTBMChan o) (map (\a -> Webpage a Serial) filteredNext)
            atomically $ mapM_ (writeTBMChan o) (map (\a -> Image a $ (comicFileName c) (concat vol) a) img)

            -- Terminate if we decide there's no more nextPage to fetch
            -- This does not work if there's multiple parser/worker going but it'll be ok for this poc
            Control.Monad.when (null filteredNext) $ atomically $ closeTBMChan o

            -- Do we have any comic we want to store to disk?
            putStrLn "Fetched Urls:"
            mapM_ putStrLn img
            mapM_ putStrLn filteredNext

            putStrLn "vol:"
            mapM_ putStrLn vol

            -- We do want to keep going cos we just submitted another page to fetch
            return True




fetch :: TBMChan FetchType -> TBMChan ReplyType -> IO ()
fetch i o = withSocketsDo $ E.bracket
    (H.newManager H.def)
    H.closeManager
    (\manager ->
        -- Forever loop (probably don't need the forever at all)
        forever $ runResourceT $ conduitFetcher manager i o
    )




conduitFetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> TBMChan FetchType -> TBMChan ReplyType -> m ()
conduitFetcher m i o = sourceTBMChan i $= CL.mapMaybeM (fetcher m) $$ sinkTBMChan o


conduitFetcherList :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> [FetchType] -> m [ReplyType]
conduitFetcherList m i = CL.sourceList i $= CL.mapMaybeM (fetcher m) $$ CL.consume


fetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> FetchType -> m (Maybe ReplyType)
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
    Failure H.HttpException m
    ) => H.Manager -> String -> m UL.ByteString
fetchSource m url = do
    response <- fetchStream m url
    chunk <- response C.$$+- CL.consume
    return $ L.fromChunks chunk


fetchToDisk :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> String -> FPO.FilePath -> m ()
fetchToDisk m url file = do
    -- TODO: Replace this with Network.HTTP.Conduit.Downloader probably for streaming file to disk
    response <- fetchStream m url

    -- Let's create the directory tree if it does not exist first
    liftIO $ createTree $ FP.directory file

    response C.$$+- sinkFile file


fetchStream :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> String -> m (ResumableSource m S.ByteString)
fetchStream m url = do
    req' <- H.parseUrl url
    let req = req' { H.checkStatus = \_ _ _ -> Nothing }

    -- Caching hook here
    --  1. Check for cache value
    --  2. Return cached value if any exists, otherwise
    --  3. Fetch the http request then stream it to the cache
    --  4. Return cached value

    exists <- liftIO $ cacheExists url
    unless exists $ do
        response <- H.http req m
        H.responseBody response C.$$+- cacheSink url

        -- Stall the read for the prerequest wait time before moving ahead
        liftIO $ threadDelay $ 1000000 * fetchWaitTime

    cacheSource url

cacheExists :: String -> IO Bool
cacheExists = isFile . cacheFile

cacheSource :: MonadResource m => String -> m (ResumableSource m S.ByteString)
cacheSource url = do
    (a, b) <- (sourceFile $ cacheFile url) C.$$+ CL.take 0
    return a

cacheSink :: MonadResource m => String -> Sink S.ByteString m ()
cacheSink url = do
    let fp = cacheFile url

    -- Let's create the cache if it does not exist.
    liftIO $ createTree $ FP.directory fp

    sinkFile fp


cacheFile :: String -> FPO.FilePath
cacheFile url = FPO.decodeString "./cache" FPO.</> (FPO.decode $ digestToHexByteString $ (hash $ US.fromString url :: Digest SHA512))




-- Execute till result is false
untilM_ :: (Monad m) => m a -> (a -> Bool) -> m ()
untilM_ f p = do
    x <- f
    if p x
        then untilM_ f p
        else return ()
