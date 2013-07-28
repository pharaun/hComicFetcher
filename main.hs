{-# LANGUAGE FlexibleContexts #-}
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

    3. Requirements?
        a. Identify what qualifies as a Volume, Chapter, Page
        b. Identify how to process/fetch "next" page, chapter, volume
        c. Identify how to download and store each to disk in a scheme that makes sense
        d. How to specifiy what to be fetched (Url of the said series?) (Site?)
            a. Typeclass
                - nextPage
                - comic
                - volume, chapter
-}
import Data.Maybe
import Data.List
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


-- Exploitation Now
nextPage :: (ArrowXml a) => a XmlTree String
nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"

comic :: (ArrowXml a) => a XmlTree String
comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"

comicFileName :: String -> String -> FPO.FilePath
comicFileName vol url =
    let base = FPO.decodeString "./exploitation_now"
        file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
        dirs = FPO.fromText $ T.pack $ exploitationNowVol vol
    in base FPO.</> dirs FPO.</> file

exploitationNowVol :: String -> String
exploitationNowVol "single-category-act-one"         = "vol-1_act-one"
exploitationNowVol "single-category-act-two"         = "vol-2_act-two"
exploitationNowVol ""                                = "vol-3"
exploitationNowVol "single-category-intermission-i"  = "vol-4_intermission-I"
exploitationNowVol "single-category-act-three"       = "vol-5_act-three"
exploitationNowVol "single-category-intermission-ii" = "vol-6_intermission-II"
exploitationNowVol "single-category-act-four"        = "vol-7_act-four"
exploitationNowVol _ = "Unknown"

-- Seconds to wait between each request to this site
fetchWaitTime :: Int
fetchWaitTime = 1

-- Identify act (vol 1, 2) via body (class) - single-category-act-four ...
whichVolChp :: (ArrowXml a) => a XmlTree String
whichVolChp =
    hasName "body"
    >>> hasAttr "class"
    >>> getAttrValue "class"
    >>> arr words
    >>> arr (filter (isPrefixOf "single-category"))
    >>> arr (filter (not . isSuffixOf "comic"))
    >>> arr (filter (not . isSuffixOf "uncategorized"))
    >>> arr concat



main = do
    let seed = "http://www.exploitationnow.com/2000-07-07/9"

    -- Queues for processing stuff
    toFetch <- atomically $ newTBMChan 10
    toReturn <- atomically $ newTBMChan 10

    -- Seed with an initial page
    atomically $ writeTBMChan toFetch $ Webpage seed

    -- Start the fetcher
    threadId <- forkIO $ fetch toFetch toReturn

    -- Do processing by pulling off each entry off the toReturn and submitting more
    untilM_ (parser toReturn toFetch) id
--    replicateM 10 (parser toReturn toFetch)

    -- We're done kill it
    killThread threadId


{-
     701,039,680 bytes allocated in the heap
      88,742,552 bytes copied during GC
       1,489,304 bytes maximum residency (32 sample(s))
          57,288 bytes maximum slop
               5 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0      1325 colls,     0 par    0.11s    0.11s     0.0001s    0.0006s
  Gen  1        32 colls,     0 par    0.06s    0.06s     0.0020s    0.0029s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    0.25s  ( 10.26s elapsed)
  GC      time    0.18s  (  0.18s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    0.43s  ( 10.43s elapsed)

  %GC     time      41.2%  (1.7% elapsed)

  Alloc rate    2,804,729,796 bytes per MUT second

  Productivity  58.8% of total user, 2.4% of total elapsed

-}


parser :: TBMChan UL.ByteString -> TBMChan FetchType -> IO Bool
parser i o = do
    r <- atomically $ readTBMChan i
    case r of
        (Just html) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
            next <- runX $ doc //> nextPage

            img <- runX $ doc //> comic

            vol <- runX $ doc //> whichVolChp
            -- HXT

            atomically $ mapM_ (writeTBMChan o) (map Webpage next)
            atomically $ mapM_ (writeTBMChan o) (map (\a -> Comic a $ comicFileName (concat vol) a) img)

            -- Terminate if we decide there's no more nextPage to fetch
            -- This does not work if there's multiple parser/worker going but it'll be ok for this poc
            Control.Monad.when (null next) $ atomically $ closeTBMChan o

            -- Do we have any comic we want to store to disk?
            putStrLn "Fetched Urls:"
            mapM_ putStrLn next
            mapM_ putStrLn img

            -- We do want to keep going cos we just submitted another page to fetch
            return True

        Nothing -> return False



fetch :: TBMChan FetchType -> TBMChan UL.ByteString -> IO ()
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
    ) => H.Manager -> TBMChan FetchType -> TBMChan UL.ByteString -> m ()
conduitFetcher m i o = sourceTBMChan i $= CL.mapMaybeM (fetcher m) $$ sinkTBMChan o


conduitFetcherList :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> [FetchType] -> m [UL.ByteString]
conduitFetcherList m i = CL.sourceList i $= CL.mapMaybeM (fetcher m) $$ CL.consume


-- Data type of the url and any additional info needed
data FetchType  = Webpage String
                | Comic String FPO.FilePath -- Url & Filename


fetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> FetchType -> m (Maybe UL.ByteString)
fetcher m (Webpage u) = do
    reply <- fetchSource m u
    return $ Just reply
fetcher m (Comic u f) = do
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
