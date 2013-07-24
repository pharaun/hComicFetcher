{-# LANGUAGE FlexibleContexts #-}
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
import Data.Conduit.Binary

import qualified Data.Conduit.List as CL

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

import Data.Maybe

import qualified Control.Exception as E

import Control.Failure

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath, encodeString, decodeString, fromText, (</>))

import Network.HTTP.Types.URI


-- Exploitation Now
nextPage :: (ArrowXml a) => a XmlTree String
nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"

comic :: (ArrowXml a) => a XmlTree String
comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"

comicFileName :: String -> FilePath
comicFileName url = decodeString "./exploitation_now" </> (fromText $ last $ decodePathSegments $ US.fromString url)

-- Seconds to wait between each request to this site
fetchWaitTime :: Integer
fetchWaitTime = 1


main = do
    let seed = "http://www.exploitationnow.com/2000-07-07/9"

    -- Queues for processing stuff
    toFetch <- atomically $ newTBMChan 10
    toReturn <- atomically $ newTBMChan 10

    -- Seed with an initial page
    atomically $ writeTBMChan toFetch $ Webpage seed

    -- Start the fetcher
    forkIO $ fetch toFetch toReturn

    -- Do processing by pulling off each entry off the toReturn and submitting more
    untilM_ (parser toReturn toFetch) id



parser :: TBMChan UL.ByteString -> TBMChan FetchType -> IO Bool
parser i o = do
    r <- atomically $ readTBMChan i
    case r of
        (Just html) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
            next <- runX $ doc //> nextPage

            img <- runX $ doc //> comic
            -- HXT

            atomically $ mapM_ (writeTBMChan o) (map Webpage next)
            atomically $ mapM_ (writeTBMChan o) (map (\a -> Comic a $ comicFileName a) img)

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
                | Comic String FilePath -- Url & Filename


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
    ) => H.Manager -> String -> FilePath -> m ()
fetchToDisk m url file = do
    -- TODO: Replace this with Network.HTTP.Conduit.Downloader probably for streaming file to disk
    response <- fetchStream m url
    response C.$$+- sinkFile $ encodeString file


fetchStream :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> String -> m (ResumableSource m S.ByteString)
fetchStream m url = do
    req' <- H.parseUrl url
    let req = req' { H.checkStatus = \_ _ _ -> Nothing }

    -- Caching hook here
    response <- H.http req m
    return $ H.responseBody response



fetchStreamCache :: String -> m (ResumableSource m S.ByteString)
fetchStreamCache = undefined



-- Execute till result is false
untilM_ :: (Monad m) => m a -> (a -> Bool) -> m ()
untilM_ f p = do
    x <- f
    if p x
        then untilM_ f p
        else return ()
