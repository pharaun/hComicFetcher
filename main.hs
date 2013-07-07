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

import Data.Maybe

import qualified Control.Exception as E

import Control.Failure


-- Exploitation Now
nextPage :: (ArrowXml a) => a XmlTree String
nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"

comic :: (ArrowXml a) => a XmlTree String
comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"


main = do
    let seed = "http://www.exploitationnow.com/2000-07-07/9"

    -- Queues for processing stuff
    toFetch <- atomically $ newTBMChan 10
    toReturn <- atomically $ newTBMChan 10

    -- Seed with an initial page
    atomically $ writeTBMChan toFetch $ Webpage seed

    -- Launch the threaded fetcher for running the toFetch Channel
    -- TODO: refactor this into its own function
--    forkIO $ E.bracket (H.newManager H.def) H.closeManager (\manager -> withSocketsDo $ forever $ conduitFetcher manager toFetch toReturn)

    -- Do processing by pulling off each entry off the toReturn and submitting more
--    untilM_ (parser toReturn toFetch) id



--parser :: TBMChan L.ByteString -> TBMChan FetchType -> IO Bool
parser i o = do
    r <- atomically $ readTBMChan i
    case r of
        (Just html) -> do
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
            next <- runX $ doc //> nextPage
            atomically $ mapM_ (writeTBMChan o) (map Webpage next)

            -- Do we have any comic we want to store to disk?
            img <- runX $ doc //> comic
            putStrLn "Comics:"
            mapM_ putStrLn img

            -- Print the url so we know whats up
            mapM_ putStrLn next

            -- We do want to keep going cos we just submitted another page to fetch
            return True

        Nothing -> return False



--conduitFetcher :: H.Manager -> TBMChan FetchType -> TBMChan L.ByteString -> IO ()
conduitFetcher m i o = runResourceT $ sourceTBMChan i $$ conduitFetch m =$ sinkTBMChan o

--conduitFetch :: MonadIO m => H.Manager -> Conduit FetchType m L.ByteString
conduitFetch m = awaitForever (\a -> do
    b <- fetcher m a
    case b of
        (Just c) -> C.yield c
        Nothing  -> return ()
    )

-- Data type of the url and any additional info needed
data FetchType  = Webpage String
                | Comic String String -- Url & Filename



--fetcher :: MonadIO m => H.Manager -> FetchType -> m (Maybe L.ByteString)
fetcher m (Webpage u) = do
    reply <- H.simpleHttp u
    -- TODO: figure out how to deal with this one
    return $ Nothing
fetcher m (Comic u f) = do
--    reply <- H.simpleHttp u
    -- Stream to disk
    return $ Nothing




fetchSource :: (MonadResource m, MonadBaseControl IO m, Failure H.HttpException m) => H.Manager -> String -> m UL.ByteString
fetchSource m url = do
    req' <- H.parseUrl url
    let req = req' { H.checkStatus = \_ _ _ -> Nothing }
    response <- H.http req m
    chunk <- H.responseBody response C.$$+- CL.consume
    return $ L.fromChunks chunk

fetchToDisk :: (MonadResource m, MonadBaseControl IO m, Failure H.HttpException m) => H.Manager -> String -> FilePath -> m ()
fetchToDisk m url file = do
    req' <- H.parseUrl url
    let req = req' { H.checkStatus = \_ _ _ -> Nothing }
    response <- H.http req m
    H.responseBody response C.$$+- sinkFile file





-- Execute till result is false
untilM_ :: (Monad m) => m a -> (a -> Bool) -> m ()
untilM_ f p = do
    x <- f
    if p x
        then untilM_ f p
        else return ()
