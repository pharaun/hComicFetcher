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

import Control.Concurrent.STM.TBMQueue

import Control.Concurrent
import Data.Conduit
import qualified Network.HTTP.Conduit as H
import qualified Data.Conduit as C

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UL

import Data.Maybe


-- Exploitation Now
nextPage :: (ArrowXml a) => a XmlTree String
nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"

comic :: (ArrowXml a) => a XmlTree String
comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"


main = do
    let seed = "http://www.exploitationnow.com/2000-07-07/9"

    -- Queues for processing stuff
    toFetch <- atomically $ newTBMQueue 10
    toReturn <- atomically $ newTBMQueue 10

    -- Seed with an initial page
    atomically $ writeTBMQueue toFetch $ Webpage seed

    -- Launch the threaded fetcher for running the toFetch Channel
    forkIO $ forever $ fetchChan toFetch toReturn

    -- Do processing by pulling off each entry off the toReturn and submitting more
    untilM_ (parser toReturn toFetch) id




parser :: TBMQueue L.ByteString -> TBMQueue FetchType -> IO Bool
parser i o = do
    r <- atomically $ readTBMQueue i
    case r of
        (Just html) -> do
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
            next <- runX $ doc //> nextPage
            atomically $ mapM_ (writeTBMQueue o) (map Webpage next)

            -- Do we have any comic we want to store to disk?
            img <- runX $ doc //> comic
            putStrLn "Comics:"
            mapM_ putStrLn img

            -- Print the url so we know whats up
            mapM_ putStrLn next

            -- We do want to keep going cos we just submitted another page to fetch
            return True

        Nothing -> return False



fetchChan :: TBMQueue FetchType -> TBMQueue L.ByteString -> IO ()
fetchChan i o = do
    -- Fetch the request
    x <- atomically $ readTBMQueue i
    case x of
        (Just z) -> do
            a <- fetcher z
            case a of
                (Just b) -> do
                    atomically $ writeTBMQueue o b
                Nothing -> return ()
        Nothing -> return ()


-- Data type of the url and any additional info needed
data FetchType  = Webpage String
                | Comic String String -- Url & Filename

fetcher :: MonadIO m => FetchType -> m (Maybe L.ByteString)
fetcher (Webpage u) = do
    page <- H.simpleHttp u
    return $ Just page
fetcher (Comic u f) = do
    page <- H.simpleHttp u
    return $ Nothing



-- Execute till result is false
untilM_ :: (Monad m) => m a -> (a -> Bool) -> m ()
untilM_ f p = do
    x <- f
    if p x
        then untilM_ f p
        else return ()
