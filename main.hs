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
import Control.Concurrent
import Data.Conduit
import qualified Data.Conduit.List as DCL
import Data.Conduit.TMChan
import Data.Conduit.Binary (sinkFile)
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
    html <- readFile "test.html"
    let doc = readString [withParseHTML yes, withWarnings no] html

    -- Image
    img <- runX $ doc //> comic
    putStrLn "Comics:"
    mapM_ putStrLn img

    -- Navi
    next <- runX $ doc //> nextPage
    putStrLn "Next Page:"
    mapM_ putStrLn next

    -- stm chans
    toFetch <- atomically $ newTBMChan 10
    toReturn <- atomically $ newTBMChan 10

    -- Pre-seed the toFetch Chan
    atomically $ mapM_ (writeTBMChan toFetch) (map Webpage next)
    atomically $ mapM_ (writeTBMChan toFetch) (map Webpage next)

    -- Pre-seed the toReturn chan
    atomically $ writeTBMChan toReturn (Just $ UL.fromString html)
    atomically $ writeTBMChan toReturn (Just $ UL.fromString html)



    -- Launch the threaded fetcher for running the toFetch Channel
    a <- forkIO $ fetchChan toFetch toReturn

    -- We have a list of image and next page, let's fetch them.
    b <- forkIO $ parseChan toReturn toFetch

    threadDelay 100000

    a <- atomically $ isClosedTBMChan toFetch
    putStrLn $ show a
    a <- atomically $ isEmptyTBMChan toFetch
    putStrLn $ show a
    a <- atomically $ isClosedTBMChan toReturn
    putStrLn $ show a
    a <- atomically $ isEmptyTBMChan toReturn
    putStrLn $ show a

-- Todo
--  May need to update these to be Sinks instead that then inserts manually into the other's chan

parseChan :: TBMChan (Maybe L.ByteString) -> TBMChan FetchType -> IO ()
parseChan i o = runResourceT $ sourceTBMChan i $$ parsing =$ sinkTBMChan o

parsing :: MonadIO m => Conduit (Maybe L.ByteString) m FetchType
parsing = do
    liftIO $ putStrLn "1"
    i <- await
    liftIO $ putStrLn "2"
    case i of
        (Just x) -> case x of
            (Just y) -> do
                let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString y
                next <- liftIO $ runX $ doc //> nextPage
                liftIO $ putStrLn ""
                liftIO $ putStrLn "Channel http:"
                liftIO $ mapM_ putStrLn next
                mapM_ (\a -> C.yieldOr a (liftIO $ putStrLn "Died parsing")) (map Webpage next)
            _ -> do
                liftIO $ putStrLn "Empty bytestring"
                return ()
        _ -> do
            liftIO $ putStrLn "No more data"
            return ()

fetchChan :: TBMChan FetchType -> TBMChan (Maybe L.ByteString) -> IO ()
fetchChan i o = runResourceT $ sourceTBMChan i $$ fetching =$ sinkTBMChan o

fetching :: MonadIO m => Conduit FetchType m (Maybe L.ByteString)
fetching = do
    liftIO $ putStrLn "3"
    i <- await
    liftIO $ putStrLn "4"
    case i of
        (Just x) -> do
            val <- liftIO $ withSocketsDo $ fetcher x
            C.yieldOr val (liftIO $ putStrLn "died fetching")
        _ -> do
            liftIO $ putStrLn "No more data to fetch"
            return ()


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

{-
    Simple version:
        List of input url
        emits into an conduit which then does http fetch stuff
        then that either dump the file to disk or send it to hxt for parsing
-}
