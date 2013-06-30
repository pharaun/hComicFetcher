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
    putStrLn ""
    putStrLn "Next Page:"
    mapM_ putStrLn next

    -- stm chans
    toFetch <- atomically $ newTBMChan 10
    toReturn <- atomically $ newTBMChan 10

    -- We have a list of image and next page, let's fetch them.
    runResourceT $ DCL.sourceList (map Webpage next) $$ sinkTBMChan toFetch

    -- Launch the threaded fetcher for running the toFetch Channel
    forkIO $ fetchChan toFetch toReturn

    -- Read data out of the toReturn channel and do stuff with it
    runResourceT $ sourceTBMChan toReturn $$ DCL.mapM_ (\a -> do
            let doc = map (readString [withParseHTML yes, withWarnings no] . UL.toString) (maybeToList a)
            next <- mapM (\b -> liftIO $ runX $ b //> nextPage) doc
            liftIO $ putStrLn ""
            liftIO $ putStrLn "Channel http:"
            mapM_ (liftIO . putStrLn) (concat next)
        )



fetchChan :: TBMChan FetchType -> TBMChan (Maybe L.ByteString) -> IO ()
fetchChan i o = runResourceT $ forever $ sourceTBMChan i $$ fetching =$ sinkTBMChan o

fetching :: MonadIO m => Conduit FetchType m (Maybe L.ByteString)
fetching = do
    i <- await
    case i of
        (Just x) -> do
            val <- liftIO $ withSocketsDo $ fetcher x
            C.yield val
        _ -> return ()


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
