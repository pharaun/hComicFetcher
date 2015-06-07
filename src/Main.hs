{-# LANGUAGE FlexibleContexts, GADTs #-}
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

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TBMChan
import Control.Monad.STM (atomically)
import qualified Control.Monad as CM

-- Local imports
import Types
import Fetcher

import Sites.Comic

--
-- Fey Winds
--
-- Operational monad
import Control.Monad
import Control.Monad.Operational
import Control.Monad.IO.Class

-- Tagsoup
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast

-- Pipes
import Pipes

import qualified Data.ByteString.Lazy.UTF8 as UL
--
-- Fey Winds
--


-- TODO:
--  - Defined stop point, Errant Story
--  - Some command line arg for picking which comic to run
main = do
    -- TODO: broken
--    backwardTarget gunnerkrigCourt
--    backwardTarget errantStory

    -- Functional
--    backwardTarget doesNotPlayWellWithOthers
--    backwardTarget exploitationNow
--    backwardTarget girlGenius
--    backwardTarget $ batoto {seedPage = "http://www.batoto.net/comic/_/comics/yotsubato-r311"}
--    backwardTarget amyaChronicles
--    backwardTarget freakAngels
--    backwardTarget denizensAttention
--    backwardTarget disenchanted
    backwardTarget tryingHuman

    -- New pipeline parsers
    pipelineTarget feyWinds


--
-- Backward Compat parser target
--
backwardTarget :: Comic a -> IO ()
backwardTarget Comic{seedPage=seedPage, seedType=seedType, pageParse=parse} = do

    -- Queues for processing stuff
    -- TODO: look into tweaking this and making the indexed parser not deadlock the whole thing... if there's more to add to the queue than can be processed
    toFetch <- atomically $ newTBMChan 10000
    toReturn <- atomically $ newTBMChan 10000

    -- Seed with an initial page
    atomically $ writeTBMChan toFetch $ Webpage seedPage seedType

    -- Start the fetcher
    threadId <- forkIO $ fetch toFetch toReturn

    -- Pipeline parser
    runEffect $ (chanProducer toReturn) >-> (toPipeline parse) >-> (chanConsumer toFetch)

    -- We're done kill it
    killThread threadId

toPipeline :: (ReplyType t -> IO [FetchType t]) -> Pipe (ReplyType t) (FetchType t) IO ()
toPipeline old = forever $ do
    a <- await
    b <- liftIO $ old a
    mapM_ yield b

--
-- New pipeline parser target
--
pipelineTarget :: Comic2 CTag -> IO ()
pipelineTarget Comic2{seedPage2=seedPage, seedType2=seedType, pipeParse=parse} = do

    -- Queues for processing stuff
    -- TODO: look into tweaking this and making the indexed parser not deadlock the whole thing... if there's more to add to the queue than can be processed
    toFetch <- atomically $ newTBMChan 10000
    toReturn <- atomically $ newTBMChan 10000

    -- Seed with an initial page
    atomically $ writeTBMChan toFetch $ Webpage seedPage seedType

    -- Start the fetcher
    threadId <- forkIO $ fetch toFetch toReturn

    -- TODO: string up the pipeline (toReturn is input, toFetch is output)
    runEffect $ (chanProducer toReturn) >-> parse >-> (chanConsumer toFetch)

    -- We're done kill it
    killThread threadId

chanProducer :: (MonadIO m) => TBMChan (ReplyType a) -> Producer (ReplyType a) m ()
chanProducer i = do
    r <- liftIO $ atomically $ readTBMChan i
    case r of
        Nothing -> return ()
        Just x  -> yield x >> chanProducer i

chanConsumer :: (MonadIO m) => TBMChan (FetchType a) -> Consumer (FetchType a) m ()
chanConsumer o = forever $ do
    r <- await
    liftIO $ atomically $ writeTBMChan o r






data CTag = Initial
    deriving (Show)

-- New comic type for pipeline targets
data Comic2 t = Comic2
    { comicName2 :: String
    , seedPage2 :: String
    , seedType2 :: t

    -- Page parser, Parse a page and return a list of stuff to fetch,
--    , pageParse :: ReplyType t -> IO [FetchType t]

    -- Pipeline parser (takes an input stream and output stream of stuff to fetch
    , pipeParse :: Pipe (ReplyType t) (FetchType t) IO ()
    }


rootPage = "http://feywinds.com/comic.html"
feyWinds = Comic2
    { comicName2 = "Fey Winds"
    , seedPage2 = rootPage
    , seedType2 = Initial

    , pipeParse = feyWindsProxy
    }

feyWindsProxy :: Pipe (ReplyType CTag) (FetchType CTag) IO ()
feyWindsProxy = runWebFetchT feyWindsParser


feyWindsParser :: WebFetchT (Pipe (ReplyType CTag) (FetchType CTag) IO) ()
feyWindsParser = do
    debug "Fetching initial page"
    idx <- fwp [rootPage]
    debug idx
    debug "Parsing index page"

    return ()


--
-- Reinversion/Operational
--
data WebFetchI a where
    FetchWebpage :: [Url] -> WebFetchI (ReplyType CTag)
    FetchImage :: Url -> WebFetchI () -- TODO: add comic tag here
    Debug :: (Show s) => s -> WebFetchI ()

type WebFetchT m a = ProgramT WebFetchI m a

runWebFetchT :: (MonadIO m, Monad m) => WebFetchT (Pipe (ReplyType CTag) (FetchType CTag) m) () -> Pipe (ReplyType CTag) (FetchType CTag) m ()
runWebFetchT = eval <=< viewT
  where
    eval :: (MonadIO m, Monad m) => ProgramViewT WebFetchI (Pipe (ReplyType CTag) (FetchType CTag) m) () -> Pipe (ReplyType CTag) (FetchType CTag) m ()
    eval (Return _)                 = return ()

    eval (FetchWebpage us :>>= k)   =
        forM_ us (\u -> (yield (Webpage u Initial)) >> await >>= \b -> runWebFetchT (k b))

    eval (FetchImage u :>>= k)      = (yield (Image u undefined)) >> runWebFetchT (k ())

    eval (Debug s :>>= k)           = do
        liftIO $ print s
        runWebFetchT (k ())


fwp :: [Url] -> WebFetchT m (ReplyType CTag)
fwp = singleton . FetchWebpage

fi :: Url -> WebFetchT m ()
fi = singleton . FetchImage

debug :: (Show a) => a -> WebFetchT m ()
debug = singleton . Debug
--
-- Reinversion/Operational
--
