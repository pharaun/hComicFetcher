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

import Control.Monad.Loops
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM.TBMChan
import Control.Monad.STM (atomically)
import qualified Control.Monad as CM
import Pipes

-- Local imports
import Types
import Fetcher

import Sites.Comic


-- TODO:
--  - Defined stop point, Errant Story
--  - Some command line arg for picking which comic to run
main = do
    -- TODO: broken
--    pipelineTarget gunnerkrigCourt
--    pipelineTarget errantStory

    -- Functional
--    pipelineTarget doesNotPlayWellWithOthers
--    pipelineTarget exploitationNow
--    pipelineTarget girlGenius
--    pipelineTarget $ batoto {seedPage = "http://www.batoto.net/comic/_/comics/yotsubato-r311"}
--    pipelineTarget amyaChronicles
--    pipelineTarget freakAngels
--    pipelineTarget denizensAttention
--    pipelineTarget disenchanted
--    pipelineTarget tryingHuman

    -- New pipeline parsers
    pipelineTarget feyWinds


--
-- Backward Compat parser target
--
pipelineTarget :: Comic a -> IO ()
pipelineTarget Comic{seedPage=seedPage', seedType=seedType', pageParse=parse, cookies=cookie} = do
    -- Queues for processing stuff
    -- TODO: look into tweaking this and making the indexed parser not deadlock the whole thing... if there's more to add to the queue than can be processed
    toFetch <- atomically $ newTBMChan 10000
    toReturn <- atomically $ newTBMChan 10000

    -- Seed with an initial page
    atomically $ writeTBMChan toFetch $ Webpage seedPage' seedType'

    -- Start the fetcher
    threadId <- forkIO $ fetch cookie toFetch toReturn

    -- Pipeline parser
    runEffect $ (chanProducer toReturn) >-> parse >-> (chanConsumer toFetch)

    -- TODO: Wait till queue is empty
    -- - Hacky, need to have a check from queue empty
    -- - Fetcher is done (downloading + write to disk)
    untilM_ (liftIO $ threadDelay $ 1000000) (atomically $ isEmptyTBMChan toFetch)
    liftIO $ threadDelay $ 1000000 * 10 -- Wait till image is written to disk

    -- We're done kill it
    killThread threadId

chanProducer :: (MonadIO m) => TBMChan (ReplyType a) -> Producer (ReplyType a) m ()
chanProducer i = do
    r <- liftIO $ atomically $ readTBMChan i
    case r of
        Nothing -> return ()
        Just x  -> yield x >> chanProducer i

chanConsumer :: (MonadIO m) => TBMChan (FetchType a) -> Consumer (FetchType a) m ()
chanConsumer o = CM.forever $ do
    r <- await
    liftIO $ atomically $ writeTBMChan o r
