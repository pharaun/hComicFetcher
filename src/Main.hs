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

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TBMChan
import Control.Monad.STM (atomically)
import qualified Control.Monad as CM

-- Local imports
import Types
import Fetcher

import Sites.Comic

-- TODO:
--  - Defined stop point, Errant Story
--  - Some command line arg for picking which comic to run
main = do
    -- TODO: broken
--    processTarget gunnerkrigCourt
--    processTarget errantStory

    -- Functional
--    processTarget doesNotPlayWellWithOthers
--    processTarget exploitationNow
--    processTarget girlGenius
--    processTarget $ batoto {seedPage = "http://www.batoto.net/comic/_/comics/yotsubato-r311"}
--    processTarget amyaChronicles
--    processTarget freakAngels
--    processTarget denizensAttention
--    processTarget disenchanted
--    processTarget tryingHuman
    processTarget feyWinds

  where
    -- TODO: Redo this so that it can accept multiple calls for each target
    processTarget :: Comic a -> IO ()
    processTarget c@Comic{seedPage=seedPage, seedType=seedType} = do
        -- Queues for processing stuff
        -- TODO: look into tweaking this and making the indexed parser not deadlock the whole thing... if there's more to add to the queue than can be processed
        toFetch <- atomically $ newTBMChan 10000
        toReturn <- atomically $ newTBMChan 10000

        -- Seed with an initial page
        atomically $ writeTBMChan toFetch $ Webpage seedPage seedType

        -- Start the fetcher
        threadId <- forkIO $ fetch toFetch toReturn

        -- Do processing by pulling off each entry off the toReturn and submitting more
        untilM_ (indexedParser c toReturn toFetch) id

        -- We're done kill it
        killThread threadId



-- Indexer parser,
-- The mother of all parser, it parses various Tagged pages and then go from there
-- TODO:
--  * look into some form of state transformer monad for tracking state between parse run if needed
--  * This ^ is probably the Tag, which let us tag specific page with additional information if its needed
indexedParser :: Comic a -> TBMChan (ReplyType a) -> TBMChan (FetchType a) -> IO Bool
indexedParser Comic{pageParse=parse} i o = do
    r <- atomically $ readTBMChan i
    case r of
        Nothing -> return False

        Just x -> do
            -- Parse the page
            nextFetch <- parse x

            -- Terminate if we decide there's no more nextPage to fetch
            -- This does not work atm
--            CM.when (null nextFetch) $ atomically $ closeTBMChan o

            -- Fetch them
            atomically $ mapM_ (writeTBMChan o) nextFetch

            -- We do want to keep going cos we just submitted another page to fetch
            return True


-- Execute till result is false
untilM_ :: (Monad m) => m a -> (a -> Bool) -> m ()
untilM_ f p = do
    x <- f
    CM.when (p x) $ untilM_ f p
