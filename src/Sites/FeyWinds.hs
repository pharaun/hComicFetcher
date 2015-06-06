{-# LANGUAGE GADTs #-}
module Sites.FeyWinds
    ( feyWinds
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes)

import qualified Data.List as DL

import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as US
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as UL

-- Local imports
import Types

-- Operational monad
import Control.Monad
import Control.Monad.Operational
import Control.Monad.IO.Class

-- Callback management
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

-- Tagsoup
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast

-- Tags
data CTag = Initial -- Probably can just ingest and throw this away then start the WebFetchT monad


rootPage = "http://feywinds.com/comic.html"
feyWinds = Comic
    { comicName = "Fey Winds"
    , seedPage = rootPage
    , seedType = Initial

    , pageParse = feyWindsProxy
    }

--
-- Reinversion/Operational
--
data WebFetchI a where
    FetchWebpage :: [String] -> WebFetchI String
    FetchImage :: String -> WebFetchI ()
    Debug :: (Show s) => s -> WebFetchI ()

type WebFetchT m a = ProgramT WebFetchI m a

runWebFetchT :: (MonadIO m, Monad m) => WebFetchT m () -> m ()
runWebFetchT = eval <=< viewT
  where
    eval :: (MonadIO m, Monad m) => ProgramViewT WebFetchI m () -> m ()
    eval (Return _)                 = return ()

    eval (FetchWebpage us :>>= k)   =
        forM_ us (\u -> do
            b <- liftIO $ fetchWebpage u
            runWebFetchT (k b))

    eval (FetchImage u :>>= k)      = do
        liftIO $ fetchImage u
        runWebFetchT (k ())

    eval (Debug s :>>= k)           = do
        liftIO $ print s
        runWebFetchT (k ())


fwp :: [String] -> WebFetchT m String
fwp = singleton . FetchWebpage

fi :: String -> WebFetchT m ()
fi = singleton . FetchImage

debug :: (Show a) => a -> WebFetchT m ()
debug = singleton . Debug
--
-- Reinversion/Operational
--


feyWindsProxy :: ReplyType CTag -> IO [FetchType CTag]
feyWindsProxy (WebpageReply html Initial) = do
    print "Entering interpreter"

    let a = runWebFetchT feyWindsParser
    runCoroutine a

    print "Exiting interpreter"

    return []



fetchWebpage url = do
    let b = case url of
            "idx1"  -> "index1"
            "url1"  -> "content1"
            "url2"  -> "content2"
            "url3"  -> "content3"
            _       -> "Unknown"

    print b
    return b

fetchImage url = do
    let b = case url of
            "urla"  -> "contenta"
            "urlb"  -> "contentb"
            "urlc"  -> "contentc"
            _       -> "Unknown"

    print b
    return ()


--feyWindsParser :: WebFetchT IO ()
--feyWindsParser :: WebFetchT (Coroutine (Request String UL.ByteString) IO) ()
feyWindsParser :: WebFetchT (Coroutine (Naught) IO) ()
feyWindsParser = do
    debug "Fetching initial page"
    idx <- fwp [rootPage]

    debug "Parsing index page"


    return ()
