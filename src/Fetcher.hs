{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Fetcher
    ( fetch
    ) where

import Data.Conduit (($=), ($$), ($$+), ($$+-))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CF
import Conduit (MonadBaseControl, MonadResource, runResourceT)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TMChan as CT

import Network (withSocketsDo)
import Network.HTTP.Conduit (HttpException, Manager)
import qualified Network.HTTP.Conduit as CH

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TBMChan

import Control.Failure (Failure)
import Control.Monad.IO.Class (liftIO)

import qualified Control.Exception as E
import qualified Control.Monad as CM
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import qualified System.FilePath as FP

import Crypto.Hash (digestToHexByteString, hash, Digest, SHA512)

import qualified Data.List as DL


-- Local types
import Types
import Parser.VolChpPrinter


-- Seconds to wait between each request to this site
fetchWaitTime :: Int
fetchWaitTime = 1



-- TODO: restart if the exception kills -- main.hs: InvalidUrlException "/ggmain/doublespreads/extrabits/Gil.jpg" "Invalid URL"
fetch :: [CH.Cookie] -> TBMChan (FetchType a) -> TBMChan (ReplyType a) -> IO ()
fetch c i o = withSocketsDo $ E.bracket
    (CH.newManager CH.conduitManagerSettings)
    CH.closeManager
    (\manager ->
        -- Forever loop (probably don't need the forever at all)
        CM.forever $ runResourceT $ conduitFetcher c manager i o
    )




conduitFetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => [CH.Cookie] -> Manager -> TBMChan (FetchType a) -> TBMChan (ReplyType a) -> m ()
conduitFetcher c m i o = CT.sourceTBMChan i $= CL.mapMaybeM (fetcher c m) $$ CT.sinkTBMChan o True


conduitFetcherList :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => [CH.Cookie] -> Manager -> [FetchType a] -> m [ReplyType a]
conduitFetcherList c m i = CL.sourceList i $= CL.mapMaybeM (fetcher c m) $$ CL.consume


fetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => [CH.Cookie] -> Manager -> FetchType a -> m (Maybe (ReplyType a))
fetcher c m (Webpage u cached t) = do
    reply <- fetchSource c m cached u
    return $ Just (WebpageReply reply t)
fetcher c m (Image u f) = do
    -- Stream to disk
    fetchToDisk c m u (comicTagToFilePath f)
    return Nothing



fetchSource :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => [CH.Cookie] -> Manager -> Cached -> String -> m UL.ByteString
fetchSource c m cached url = do
    response <- fetchStream c m cached url
    chunk <- response $$+- CL.consume
    return $ L.fromChunks chunk


fetchToDisk :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => [CH.Cookie] -> Manager -> String -> FP.FilePath -> m ()
fetchToDisk c m url file = do
    -- TODO: Replace this with Network.HTTP.Conduit.Downloader probably for streaming file to disk
    response <- fetchStream c m Always url

    -- Let's create the directory tree if it does not exist first
    liftIO $ createDirectoryIfMissing True $ FP.dropFileName file

    response $$+- CF.sinkFile file


fetchStream :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => [CH.Cookie] -> Manager -> Cached -> String -> m (C.ResumableSource m S.ByteString)
fetchStream c m cached url = do
    req' <- liftIO $ CH.parseUrl url

    -- TODO: remove the batoto special case
    let req = req' { CH.checkStatus = \_ _ _ -> Nothing, CH.cookieJar = Just $ CH.createCookieJar c }

    -- Caching hook here
    --  1. Check for cache value
    --  2. Return cached value if any exists, otherwise
    --  3. Fetch the http request then stream it to the cache
    --  4. Return cached value
    --  cached <-

    exists <- liftIO $ cacheExists url
    CM.unless exists $ do
        response <- CH.http req m
        CH.responseBody response $$+- cacheSink url

        -- Stall the read for the prerequest wait time before moving ahead
        liftIO $ threadDelay $ 1000000 * fetchWaitTime

    cacheSource url

cacheExists :: String -> IO Bool
cacheExists = doesFileExist . cacheFile

cacheSource :: MonadResource m => String -> m (C.ResumableSource m S.ByteString)
cacheSource url = do
    (a, _) <- CF.sourceFile (cacheFile url) $$+ CL.take 0
    return a

cacheSink :: MonadResource m => String -> C.Sink S.ByteString m ()
cacheSink url = do
    let fp = cacheFile url

    -- Let's create the cache if it does not exist.
    liftIO $ createDirectoryIfMissing True $ FP.dropFileName fp

    CF.sinkFile fp

cacheFile :: String -> FP.FilePath
cacheFile url = "./cache" </> US.toString (digestToHexByteString (hash $ US.fromString url :: Digest SHA512))
