{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Fetcher
    ( fetch
    ) where

import Data.Conduit (($=), ($$), ($$+), ($$+-), MonadBaseControl, MonadResource)
import qualified Data.Conduit as C
import qualified Data.Conduit.Filesystem as CF
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

import Filesystem (createTree, isFile)
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FPO

import Crypto.Hash (digestToHexByteString, hash, Digest, SHA512)


import Data.Maybe
import qualified Data.List as DL
import qualified Data.Text as T


-- Local types
import Types



-- Seconds to wait between each request to this site
fetchWaitTime :: Int
fetchWaitTime = 1





-- TODO: Find a better home for this
comicTagToFilePath :: ComicTag -> FPO.FilePath
comicTagToFilePath ct = DL.foldl (</>) (FPO.decodeString "./") (DL.filter (not . FP.null)
    -- Site Name
    [ FPO.fromText (ctSiteName ct)

    -- Story Name
    , case (ctStoryName ct) of
        Nothing -> FP.empty
        Just x  -> FPO.fromText x

    -- Volume
    , case (ctVolume ct) of
        Nothing -> FP.empty
        Just x  -> unitTagToFilePath UnitTagVolume x

    -- Chapter
    , case (ctChapter ct) of
        Nothing -> FP.empty
        Just x  -> unitTagToFilePath UnitTagChapter x

    -- TODO: this fromJust is bad news
    , FPO.fromText (fromJust $ ctFileName ct)
    ])

    where
        unitTagToFilePath :: UnitTagType -> UnitTag -> FPO.FilePath
        unitTagToFilePath UnitTagVolume ut  = unitTagString (T.pack "Volume ") ut
        unitTagToFilePath UnitTagChapter ut = unitTagString (T.pack "Chapter ") ut

        -- TODO: need to add zero padding
        unitTagString :: T.Text -> UnitTag -> FPO.FilePath
        unitTagString s UnitTag{utNumber=issue, utTitle=name} = FPO.fromText (s `T.append` (T.pack $ show issue) `T.append` (
            case name of
                Nothing -> T.empty
                Just x  -> (T.pack ": ") `T.append` x
            ))









-- TODO: restart if the exception kills -- main.hs: InvalidUrlException "/ggmain/doublespreads/extrabits/Gil.jpg" "Invalid URL"
fetch :: TBMChan FetchType -> TBMChan ReplyType -> IO ()
fetch i o = withSocketsDo $ E.bracket
    (CH.newManager CH.def)
    CH.closeManager
    (\manager ->
        -- Forever loop (probably don't need the forever at all)
        CM.forever $ C.runResourceT $ conduitFetcher manager i o
    )




conduitFetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> TBMChan FetchType -> TBMChan ReplyType -> m ()
conduitFetcher m i o = CT.sourceTBMChan i $= CL.mapMaybeM (fetcher m) $$ CT.sinkTBMChan o


conduitFetcherList :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> [FetchType] -> m [ReplyType]
conduitFetcherList m i = CL.sourceList i $= CL.mapMaybeM (fetcher m) $$ CL.consume


fetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> FetchType -> m (Maybe ReplyType)
fetcher m (Webpage u t) = do
    reply <- fetchSource m u
    return $ Just (WebpageReply reply t)
fetcher m (Image u f) = do
    -- Stream to disk
    fetchToDisk m u (comicTagToFilePath f)
    return $ Nothing



fetchSource :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> String -> m UL.ByteString
fetchSource m url = do
    response <- fetchStream m url
    chunk <- response $$+- CL.consume
    return $ L.fromChunks chunk


fetchToDisk :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> String -> FPO.FilePath -> m ()
fetchToDisk m url file = do
    -- TODO: Replace this with Network.HTTP.Conduit.Downloader probably for streaming file to disk
    response <- fetchStream m url

    -- Let's create the directory tree if it does not exist first
    liftIO $ createTree $ FP.directory file

    response $$+- CF.sinkFile file


fetchStream :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> String -> m (C.ResumableSource m S.ByteString)
fetchStream m url = do
    req' <- CH.parseUrl url
    let req = req' { CH.checkStatus = \_ _ _ -> Nothing }

    -- Caching hook here
    --  1. Check for cache value
    --  2. Return cached value if any exists, otherwise
    --  3. Fetch the http request then stream it to the cache
    --  4. Return cached value

    exists <- liftIO $ cacheExists url
    CM.unless exists $ do
        response <- CH.http req m
        CH.responseBody response $$+- cacheSink url

        -- Stall the read for the prerequest wait time before moving ahead
        liftIO $ threadDelay $ 1000000 * fetchWaitTime

    cacheSource url

cacheExists :: String -> IO Bool
cacheExists = isFile . cacheFile

cacheSource :: MonadResource m => String -> m (C.ResumableSource m S.ByteString)
cacheSource url = do
    (a, b) <- (CF.sourceFile $ cacheFile url) $$+ CL.take 0
    return a

cacheSink :: MonadResource m => String -> C.Sink S.ByteString m ()
cacheSink url = do
    let fp = cacheFile url

    -- Let's create the cache if it does not exist.
    liftIO $ createTree $ FP.directory fp

    CF.sinkFile fp

cacheFile :: String -> FPO.FilePath
cacheFile url = FPO.decodeString "./cache" </> (FPO.decode $ digestToHexByteString $ (hash $ US.fromString url :: Digest SHA512))
