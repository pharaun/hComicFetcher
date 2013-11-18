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

import Data.Time.Clock
import Data.Time.Calendar


-- Local types
import Types



-- Seconds to wait between each request to this site
fetchWaitTime :: Int
fetchWaitTime = 1

--
-- Custom cookie jar for Batoto to only display english
--
-- TODO: Create a way for us to have per site rules for (auth/cookies/etc)
past :: UTCTime
past = UTCTime (ModifiedJulianDay 56000) (secondsToDiffTime 0) -- 2012-03-14

future :: UTCTime
future = UTCTime (ModifiedJulianDay 60000) (secondsToDiffTime 0) -- 2023-02-25

batotoCookie :: CH.Cookie
batotoCookie = CH.Cookie
    { CH.cookie_name   = US.fromString "lang_option"
    , CH.cookie_value  = US.fromString "English"
    , CH.cookie_domain = US.fromString ".batoto.net"
    , CH.cookie_path   = US.fromString "/"

    , CH.cookie_expiry_time = future
    , CH.cookie_creation_time = past
    , CH.cookie_last_access_time = past

    , CH.cookie_persistent = True
    , CH.cookie_host_only = False
    , CH.cookie_secure_only = False
    , CH.cookie_http_only = False
    }




data UnitTagType = UnitTagVolume | UnitTagChapter

-- TODO: Find a better home for this
comicTagToFilePath :: ComicTag -> FPO.FilePath
comicTagToFilePath ct = DL.foldl (</>) (FPO.decodeString "./") (DL.filter (not . FP.null)
    -- Site Name
    [ FPO.fromText (ctSiteName ct)

    -- Story Name
    , case ctStoryName ct of
        Nothing -> FP.empty
        Just x  -> FPO.fromText x

    -- Volume
    , case ctVolume ct of
        Nothing -> FP.empty
        Just x  -> unitTagToFilePath UnitTagVolume x

    -- Chapter
    , case ctChapter ct of
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
        unitTagString s UnitTag{utNumber=issue, utTitle=name} = FPO.fromText (s `T.append` (formatListDigits issue) `T.append` (
            case name of
                Nothing -> T.empty
                Just x  -> T.pack ": " `T.append` x
            ))

        formatListDigits :: [Digits] -> T.Text
        formatListDigits x = T.intercalate (T.pack ",") (map formatDigits x)

        formatDigits :: Digits -> T.Text
        formatDigits (RangeDigit a b) = T.concat [formatDigit a, T.pack "-", formatDigit b]
        formatDigits (StandAlone a)   = formatDigit a

        formatDigit :: Digit -> T.Text
        formatDigit (Digit i (Just s) (Just v)) = T.concat [zeroPad i, formatSubDigit s, formatVersion v]
        formatDigit (Digit i (Just s) Nothing)  = T.concat [zeroPad i, formatSubDigit s]
        formatDigit (Digit i Nothing (Just v))  = T.concat [zeroPad i, formatVersion v]
        formatDigit (Digit i Nothing Nothing)   = T.concat [zeroPad i]

        -- TODO: Formatting is a bit debatable but its directly concat
        formatSubDigit :: SubDigit -> T.Text
        formatSubDigit (DotSubDigit (Just i) t) = T.concat [T.pack ".", T.pack $ show i, t]
        formatSubDigit (DotSubDigit Nothing t)  = T.concat [T.pack ".", t]

        formatVersion :: Integer -> T.Text
        formatVersion v = T.pack "v" `T.append` (T.pack $ show v)

        -- TODO: adjust but let's start with 3 digits for now
        zeroPad :: Integer -> T.Text
        zeroPad = T.justifyRight 3 '0' . T.pack . show





-- TODO: restart if the exception kills -- main.hs: InvalidUrlException "/ggmain/doublespreads/extrabits/Gil.jpg" "Invalid URL"
fetch :: TBMChan (FetchType a) -> TBMChan (ReplyType a) -> IO ()
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
    ) => Manager -> TBMChan (FetchType a) -> TBMChan (ReplyType a) -> m ()
conduitFetcher m i o = CT.sourceTBMChan i $= CL.mapMaybeM (fetcher m) $$ CT.sinkTBMChan o


conduitFetcherList :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> [FetchType a] -> m [ReplyType a]
conduitFetcherList m i = CL.sourceList i $= CL.mapMaybeM (fetcher m) $$ CL.consume


fetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure HttpException m
    ) => Manager -> FetchType a -> m (Maybe (ReplyType a))
fetcher m (Webpage u t) = do
    reply <- fetchSource m u
    return $ Just (WebpageReply reply t)
fetcher m (Image u f) = do
    -- Stream to disk
    fetchToDisk m u (comicTagToFilePath f)
    return Nothing



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

    -- TODO: remove the batoto special case
    let req =   if "batoto" `DL.isInfixOf` url
                then req' { CH.checkStatus = \_ _ _ -> Nothing, CH.cookieJar = Just $ CH.createCookieJar [batotoCookie] }
                else req' { CH.checkStatus = \_ _ _ -> Nothing }

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
    (a, b) <- CF.sourceFile (cacheFile url) $$+ CL.take 0
    return a

cacheSink :: MonadResource m => String -> C.Sink S.ByteString m ()
cacheSink url = do
    let fp = cacheFile url

    -- Let's create the cache if it does not exist.
    liftIO $ createTree $ FP.directory fp

    CF.sinkFile fp

cacheFile :: String -> FPO.FilePath
cacheFile url = FPO.decodeString "./cache" </> FPO.decode (digestToHexByteString (hash $ US.fromString url :: Digest SHA512))
