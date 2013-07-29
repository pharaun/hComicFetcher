{-# LANGUAGE FlexibleContexts, Rank2Types #-}
{-
    1. Basic structure
        a. Fetch a comic page
        b. Store it on disk in sequence order

        c. Close each volume/chapter such as -- TODO
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

    3. Requirements?
        a. Identify what qualifies as a Volume, Chapter, Page
        b. Identify how to process/fetch "next" page, chapter, volume
        c. Identify how to download and store each to disk in a scheme that makes sense
        d. How to specifiy what to be fetched (Url of the said series?) (Site?)
            a. Typeclass
                - nextPage
                - comic
                - volume, chapter

    4. TODO:
        a. If cancel/exit, should delete/re-download the corrupt file
-}
import Data.Maybe
import Data.List
import Control.Arrow.ArrowTree
import Text.XML.HXT.Core

import Network

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Concurrent.STM.TBMChan
import Control.Concurrent

import Data.Conduit.TMChan

import Data.Conduit
import qualified Network.HTTP.Conduit as H
import qualified Data.Conduit as C
import Data.Conduit.Binary hiding (sourceFile, sinkFile)
import Data.Conduit.Filesystem

import qualified Data.Conduit.List as CL

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

import Data.Maybe

import qualified Control.Exception as E

import Control.Failure

import Prelude hiding (FilePath)
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FPO
import qualified Data.Conduit.Internal as IC

import Filesystem

import Network.HTTP.Types.URI

import qualified Data.Text as T

-- Cache hash
import Crypto.Hash

-- Seconds to wait between each request to this site
fetchWaitTime :: Int
fetchWaitTime = 1

-- Records for all of the site to scrap from
data Comic = Comic
    { comicName :: String
    , seedPage :: String
    , nextPage :: (ArrowXml a) => a XmlTree String
    , comic :: (ArrowXml a) => a XmlTree String
    , comicFileName :: String -> String -> FPO.FilePath
    -- Identify act (vol 1, 2) via body (class) - single-category-act-four ...
    , whichVolChp :: (ArrowXml a) => a XmlTree String
    }


--
-- SEQUAL SCAN COMICS
--

-- Exploitation Now
exploitationNow = Comic
    { comicName = "Exploitation Now"
    , seedPage = "http://www.exploitationnow.com/2000-07-07/9"
    , nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"
    , comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    , comicFileName = \vol url ->
        let base = FPO.decodeString "./exploitation_now"
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
            dirs = FPO.fromText $ T.pack $ exploitationNowVol vol
        in base FPO.</> dirs FPO.</> file
    , whichVolChp =
        hasName "body"
        >>> hasAttr "class"
        >>> getAttrValue "class"
        >>> arr words
        >>> arr (filter (isPrefixOf "single-category"))
        >>> arr (filter (not . isSuffixOf "comic"))
        >>> arr (filter (not . isSuffixOf "comics"))
        >>> arr (filter (not . isSuffixOf "uncategorized"))
        >>> arr concat
    }

-- Does Not Play Well With Others
doesNotPlayWellWithOthers = exploitationNow
    { comicName = "Does Not Play Well With Others"
    , seedPage = "http://www.doesnotplaywellwithothers.com/comics/pwc-000f"
    , comicFileName = \vol url ->
        let base = FPO.decodeString "./does_not_play_well_with_others"
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
        in base FPO.</> file
    }

exploitationNowVol :: String -> String
exploitationNowVol "single-category-act-one"         = "vol-1_act-one"
exploitationNowVol "single-category-act-two"         = "vol-2_act-two"
exploitationNowVol ""                                = "vol-3"
exploitationNowVol "single-category-intermission-i"  = "vol-4_intermission-I"
exploitationNowVol "single-category-act-three"       = "vol-5_act-three"
exploitationNowVol "single-category-intermission-ii" = "vol-6_intermission-II"
exploitationNowVol "single-category-act-four"        = "vol-7_act-four"
exploitationNowVol _ = "Unknown"


--
-- INDEX BASED COMIC
--

-- Errant Story
--  <li id="categories-412160671" class="widget widget_categories"><h2 class="widgettitle">Archives</h2>
--  <select name='cat' id='cat' class='postform' >
--  <option value='-1'>Select Category</option>
--  <option class="level-0" value="7">Blog</option>
--  <option class="level-0" value="6">Comics</option>
--  <option class="level-1" value="129">Errant Story</option>
--  <option class="level-2" value="131">Chronicles of Heretic Knowledge</option>
--  <option class="level-2" value="9">Errant Commentary</option>
--  <option class="level-2" value="137">Guest Comics</option>
--  <option class="level-2" value="59">Volume 1</option>
--  <option class="level-3" value="25">Chapter 00 (Prologue)</option>
--  <option class="level-3" value="24">Chapter 01</option>
--  <option class="level-3" value="23">Chapter 02</option>
--  <option class="level-3" value="26">Chapter 03</option>
--  <option class="level-3" value="27">Chapter 04</option>
--  <option class="level-2" value="58">Volume 2</option>
--  <option class="level-3" value="30">Chapter 05</option>
--  <option class="level-3" value="29">Chapter 06</option>
--  <option class="level-3" value="28">Chapter 07</option>
--  <option class="level-3" value="22">Chapter 08</option>
--  <option class="level-3" value="31">Chapter 09</option>
--  <option class="level-3" value="21">Chapter 10</option>
--  <option class="level-2" value="57">Volume 3</option>
--  <option class="level-3" value="14">Chapter 11</option>
--  <option class="level-3" value="15">Chapter 12</option>
--  <option class="level-3" value="13">Chapter 13</option>
--  <option class="level-3" value="16">Chapter 14</option>
--  <option class="level-3" value="20">Chapter 15</option>
--  <option class="level-3" value="19">Chapter 16</option>
--  <option class="level-3" value="18">Chapter 17</option>
--  <option class="level-3" value="17">Chapter 18</option>
--  <option class="level-2" value="56">Volume 4</option>
--  <option class="level-3" value="46">Chapter 19</option>
--  <option class="level-3" value="45">Chapter 20</option>
--  <option class="level-3" value="44">Chapter 21</option>
--  <option class="level-3" value="47">Chapter 22</option>
--  <option class="level-3" value="48">Chapter 23</option>
--  <option class="level-3" value="51">Chapter 24</option>
--  <option class="level-3" value="50">Chapter 25</option>
--  <option class="level-3" value="49">Chapter 26</option>
--  <option class="level-2" value="54">Volume 5</option>
--  <option class="level-3" value="52">Chapter 27</option>
--  <option class="level-3" value="43">Chapter 28</option>
--  <option class="level-3" value="42">Chapter 29</option>
--  <option class="level-3" value="35">Chapter 30</option>
--  <option class="level-3" value="34">Chapter 31</option>
--  <option class="level-3" value="33">Chapter 32</option>
--  <option class="level-3" value="36">Chapter 33</option>
--  <option class="level-3" value="37">Chapter 34</option>
--  <option class="level-2" value="55">Volume 6</option>
--  <option class="level-3" value="41">Chapter 35</option>
--  <option class="level-3" value="40">Chapter 36</option>
--  <option class="level-3" value="39">Chapter 37</option>
--  <option class="level-3" value="38">Chapter 38</option>
--  <option class="level-3" value="53">Chapter 39</option>
--  <option class="level-3" value="96">Chapter 40</option>
--  <option class="level-3" value="104">Chapter 41</option>
--  <option class="level-3" value="112">Chapter 42</option>
--  <option class="level-2" value="103">Volume 7</option>
--  <option class="level-3" value="120">Chapter 43</option>
--  <option class="level-3" value="124">Chapter 44</option>
--  <option class="level-3" value="128">Chapter 45</option>
--  <option class="level-3" value="130">Chapter 46</option>
--  <option class="level-3" value="132">Chapter 47</option>
--  <option class="level-3" value="134">Chapter 48</option>
--  <option class="level-3" value="140">Chapter 49</option>
--  <option class="level-3" value="143">Chapter 50</option>
--  <option class="level-3" value="144">Chapter 51 (Epilogue)</option>
--  <option class="level-1" value="147">Errant Story Commentary Track</option>
--  <option class="level-2" value="148">Volume 1 CT</option>
--  <option class="level-3" value="149">Chapter 00 (Prologue) CT</option>
--  <option class="level-3" value="150">Chapter 01 CT</option>
--  <option class="level-3" value="152">Chapter 02 CT</option>
--  <option class="level-3" value="153">Chapter 03 CT</option>
--  <option class="level-3" value="154">Chapter 04 CT</option>
--  <option class="level-3" value="160">Chapter 05 CT</option>
--  <option class="level-3" value="157">Chapter 06 CT</option>
--  <option class="level-3" value="158">Chapter 07 CT</option>
--  <option class="level-3" value="159">Chapter 08 CT</option>
--  <option class="level-2" value="155">Volume 2 CT</option>
--  <option class="level-3" value="161">Chapter 09 CT</option>
--  <option class="level-3" value="162">Chapter 10 CT</option>
--  <option class="level-0" value="1">Uncategorized</option>
--  </select>
--  <script type='text/javascript'>
--  /* <![CDATA[ */
--  var dropdown = document.getElementById("cat");
--  function onCatChange() {
--  if ( dropdown.options[dropdown.selectedIndex].value > 0 ) {
--  location.href = "http://www.errantstory.com/?cat="+dropdown.options[dropdown.selectedIndex].value;
--  }
--  }
--  dropdown.onchange = onCatChange;
--  /* ]]> */
--  </script>
--    { comicName :: String
--    , seedPage :: String
--    , nextPage :: (ArrowXml a) => a XmlTree String
--    , comic :: (ArrowXml a) => a XmlTree String
--    , comicFileName :: String -> String -> FPO.FilePath
--    -- Identify act (vol 1, 2) via body (class) - single-category-act-four ...
--    , whichVolChp :: (ArrowXml a) => a XmlTree String
--    }
-- TODO:
--  * The page by page works for a raw dump but it does not work for vol/chp/etc
--  * Need to work/create an indexer that will walk the vol/chp tree of the archive
errantStory = Comic
    { comicName = "Errant Story"
    , seedPage = "http://www.errantstory.com/2002-11-04/15"
    , nextPage =
        hasName "h4"
        >>> hasAttrValue "class" (isInfixOf "nav-next")
        >>> getChildren
        >>> hasName "a"
        >>> hasAttr "href"
        >>> getAttrValue "href"
    , comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    , comicFileName = \vol url ->
        let base = FPO.decodeString "./errant_story"
            file = FPO.fromText $ last $ decodePathSegments $ US.fromString url
        in base FPO.</> file
    , whichVolChp =
        hasName "body"
        >>> hasAttr "class"
        >>> getAttrValue "class"
        >>> arr words
        >>> arr (filter (isPrefixOf "single-category"))
        >>> arr (filter (not . isSuffixOf "comic"))
        >>> arr (filter (not . isSuffixOf "comics"))
        >>> arr (filter (not . isSuffixOf "uncategorized"))
        >>> arr concat
    }


-- TODO:
--  - Defined stop point, Errant Story
main = do
    let target = errantStory

    -- Queues for processing stuff
    toFetch <- atomically $ newTBMChan 10
    toReturn <- atomically $ newTBMChan 10

    -- Seed with an initial page
    atomically $ writeTBMChan toFetch $ Webpage (seedPage target)

    -- Start the fetcher
    threadId <- forkIO $ fetch toFetch toReturn

    -- Do processing by pulling off each entry off the toReturn and submitting more
    untilM_ (parser target toReturn toFetch) id
--    replicateM 10 (parser toReturn toFetch)

    -- We're done kill it
    killThread threadId


{-
     701,039,680 bytes allocated in the heap
      88,742,552 bytes copied during GC
       1,489,304 bytes maximum residency (32 sample(s))
          57,288 bytes maximum slop
               5 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0      1325 colls,     0 par    0.11s    0.11s     0.0001s    0.0006s
  Gen  1        32 colls,     0 par    0.06s    0.06s     0.0020s    0.0029s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    0.25s  ( 10.26s elapsed)
  GC      time    0.18s  (  0.18s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    0.43s  ( 10.43s elapsed)

  %GC     time      41.2%  (1.7% elapsed)

  Alloc rate    2,804,729,796 bytes per MUT second

  Productivity  58.8% of total user, 2.4% of total elapsed

-}


-- LINEAR comic parser/scanner
-- TODO:
--  * Make this work with indexed comic, if not need a second type
--  * look into some form of state transformer monad for tracking state between parse run if needed
parser :: Comic -> TBMChan UL.ByteString -> TBMChan FetchType -> IO Bool
parser c i o = do
    r <- atomically $ readTBMChan i
    case r of
        (Just html) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
            next <- runX $ doc //> (nextPage c)

            img <- runX $ doc //> (comic c)

            vol <- runX $ doc //> (whichVolChp c)
            -- HXT

            -- Errant Story (Bail out when the next webpage matches this)
            let filteredNext = filter (/= "http://www.errantstory.com/2012-03-23/5460") next

            atomically $ mapM_ (writeTBMChan o) (map Webpage filteredNext)
            atomically $ mapM_ (writeTBMChan o) (map (\a -> Image a $ (comicFileName c) (concat vol) a) img)

            -- Terminate if we decide there's no more nextPage to fetch
            -- This does not work if there's multiple parser/worker going but it'll be ok for this poc
            Control.Monad.when (null filteredNext) $ atomically $ closeTBMChan o

            -- Do we have any comic we want to store to disk?
            putStrLn "Fetched Urls:"
            mapM_ putStrLn img
            mapM_ putStrLn filteredNext

            putStrLn "vol:"
            mapM_ putStrLn vol

            -- We do want to keep going cos we just submitted another page to fetch
            return True

        Nothing -> return False





fetch :: TBMChan FetchType -> TBMChan UL.ByteString -> IO ()
fetch i o = withSocketsDo $ E.bracket
    (H.newManager H.def)
    H.closeManager
    (\manager ->
        -- Forever loop (probably don't need the forever at all)
        forever $ runResourceT $ conduitFetcher manager i o
    )




conduitFetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> TBMChan FetchType -> TBMChan UL.ByteString -> m ()
conduitFetcher m i o = sourceTBMChan i $= CL.mapMaybeM (fetcher m) $$ sinkTBMChan o


conduitFetcherList :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> [FetchType] -> m [UL.ByteString]
conduitFetcherList m i = CL.sourceList i $= CL.mapMaybeM (fetcher m) $$ CL.consume


-- Data type of the url and any additional info needed
data FetchType  = Webpage String
                | Image String FPO.FilePath -- Url & Filename


fetcher :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> FetchType -> m (Maybe UL.ByteString)
fetcher m (Webpage u) = do
    reply <- fetchSource m u
    return $ Just reply
fetcher m (Image u f) = do
    -- Stream to disk
    fetchToDisk m u f
    return $ Nothing



fetchSource :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> String -> m UL.ByteString
fetchSource m url = do
    response <- fetchStream m url
    chunk <- response C.$$+- CL.consume
    return $ L.fromChunks chunk


fetchToDisk :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> String -> FPO.FilePath -> m ()
fetchToDisk m url file = do
    -- TODO: Replace this with Network.HTTP.Conduit.Downloader probably for streaming file to disk
    response <- fetchStream m url

    -- Let's create the directory tree if it does not exist first
    liftIO $ createTree $ FP.directory file

    response C.$$+- sinkFile file


fetchStream :: (
    MonadBaseControl IO m,
    MonadResource m,
    Failure H.HttpException m
    ) => H.Manager -> String -> m (ResumableSource m S.ByteString)
fetchStream m url = do
    req' <- H.parseUrl url
    let req = req' { H.checkStatus = \_ _ _ -> Nothing }

    -- Caching hook here
    --  1. Check for cache value
    --  2. Return cached value if any exists, otherwise
    --  3. Fetch the http request then stream it to the cache
    --  4. Return cached value

    exists <- liftIO $ cacheExists url
    unless exists $ do
        response <- H.http req m
        H.responseBody response C.$$+- cacheSink url

        -- Stall the read for the prerequest wait time before moving ahead
        liftIO $ threadDelay $ 1000000 * fetchWaitTime

    cacheSource url

cacheExists :: String -> IO Bool
cacheExists = isFile . cacheFile

cacheSource :: MonadResource m => String -> m (ResumableSource m S.ByteString)
cacheSource url = do
    (a, b) <- (sourceFile $ cacheFile url) C.$$+ CL.take 0
    return a

cacheSink :: MonadResource m => String -> Sink S.ByteString m ()
cacheSink url = do
    let fp = cacheFile url

    -- Let's create the cache if it does not exist.
    liftIO $ createTree $ FP.directory fp

    sinkFile fp


cacheFile :: String -> FPO.FilePath
cacheFile url = FPO.decodeString "./cache" FPO.</> (FPO.decode $ digestToHexByteString $ (hash $ US.fromString url :: Digest SHA512))




-- Execute till result is false
untilM_ :: (Monad m) => m a -> (a -> Bool) -> m ()
untilM_ f p = do
    x <- f
    if p x
        then untilM_ f p
        else return ()
