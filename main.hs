{-# LANGUAGE FlexibleContexts, Rank2Types #-}
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

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (catMaybes)

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List as DL
import qualified Data.List.Split as SL

import Text.XML.HXT.Core

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TBMChan
import Control.Monad.STM (atomically)
import qualified Control.Monad as CM

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FPO


-- Local imports
import Types
import Fetcher




-- Parameterized type
data Foo a = Foo
    { bar :: a
    , baz :: a -> String
    }
-- If no good there's - ExistentialQuantification
--
-- {-# LANGUAGE ExistentialQuantification #-}
-- data Foo = forall a . Foo { foo :: a, bar :: a -> String }

-- Records for all of the site to scrap from
data Comic = Comic
    { comicName :: String

    -- Seed page/type for kickstarting the parser/fetcher
    , seedPage :: String
    , seedType :: Tag

    , nextPage :: (ArrowXml a) => a XmlTree String
    , comic :: (ArrowXml a) => a XmlTree String
    -- Identify act (vol 1, 2) via body (class) - single-category-act-four ...
    , whichVolChp :: (ArrowXml a) => a b XmlTree -> a b String

    -- Indexing parser
    , indexList :: (ArrowXml a) => a XmlTree (String, (String, String))
    , chapterList :: (ArrowXml a) => a XmlTree (String, String)
    , chapterNextPage :: (ArrowXml a) => a XmlTree String

    , chapterPage :: (ArrowXml a) => a b XmlTree -> a b (String, [String])

    -- TODO: first step is cleaning up the ComicTag Generation for
    -- outputting, then we can look into fixing up the Tags
    , comicFileName :: String -> String -> ComicTag
    , comicTagFileName :: ComicTag -> String -> ComicTag
    }


--
-- SEQUAL SCAN COMICS
--

-- Exploitation Now
exploitationNow = Comic
    { comicName = "Exploitation Now"
    , seedPage = "http://www.exploitationnow.com/2000-07-07/9"
    , seedType = Serial
    , nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"
    , comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"

    , comicFileName = \vol url -> ComicTag (T.pack "exploitation_now") Nothing (exploitationNowVol vol) Nothing (Just $ last $ decodePathSegments $ US.fromString url)
    , comicTagFileName = undefined

    , whichVolChp = \doc -> doc
        //> hasName "body"
        >>> hasAttr "class"
        >>> getAttrValue "class"
        >>> arr words
        >>> arr (filter (isPrefixOf "single-category"))
        >>> arr (filter (not . isSuffixOf "comic"))
        >>> arr (filter (not . isSuffixOf "comics"))
        >>> arr (filter (not . isSuffixOf "uncategorized"))
        >>> arr concat

    -- TODO: NOOP
    , indexList = undefined
    , chapterList = undefined
    , chapterNextPage = undefined
    , chapterPage = undefined
    }

exploitationNowVol :: String -> Maybe UnitTag
exploitationNowVol "single-category-act-one"         = Just $ UnitTag 1 $ Just $ T.pack "Act One"
exploitationNowVol "single-category-act-two"         = Just $ UnitTag 2 $ Just $ T.pack "Act Two"
exploitationNowVol "single-category-intermission-i"  = Just $ UnitTag 3 $ Just $ T.pack "Intermission I"
exploitationNowVol "single-category-act-three"       = Just $ UnitTag 4 $ Just $ T.pack "Act Three"
exploitationNowVol "single-category-intermission-ii" = Just $ UnitTag 5 $ Just $ T.pack "Intermission II"
exploitationNowVol "single-category-act-four"        = Just $ UnitTag 6 $ Just $ T.pack "Act Four"
exploitationNowVol _ = Nothing


-- Does Not Play Well With Others
doesNotPlayWellWithOthers = exploitationNow
    { comicName = "Does Not Play Well With Others"
    , seedPage = "http://www.doesnotplaywellwithothers.com/comics/pwc-000f"
    , seedType = Serial

    -- TODO: this is a no-op because its not used, need to find a way to make it do nothing
    --  This can't be undefined because its still invokved
    , whichVolChp = \doc -> doc //> hasName "body" >>> hasAttr "class" >>> getAttrValue "class"


    , comicTagFileName = undefined
    , comicFileName = \_ url -> ComicTag (T.pack "./does_not_play_well_with_others") Nothing Nothing Nothing (Just $ last $ decodePathSegments $ US.fromString url)
    }


-- Girl Genius
girlGenius = Comic
    { comicName = "Girl Genius"
    , seedPage = "http://www.girlgeniusonline.com/comic.php?date=20021104"
    , seedType = Serial

    , nextPage = hasName "td" >>> hasAttrValue "valign" (== "top") //> (hasName "a" </ (hasName "img" >>> hasAttrValue "alt" (== "The Next Comic"))) >>> getAttrValue "href"

    , comic =
        hasName "td"
        >>> hasAttrValue "valign" (== "middle")
        //> hasName "img"
        >>> hasAttr "src"
        >>> getAttrValue "src"
        >>. arr (filter (isPrefixOf "http"))


    -- TODO: this returns a single string (cuz its concating all of this), we worked around this but this is very much non-ideal
    --  For workaround see girlGeniusVol
    , whichVolChp = \doc ->
        (doc //>
        hasName "form"
        >>> hasAttrValue "name" (== "storyline")
        //> (hasName "option" `notContaining` (getChildren >>> hasText (== "---\"ADVANCED CLASS\" BEGINS---")))
        >>> (
                (withDefault (getAttrValue0 "selected") "no" >>> arr (/= "no"))
                &&&
                (getChildren >>> getText)
            )
        >>. filter (\(a, b) -> a || ((("VOLUME" `isInfixOf` b) || ("Volume" `isInfixOf` b)) && (not (("Final Page" `isPrefixOf` b) || ("Wallpaper" `isInfixOf` b)))))
        ) >. (fst . DL.break fst)
        >>> unlistA
        >>> arr snd
        -- ) >>> arr ((SL.split . SL.keepDelimsL . SL.whenElt) (isPrefixOf "Chapter")) -- TODO: this is for splitting things up


    -- TODO: NOOP
    , indexList = undefined
    , chapterList = undefined
    , chapterNextPage = undefined
    , chapterPage = undefined


    , comicTagFileName = undefined
    , comicFileName = \vol url ->
        ComicTag (T.pack "girl_genius") Nothing (Just $ UnitTag (girlGeniusVol vol) Nothing) Nothing (Just $ last $ decodePathSegments $ US.fromString url)
    }

girlGeniusVol :: String -> Integer
girlGeniusVol a = wordToNumber (DL.reverse $ DL.head $ DL.words $ DL.drop 3 $ DL.reverse a)




--
-- INDEX BASED COMIC
--

-- Errant Story
errantStory = Comic
    { comicName = "Errant Story"
    , seedPage = "http://www.errantstory.com"
    , seedType = VolIndex

    -- TODO: this is no-op because its a indexed/volume type of comic not sequal
    , nextPage = undefined

    , comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"

    -- TODO: this is a no-op because its not used, need to find a way to make it do nothing
    , whichVolChp = undefined

    , indexList =
        hasName "select"
        >>> hasAttrValue "id" (isInfixOf "cat")
        >>> getChildren
        >>> hasName "option"
        >>> hasAttrValue "class" (/= "level-0") -- Filter first level
        >>> hasAttrValue "value" (/= "131") -- History
        >>> hasAttrValue "value" (/= "9") -- Commentary
        >>> hasAttrValue "value" (/= "137") -- Guest Comics
        >>> (
            (getAttrValue "value" >>> arr ("http://www.errantstory.com/?cat=" ++))
            &&&
                (
                getAttrValue "class"
                &&&
                (getChildren >>> getText >>> arr (filter (/= '\160')))
                )
            )

    , chapterList =
        hasName "div"
        >>> hasAttrValue "class" (isInfixOf "comicarchiveframe")
        >>> getChildren
        >>> hasName "a"
        >>> hasAttr "href"
        >>> (
            getAttrValue "href"
            &&&
                (
                getChildren
                >>> hasName "img"
                >>> hasAttr "alt"
                >>> getAttrValue "alt"
                )
            )

    , chapterNextPage =
        hasName "div"
        >>> hasAttrValue "class" (== "pagenav-right")
        >>> getChildren
        >>> hasName "a"
        >>> hasAttr "href"
        >>> getAttrValue "href"

    , chapterPage = undefined

    , comicTagFileName = \ct url -> ct{ctFileName = Just $ last $ decodePathSegments $ US.fromString url}
    , comicFileName = undefined
    }

--buildUrlAndComicTagMapping :: FPO.FilePath -> [(Url, (String, String))] -> [(Url, FPO.FilePath)]
buildUrlAndComicTagMapping :: ComicTag -> [(Url, (String, String))] -> [(Url, ComicTag)]
buildUrlAndComicTagMapping _ [] = []
buildUrlAndComicTagMapping root (x@(_, (level, name)):xs)
     | level == "level-3" = map (levelToComicTagMapping root) (x:xs)
     | otherwise          =
         let ours = DL.takeWhile (notSameLevel level) xs
             rest = DL.dropWhile (notSameLevel level) xs
         in
             buildUrlAndComicTagMapping (snd $ levelToComicTagMapping root x) ours ++ buildUrlAndComicTagMapping root rest
    where
        notSameLevel :: String -> (Url, (String, String)) -> Bool
        notSameLevel targetLevel (_, (level, _)) = targetLevel /= level

levelToComicTagMapping :: ComicTag -> (Url, (String, String)) -> (Url, ComicTag)
levelToComicTagMapping parent (url, ("level-3", name)) = (url, parent {ctChapter = Just $ UnitTag 0 $ Just $ T.pack name})
levelToComicTagMapping parent (url, ("level-2", name)) = (url, parent {ctVolume = Just $ UnitTag 0 $ Just $ T.pack name})
levelToComicTagMapping parent (url, ("level-1", name)) = (url, parent {ctStoryName = Just $ T.pack name})

--buildUrlAndComicTagMapping root all@((_, (level, name)):xs)
--     | level == "level-3" = map (chapterMapping root) all
--     | otherwise          =
--         let ours = DL.takeWhile (\a -> (fst $ snd a) /= level) xs
--             rest = filter (not . flip elem ours) xs
--         in buildUrlAndComicTagMapping (root </> (FPO.decodeString name)) ours ++ buildUrlAndComicTagMapping (root) rest
--     where
--         chapterMapping root (url, (_, name)) = (url, root </> (FPO.decodeString name))

-- data ComicTag = ComicTag
--     { ctSiteName :: String
--     , ctStoryName :: Maybe String
--
--     , ctVolume :: Maybe UnitTag
--     , ctChapter :: Maybe UnitTag
--     , ctFileName :: Maybe String
--     }
--
-- data UnitTag = UnitTag
--     { utNumber :: Integer
--     , utTitle :: Maybe String
--     }
--

testTag = ComicTag {ctSiteName = T.pack "errant_story", ctStoryName = Nothing, ctVolume = Nothing, ctChapter = Nothing, ctFileName = Nothing}
testUrl =  [
    ("http://www.errantstory.com/?cat=129",("level-1","Errant Story")),
        ("http://www.errantstory.com/?cat=59",("level-2","Volume 1")),
            ("http://www.errantstory.com/?cat=25",("level-3","Chapter 00 (Prologue)")),
            ("http://www.errantstory.com/?cat=24",("level-3","Chapter 01")),
        ("http://www.errantstory.com/?cat=59",("level-2","Volume 2")),
            ("http://www.errantstory.com/?cat=25",("level-3","Chapter 02")),
            ("http://www.errantstory.com/?cat=24",("level-3","Chapter 03")),
    ("http://www.errantstory.com/?cat=129",("level-1","Errant Story CT")),
        ("http://www.errantstory.com/?cat=59",("level-2","Volume 1")),
            ("http://www.errantstory.com/?cat=25",("level-3","Chapter 00 (Prologue)")),
            ("http://www.errantstory.com/?cat=24",("level-3","Chapter 01")),
        ("http://www.errantstory.com/?cat=59",("level-2","Volume 2")),
            ("http://www.errantstory.com/?cat=25",("level-3","Chapter 02")),
            ("http://www.errantstory.com/?cat=24",("level-3","Chapter 03"))
    ]





-- Gunnerkrigg Court
gunnerkrigCourt = Comic
    { comicName = "Gunnerkrigg Court"

    , seedPage = "http://www.gunnerkrigg.com/archives/"
    , seedType = ChpIndex

    -- TODO: this is no-op because its a indexed/volume type of comic not sequal
    , nextPage = undefined

    , comic = undefined

    -- TODO: this is a no-op because its not used, need to find a way to make it do nothing
    , whichVolChp = undefined

    , indexList = undefined
    , chapterList = undefined
    , chapterNextPage = undefined

    , chapterPage = \doc -> listA (doc
        //> hasAttrValue "class" (== "chapters")
        >>> hasName "div"
        >>> getChildren
        >>> (
                (
                    hasName "a"
                    >>> hasAttrValue "class" (== "chapter_button")
                    /> hasName "h4"
                )
                `Text.XML.HXT.Core.orElse`
                (
                    hasName "select"
                    >>> hasAttrValue "name" (== "page")
                    >>> getChildren
                    >>> hasName "option"
                )
            )
        >>> ifA (hasName "h4") (getChildren >>> getText) (getAttrValue "value" >>> arr linkComic)
        ) >>> arr ((SL.split . SL.keepDelimsL . SL.whenElt) (isPrefixOf "Chapter"))
        >>> unlistA
        >>> arr tupleComic
        >>. arr catMaybes


    , comicTagFileName = undefined
    , comicFileName = \filepath url ->
        let (chp, name) = fixChp filepath
        in ComicTag (T.pack "gunnerkrigg_court") Nothing Nothing (Just $ UnitTag chp $ Just $ T.pack name) (Just $ last $ decodePathSegments $ US.fromString url)
    }

linkComic :: String -> String
linkComic u = "http://www.gunnerkrigg.com/comics/" ++ padNum u ++ ".jpg"
    where
        padNum n = (DL.concat $ DL.take (8 - DL.length u) (DL.repeat "0")) ++ n

tupleComic :: [String] -> Maybe (String, [String])
tupleComic [] = Nothing
tupleComic (x:xs) = Just (x, xs)

fixChp :: String -> (Integer, String)
fixChp = extract . cleanChapter

extract :: String -> (Integer, String)
extract ('C':'h':'a':'p':'t':'e':'r':' ':a:b:':':' ':xs) = (read $ [a, b], xs)

-- TODO: this is terribad but it'll let us have consistent chp/vol stuff
-- for cheap extracting
cleanChapter :: String -> String
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':x:'0':xs) = "Chapter " ++ [x] ++ "0" ++ xs
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':x:'1':xs) = "Chapter " ++ [x] ++ "1" ++ xs
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':x:'2':xs) = "Chapter " ++ [x] ++ "2" ++ xs
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':x:'3':xs) = "Chapter " ++ [x] ++ "3" ++ xs
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':x:'4':xs) = "Chapter " ++ [x] ++ "4" ++ xs
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':x:'5':xs) = "Chapter " ++ [x] ++ "5" ++ xs
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':x:'6':xs) = "Chapter " ++ [x] ++ "6" ++ xs
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':x:'7':xs) = "Chapter " ++ [x] ++ "7" ++ xs
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':x:'8':xs) = "Chapter " ++ [x] ++ "8" ++ xs
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':x:'9':xs) = "Chapter " ++ [x] ++ "9" ++ xs
cleanChapter ('C':'h':'a':'p':'t':'e':'r':' ':xs) = "Chapter 0" ++ xs
cleanChapter xs = xs




-- Batoto
batoto = Comic
    { comicName = "Batoto"
    , seedPage = "http://www.batoto.net/comic/_/comics/yotsubato-r311"
    , seedType = VolChpIndex

    , nextPage = undefined

    , comic = undefined

    , whichVolChp = undefined
    , indexList = undefined
    , chapterList = undefined
    , chapterNextPage = undefined
    , chapterPage = undefined

    , comicTagFileName = undefined
    , comicFileName = \_ url ->
        ComicTag (T.pack "batoto") (Just $ T.pack "yotsubato") Nothing Nothing Nothing
    }





-- TODO:
--  - Defined stop point, Errant Story
--  - Some command line arg for picking which comic to run
main = do
--    let target = exploitationNow
--    let target = doesNotPlayWellWithOthers
    let target = errantStory
--    let target = girlGenius
--    let target = gunnerkrigCourt
--    let target = batoto

    -- Queues for processing stuff
    -- TODO: look into tweaking this and making the indexed parser not deadlock the whole thing... if there's more to add to the queue than can be processed
    toFetch <- atomically $ newTBMChan 10000
    toReturn <- atomically $ newTBMChan 10000

    -- Seed with an initial page
    atomically $ writeTBMChan toFetch $ Webpage (seedPage target) (seedType target)

    -- Start the fetcher
    threadId <- forkIO $ fetch toFetch toReturn

    -- Do processing by pulling off each entry off the toReturn and submitting more
    untilM_ (indexedParser target toReturn toFetch) id

    -- We're done kill it
    killThread threadId



-- Indexer parser,
-- The mother of all parser, it parses various Tagged pages and then go from there
-- TODO:
--  * look into some form of state transformer monad for tracking state between parse run if needed
--  * This ^ is probably the Tag, which let us tag specific page with additional information if its needed
indexedParser :: Comic -> TBMChan ReplyType -> TBMChan FetchType -> IO Bool
indexedParser c i o = do
    r <- atomically $ readTBMChan i
    case r of
        Nothing -> return False
        (Just (WebpageReply html ChpIndex)) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html

            chpPages <- runX $ (chapterPage c) doc
            -- HXT

            -- TODO: this step isn't working correctly. validate what its doing.
            --  1. Probably being limited by the queue length and blocking here, tho fetcher should work?
            atomically $ mapM_ (writeTBMChan o) (DL.concatMap (\(chp, img) -> map (\i -> Image i ((comicFileName c) chp i)) img) chpPages)

            -- Terminate since we know we are done.
            -- This does not work if there's multiple parser/worker going but it'll be ok for this poc
            atomically $ closeTBMChan o

            -- Dump list of Comic page fetched
            putStrLn "Chp list:"
            mapM_ (putStrLn . fst) chpPages

            -- We do want to keep going cos we just submitted another page to fetch
            return True




-- This mess is all errant story's mess
        (Just (WebpageReply html VolIndex)) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html

            index <- runX $ doc //> (indexList c)
            -- HXT

            -- TODO: have a pre-process step for processing the parsing results into a useful list for dumping into TBMChans
            let defaultErrantStory = ComicTag (T.pack "errant_story") Nothing Nothing Nothing Nothing
            let list = buildUrlAndComicTagMapping defaultErrantStory index

            -- Dump list of Comic page fetched
            putStrLn "Chp list:"
            mapM_ print list

            -- TODO: this step isn't working correctly. validate what its doing.
            --  1. Probably being limited by the queue length and blocking here, tho fetcher should work?
            atomically $ mapM_ (writeTBMChan o) (map (\(u, p) -> Webpage u (Chapter p)) list)

            -- We do want to keep going cos we just submitted another page to fetch
            return True

        (Just (WebpageReply html (Chapter ct))) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html

            chp <- runX $ doc //> (chapterList c)
            next <- runX $ doc //> (chapterNextPage c)
            -- HXT

            -- Dump the next pages into the queue
            atomically $ mapM_ (writeTBMChan o) (map (\a -> Webpage a (Chapter ct)) next)

            -- Dump the single page into the queue
            -- TODO:
            --  1. Do something with the name of the page (Chapter 42: foobar) (not on all pages unfortunately)
            atomically $ mapM_ (writeTBMChan o) (map (\a -> Webpage (fst a) (Page ct)) chp)

            -- Do we have any comic we want to store to disk?
            putStrLn "Chp list:"
            mapM_ print chp

            putStrLn "Next archive:"
            mapM_ print next

            -- We do want to keep going cos we just submitted another page to fetch
            return True

        (Just (WebpageReply html (Page ct))) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html

            img <- runX $ doc //> (comic c)
            -- HXT

            atomically $ mapM_ (writeTBMChan o) (map (\a -> Image a $ (comicTagFileName c) ct a) img)

            -- TODO: probably want to have a way to identify when all of the fetching is done and parsing is done and die
            --  1. But it needs to allow for *long* parsing pauses and long fetching pauses in case of network issues or slow parser step
            -- Terminate if we decide there's no more nextPage to fetch
            -- This does not work if there's multiple parser/worker going but it'll be ok for this poc
--            CM.when (null filteredNext) $ atomically $ closeTBMChan o

            -- Do we have any comic we want to store to disk?
            putStrLn "Fetched Urls:"
            mapM_ putStrLn img

            -- We do want to keep going cos we just submitted another page to fetch
            return True







        (Just (WebpageReply html Serial)) -> do
            -- HXT
            let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
            next <- runX $ doc //> (nextPage c)
            img <- runX $ doc //> (comic c)
            vol <- runX $ (whichVolChp c) doc
            -- HXT

            atomically $ mapM_ (writeTBMChan o) (map (\a -> Webpage a Serial) next)
            atomically $ mapM_ (writeTBMChan o) (map (\a -> Image a $ (comicFileName c) (concat vol) a) img)

            -- Terminate if we decide there's no more nextPage to fetch
            -- This does not work if there's multiple parser/worker going but it'll be ok for this poc
            CM.when (null next) $ atomically $ closeTBMChan o
--            CM.when (null []) $ atomically $ closeTBMChan o

            -- Do we have any comic we want to store to disk?
            putStrLn "Fetched Urls:"
            mapM_ putStrLn img
            mapM_ putStrLn next

            putStrLn "vol:"
            mapM_ putStrLn vol


            -- We do want to keep going cos we just submitted another page to fetch
            return True












-- Execute till result is false
untilM_ :: (Monad m) => m a -> (a -> Bool) -> m ()
untilM_ f p = do
    x <- f
    if p x
        then untilM_ f p
        else return ()


-- Process a sequence of english number into an integer value
-- If invalid/incomplete/what so not it will return a 0
wordToNumber :: String -> Integer
wordToNumber = DL.foldl' compute 0 . (map T.unpack) . T.words . T.toLower . T.pack
    where
        compute :: Integer -> String -> Integer
        compute prior word =
            let v = DL.lookup word numberWordTable
            in case v of
                (Just x) -> (x + prior)
                Nothing  ->
                    let w = DL.lookup word multiplerWordTable
                    in case w of
                        (Just y) -> (y * prior)
                        Nothing  -> prior

numberWordTable :: [(String, Integer)]
numberWordTable =
    [ ("zero", 0)
    , ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    , ("ten", 10)
    , ("eleven", 11)
    , ("twelve", 12)
    , ("thirteen", 13)
    , ("fourteen", 14)
    , ("fifteen", 15)
    , ("sixteen", 16)
    , ("seventeen", 17)
    , ("eighteen", 18)
    , ("nineteen", 19)
    , ("twenty", 20)
    , ("thirty", 30)
    , ("fourty", 40)
    , ("fifty", 50)
    , ("sixty", 60)
    , ("seventy", 70)
    , ("eighty", 80)
    , ("ninety", 90)
    ]

multiplerWordTable :: [(String, Integer)]
multiplerWordTable =
    [ ("hundred", 100)
    , ("thousand", 1000)
    , ("million", 1000000)
    , ("billion", 1000000000)
    , ("trillion", 1000000000000)
    , ("quadrillion", 1000000000000000)
    , ("quintillion", 1000000000000000000)
    , ("sexillion", 1000000000000000000000)
    , ("septillion", 1000000000000000000000000)
    , ("octillion", 1000000000000000000000000000)
    , ("nonillion", 1000000000000000000000000000000)
    , ("decillion", 1000000000000000000000000000000000)
    ]
