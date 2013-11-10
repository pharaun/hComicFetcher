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

import Control.Exception (throw)


-- Local imports
import Types
import Fetcher


--
-- SEQUAL SCAN COMICS
--

--
-- Does Not Play Well With Others
--
doesNotPlayWellWithOthers = Comic
    { comicName = "Does Not Play Well With Others"
    , seedPage = "http://www.doesnotplaywellwithothers.com/comics/pwc-000f"
    , seedType = undefined

    , pageParse = dnpwwoPageParse
    }

dnpwwoPageParse :: ReplyType t -> IO [FetchType t]
dnpwwoPageParse (WebpageReply html _) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    next <- runX $ doc //> nextPage
    img <- runX $ doc //> comic

    -- Do we have any comic we want to store to disk?
    putStrLn "Fetched Urls:"
    mapM_ putStrLn img
    mapM_ putStrLn next

    return $ (map (\a -> Webpage a undefined) next) ++ (map (\a -> Image a $ comicFileName a) img)

   where
    nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"
    comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    comicFileName url = ComicTag (T.pack "does_not_play_well_with_others") Nothing Nothing Nothing (Just $ last $ decodePathSegments $ US.fromString url)

--
-- Exploitation Now
--
exploitationNow = Comic
    { comicName = "Exploitation Now"
    , seedPage = "http://www.exploitationnow.com/2000-07-07/9"
    , seedType = undefined

    , pageParse = exploitationPageParse
    }

exploitationPageParse :: ReplyType t -> IO [FetchType t]
exploitationPageParse (WebpageReply html _) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    next <- runX $ doc //> nextPage
    img <- runX $ doc //> comic
    vol <- (exploitationNowVol . concat) `fmap` (runX $ doc //> whichVol)

    -- Do we have any comic we want to store to disk?
    putStrLn "Fetched Urls:"
    mapM_ putStrLn img
    mapM_ putStrLn next

    return $ (map (\a -> Webpage a undefined) next) ++ (map (\a -> Image a $ comicFileName vol a) img)

   where
    nextPage = hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href" >>> getAttrValue "href"
    comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    comicFileName vol url = ComicTag (T.pack "exploitation_now") Nothing vol Nothing (Just $ last $ decodePathSegments $ US.fromString url)
    whichVol =
        hasName "body"
        >>> hasAttr "class"
        >>> getAttrValue "class"
        >>> arr words
        >>> arr (filter (isPrefixOf "single-category"))
        >>> arr (filter (not . isSuffixOf "comic"))
        >>> arr (filter (not . isSuffixOf "comics"))
        >>> arr (filter (not . isSuffixOf "uncategorized"))
        >>> arr concat

    exploitationNowVol :: String -> Maybe UnitTag
    exploitationNowVol "single-category-act-one"         = Just $ UnitTag 1 $ Just $ T.pack "Act One"
    exploitationNowVol "single-category-act-two"         = Just $ UnitTag 2 $ Just $ T.pack "Act Two"
    exploitationNowVol "single-category-intermission-i"  = Just $ UnitTag 3 $ Just $ T.pack "Intermission I"
    exploitationNowVol "single-category-act-three"       = Just $ UnitTag 4 $ Just $ T.pack "Act Three"
    exploitationNowVol "single-category-intermission-ii" = Just $ UnitTag 5 $ Just $ T.pack "Intermission II"
    exploitationNowVol "single-category-act-four"        = Just $ UnitTag 6 $ Just $ T.pack "Act Four"
    exploitationNowVol _ = Nothing


--
-- Girl Genius
--
girlGenius = Comic
    { comicName = "Girl Genius"
    , seedPage = "http://www.girlgeniusonline.com/comic.php?date=20021104"
    , seedType = undefined

    , pageParse = girlGeniusPageParse
    }

girlGeniusPageParse :: ReplyType t -> IO [FetchType t]
girlGeniusPageParse (WebpageReply html _) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    next <- runX $ doc //> nextPage
    img <- runX $ doc //> comic
    vol <- concat `fmap` (runX $ (whichVol doc))

    -- Do we have any comic we want to store to disk?
    putStrLn "Fetched Urls:"
    mapM_ putStrLn img
    mapM_ putStrLn next

    return $ (map (\a -> Webpage a undefined) next) ++ (map (\a -> Image a $ comicFileName vol a) img)

   where
    nextPage = hasName "td" >>> hasAttrValue "valign" (== "top") //> (hasName "a" </ (hasName "img" >>> hasAttrValue "alt" (== "The Next Comic"))) >>> getAttrValue "href"
    comic =
        hasName "td"
        >>> hasAttrValue "valign" (== "middle")
        //> hasName "img"
        >>> hasAttr "src"
        >>> getAttrValue "src"
        >>. arr (filter (isPrefixOf "http"))

    comicFileName vol url = ComicTag (T.pack "girl_genius") Nothing (Just $ UnitTag (girlGeniusVol vol) Nothing) Nothing (Just $ last $ decodePathSegments $ US.fromString url)

    -- TODO: this returns a single string (cuz its concating all of this), we worked around this but this is very much non-ideal
    --  For workaround see girlGeniusVol
    whichVol doc =
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

    girlGeniusVol :: String -> Integer
    girlGeniusVol a = wordToNumber (DL.reverse $ DL.head $ DL.words $ DL.drop 3 $ DL.reverse a)


--
-- INDEX BASED COMIC
--

--
-- Gunnerkrigg Court
--
gunnerkrigCourt = Comic
    { comicName = "Gunnerkrigg Court"
    , seedPage = "http://www.gunnerkrigg.com/archives/"
    , seedType = undefined

    , pageParse = gunnerkrigCourtPageParse
    }

gunnerkrigCourtPageParse :: ReplyType t -> IO [FetchType t]
gunnerkrigCourtPageParse (WebpageReply html _) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    chpPages <- runX $ chapterPage doc

    -- Dump list of Comic page fetched
    putStrLn "Chp list:"
    mapM_ (putStrLn . fst) chpPages

    return (DL.concatMap (\(chp, img) -> map (\i -> Image i (comicFileName chp i)) img) chpPages)

   where
    -- TODO: this can probably be made more specific/nicer
    comicFileName filepath url =
        let (chp, name) = fixChp filepath
        in ComicTag (T.pack "gunnerkrigg_court") Nothing Nothing (Just $ UnitTag chp name) (Just $ last $ decodePathSegments $ US.fromString url)

    chapterPage doc = listA (doc
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

    linkComic :: String -> String
    linkComic u = "http://www.gunnerkrigg.com/comics/" ++ padNum u ++ ".jpg"
        where
            padNum n = (DL.concat $ DL.take (8 - DL.length u) (DL.repeat "0")) ++ n

    tupleComic :: [String] -> Maybe (String, [String])
    tupleComic [] = Nothing
    tupleComic (x:xs) = Just (x, xs)


-- TODO: errant story only
data Tag = VolIndex -- Volume Index page
         | Chapter ComicTag -- Entire chapters page
         | Page ComicTag -- single comic page

-- Errant Story
errantStory :: Comic Tag
errantStory = Comic
    { comicName = "Errant Story"
    , seedPage = "http://www.errantstory.com"
    , seedType = VolIndex

    , pageParse = errantStoryPageParse
    }

errantStoryPageParse :: ReplyType Tag -> IO [FetchType Tag]
errantStoryPageParse (WebpageReply html VolIndex) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    index <- runX $ doc //> indexList

    -- TODO: have a pre-process step for processing the parsing results into a useful list for dumping into TBMChans
    let defaultErrantStory = ComicTag (T.pack "errant_story") Nothing Nothing Nothing Nothing
    let list = buildUrlAndComicTagMapping defaultErrantStory index

    -- Dump list of Comic page fetched
    putStrLn "Chp list:"
    mapM_ print list

    return $ map (\(u, p) -> Webpage u (Chapter p)) list

  where
    indexList =
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
    levelToComicTagMapping parent (url, ("level-3", name)) =
        let (chp, chpName) = fixChp name
        in (url, parent {ctChapter = Just $ UnitTag chp chpName})
    levelToComicTagMapping parent (url, ("level-2", name)) =
        let (vol, volName) = fixVol name
        in (url, parent {ctVolume = Just $ UnitTag vol volName})
    levelToComicTagMapping parent (url, ("level-1", name)) = (url, parent {ctStoryName = Just $ T.pack name})
    levelToComicTagMapping parent content = throw $ DebugException "levelToComicTagMapping" ("Parent: " ++ show parent ++ " - Content: " ++ show content)

errantStoryPageParse (WebpageReply html (Chapter ct)) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    chp <- runX $ doc //> chapterList
    next <- runX $ doc //> chapterNextPage

    -- Do we have any comic we want to store to disk?
    putStrLn "Chp list:"
    mapM_ print chp

    putStrLn "Next archive:"
    mapM_ print next

    -- TODO:
    --  1. Do something with the name of the page (Chapter 42: foobar) (not on all pages unfortunately)
    return $ (map (\a -> Webpage a (Chapter ct)) next) ++ (map (\a -> Webpage (fst a) (Page ct)) chp)

   where
    chapterList =
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

    chapterNextPage =
        hasName "div"
        >>> hasAttrValue "class" (== "pagenav-right")
        >>> getChildren
        >>> hasName "a"
        >>> hasAttr "href"
        >>> getAttrValue "href"

errantStoryPageParse (WebpageReply html (Page ct)) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    img <- runX $ doc //> comic

    -- Do we have any comic we want to store to disk?
    putStrLn "Fetched Urls:"
    mapM_ putStrLn img

    return (map (\a -> Image a $ comicTagFileName ct a) img)

   where
    comic = hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src" >>> getAttrValue "src"
    comicTagFileName ct url = ct{ctFileName = Just $ last $ decodePathSegments $ US.fromString url}



----
---- Batoto
----
--batoto = Comic
--    { comicName = "Batoto"
--    , seedPage = "http://www.batoto.net/comic/_/comics/yotsubato-r311"
--    , seedType = VolChpIndex
--
--    , nextPage = undefined
--
--    , comic = undefined
--
--    , whichVolChp = undefined
--    , indexList = undefined
--    , chapterList = undefined
--    , chapterNextPage = undefined
--    , chapterPage = undefined
--
--    , comicTagFileName = undefined
--    , comicFileName = \_ url ->
--        ComicTag (T.pack "batoto") (Just $ T.pack "yotsubato") Nothing Nothing Nothing
--    }





-- TODO:
--  - Defined stop point, Errant Story
--  - Some command line arg for picking which comic to run
main = do
--    processTarget doesNotPlayWellWithOthers
--    processTarget exploitationNow
--    processTarget girlGenius
--    processTarget gunnerkrigCourt
    processTarget errantStory
--    processTarget batoto

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














fixChp :: String -> (Integer, Maybe T.Text)
fixChp = extract . cleanChapter

extract :: String -> (Integer, Maybe T.Text)
extract ('C':'h':'a':'p':'t':'e':'r':' ':a:b:xs) = (read $ [a, b], (let txs = T.strip $ T.dropWhile (== ':') $ T.pack xs in if T.null txs then Nothing else Just txs))

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



fixVol :: String -> (Integer, Maybe T.Text)
fixVol = extractV . cleanVolume

extractV :: String -> (Integer, Maybe T.Text)
extractV ('V':'o':'l':'u':'m':'e':' ':a:b:xs) = (read $ [a, b], (let txs = T.strip $ T.dropWhile (== ':') $ T.pack xs in if T.null txs then Nothing else Just txs))

-- TODO: this is terribad but it'll let us have consistent chp/vol stuff
-- for cheap extracting
cleanVolume :: String -> String
cleanVolume ('V':'o':'l':'u':'m':'e':' ':x:'0':xs) = "Volume " ++ [x] ++ "0" ++ xs
cleanVolume ('V':'o':'l':'u':'m':'e':' ':x:'1':xs) = "Volume " ++ [x] ++ "1" ++ xs
cleanVolume ('V':'o':'l':'u':'m':'e':' ':x:'2':xs) = "Volume " ++ [x] ++ "2" ++ xs
cleanVolume ('V':'o':'l':'u':'m':'e':' ':x:'3':xs) = "Volume " ++ [x] ++ "3" ++ xs
cleanVolume ('V':'o':'l':'u':'m':'e':' ':x:'4':xs) = "Volume " ++ [x] ++ "4" ++ xs
cleanVolume ('V':'o':'l':'u':'m':'e':' ':x:'5':xs) = "Volume " ++ [x] ++ "5" ++ xs
cleanVolume ('V':'o':'l':'u':'m':'e':' ':x:'6':xs) = "Volume " ++ [x] ++ "6" ++ xs
cleanVolume ('V':'o':'l':'u':'m':'e':' ':x:'7':xs) = "Volume " ++ [x] ++ "7" ++ xs
cleanVolume ('V':'o':'l':'u':'m':'e':' ':x:'8':xs) = "Volume " ++ [x] ++ "8" ++ xs
cleanVolume ('V':'o':'l':'u':'m':'e':' ':x:'9':xs) = "Volume " ++ [x] ++ "9" ++ xs
cleanVolume ('V':'o':'l':'u':'m':'e':' ':xs) = "Volume 0" ++ xs
cleanVolume xs = xs
















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
