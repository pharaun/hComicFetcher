{-# LANGUAGE FlexibleContexts #-}
module Sites.Util
    ( volChpParse
    , fixChp
    , fixVol
    , wordToNumber

    -- Testing
    , cleanSegment
    , breakSegment

    , parseKeyword
    , Keyword(..)

    , parseContent
    , parseSegment
    , digitsParse
    , singleDigit
    , simplifiedDigit
    , simplifiedSubDigit
    , dotSubDigit

    , textExceptVersionParse
    ) where

import qualified Data.List as DL
import qualified Data.Text as T

-- Parsec
import Text.Parsec
import Text.Parsec.Text
import Data.Functor.Identity (Identity)
import Control.Applicative ((*>), (<*), (<*>), (<$>), (<$), pure, liftA)
import Data.Monoid (mconcat)
import Data.Either (either)
import Data.Maybe (maybe)
import Control.Monad

-- Testing
import Test.HUnit

-- TODO: Quickcheck this, we have a pretty printer so it should be quite
-- doable to quickcheck this
import Debug.Trace

-- Local Imports
import Types


-- Some pack Instances to make it easy to concat the output of parsec back into Text
class PackToText a where
    toText :: a -> T.Text
instance PackToText Char where
    toText = T.singleton
instance PackToText T.Text where
    toText = id
instance (PackToText a) => PackToText [a] where
    -- Not the fastest instance on the block (7us vs 1.1us for T.pack) but fast enough
    toText = mconcat . map toText

infixl 4 <++>
f <++> g = (\x y -> toText x `T.append` toText y) <$> f <*> g


-- Site, Story, "Vol/Chp/etc" parse
volChpParse :: String -> Maybe String -> String -> ComicTag
volChpParse site story segment
    -- There's nothing to parse
    | null segment = ComicTag (T.pack site) (maybe Nothing (Just . T.pack) story) Nothing Nothing Nothing

    -- Let's get the parsing party on
    | otherwise = case traceShow segment (breakSegment $ cleanSegment (T.pack segment)) of

        -- TODO: evil bits
        Left e  -> error e
        Right s -> do
            -- Grab one of each keyword if it exists
            let vol = DL.lookup Vol s
            let chp = DL.lookup Chp s
            let title = DL.lookup Title s

            -- Volume or Chapter is optional as long as we have 1 of either, title is optional as well
            case vol of
                Nothing ->
                    case chp of
                        Nothing -> error "Missing both Volume and Chapter"
                        Just c1 ->
                            case (parse parseSegment "Chapter" c1) of
                                Left e   -> error $ show e
                                Right c2 ->
                                    let c3 = UnitTag c2 title
                                    in ComicTag (T.pack site) (maybe Nothing (Just . T.pack) story) Nothing (Just c3) Nothing

                Just v1  ->
                    case (parse parseSegment "Volume" v1) of
                        Left e   -> error $ show e
                        Right v2 ->
                            case chp of
                                Nothing ->
                                    let v3 = UnitTag v2 title
                                    in ComicTag (T.pack site) (maybe Nothing (Just . T.pack) story) (Just v3) Nothing Nothing

                                Just c4 ->
                                    case (parse parseSegment "Chapter" c4) of
                                        Left e   -> error $ show e
                                        Right c5 ->
                                            let v4 = UnitTag v2 Nothing
                                                c6 = UnitTag c5 title

                                            in ComicTag (T.pack site) (maybe Nothing (Just . T.pack) story) (Just v4) (Just c6) Nothing

-- First pass
--  - Trim string
--  - Remove " Read Online"
--  - TODO: Check if its one that should be discarded? ([Oneshot], [Complete]) ?
cleanSegment :: T.Text -> T.Text
cleanSegment t =
    let clean = T.strip t
    in case T.stripSuffix (T.pack " Read Online") clean of
        Nothing -> clean
        Just t' -> t'

-- Second pass
--  - Break up in 3 part vol, chp, and optional title
--  - Reconstructs the title if there's multiples
--  - Trims the results
--  - Runs sanity check to ensure there is at most 1 of each types.
--
-- {Vol}[ .]{...} {Chp}[ .]{...} [ :]{Chp Title}{eof}
breakSegment :: T.Text -> Either String [(Keyword, T.Text)]
breakSegment segment = sanityCheck $ either (Left . show) Right $ liftM trim $ liftM merge $ parse chunk "" segment
   where
    trim    = map (\(a, b) -> (a, T.strip b))
    chunk   = many ((,) <$> parseKeyword <*> parseContent) <* eof

    -- Merge all segments with Title in it.
    merge s =
        let (titles, seg) = DL.partition (\a -> Title == fst a) s
        in if DL.null titles
           then seg
           else (Title, T.intercalate (T.pack ":") (map snd titles)) : seg

    -- Sanity check to make sure there isn't more than 1 of any keyword if it exists
    sanityCheck (Left e)  = Left e
    sanityCheck (Right s) = if DL.any (\t -> DL.length (DL.filter (\a -> t == fst a) s) > 1) [Vol, Chp, Title]
                            then Left $ "More than 1 of a keyword: " ++ show s
                            else Right s

-- TODO: may be better to just let the Title fall through and let the Vol or Chp unit tag eat it up
data Keyword = Vol | Chp | Title deriving (Eq, Show)

parseKeyword :: ParsecT T.Text u Identity Keyword
parseKeyword =
    (Vol <$ string "Vol") <|>
    (Vol <$ string "Volume") <|>
    (Chp <$ string "Ch") <|>
    (Chp <$ string "Chp") <|>
    (Chp <$ string "Chapter") <|>
    (Title <$ string ":") <?>
    "keyword (Vol, Chp, Title)"

-- TODO: a Chp/Vol expects a space or a dot after
parseContent :: ParsecT T.Text u Identity T.Text
parseContent = (T.pack <$> many space)
          <++> ((T.empty <$ lookAhead (try (parseKeyword <* oneOf " .")))
           <|> option T.empty (T.pack <$> many1 (noneOf " \t:") <++> parseContent))




-- Third pass
--  - Parse each segment (vol, chp) in isolation
parseSegment :: ParsecT T.Text u Identity [Digits]
parseSegment = (skipMany $ oneOf " .") *> option [] digitsParse <* eof

-- {digits} -> ( {simplified_digit}-{simplified_digit}(,{digits})? | {simplified_digit}(,{digits})? | {single_digit} ){eof}
--
-- TODO: this fails for - "1.5v3-5.1v1,10.0v6-22v1,29.Eternity,30"
-- [ RangeDigit (Digit 1 (Just (DotSubDigit (Just 5) "")) (Just 3))  -- 1.5v3
--              (Digit 5 (Just (DotSubDigit (Just 1) "")) (Just 1))  -- 5.1v1
-- , RangeDigit (Digit 10 (Just (DotSubDigit (Just 0) "")) (Just 6)) -- 10.0v6
--              (Digit 22 Nothing (Just 1))                          -- 22v1
-- , StandAlone (Digit 29 (Just (DotSubDigit Nothing "Eternity,30")) Nothing) -- 29.Eternity,30 -- Should be 29.Eternity and 30
-- ]
digitsParse :: ParsecT T.Text u Identity [Digits]
digitsParse = choice
        [ try (do
            range <- RangeDigit <$> simplifiedDigit <*> (char '-' >> simplifiedDigit)

            next <- option [] (char ',' >> digitsParse)
            return $ [range] ++ next
          )
        , try (do
            digit <- StandAlone <$> simplifiedDigit

            next <- option [] (char ',' >> digitsParse)
            return $ [digit] ++ next
          )
        , (:[]) `fmap` (StandAlone <$> singleDigit)
        ]

-- {single_digit} -> {num} ( {subdigit:[.]}{version} | {subdigit:[.]} | {text} )?
-- TODO: May be able to just merge it into simplifiedDigit
singleDigit :: ParsecT T.Text u Identity Digit
singleDigit = do
    digit <- numParse
    -- TODO: Fix this so it will check that its not a version first
    subdigit <- optionMaybe dotSubDigit
    version <- optionMaybe version

    -- TODO: extend this to deal with {text} case
    return $ Digit digit subdigit version Nothing

-- {simplified_digit} -> {num} ( {simplified_subdigit}?{version}?{letter}? )
simplifiedDigit :: ParsecT T.Text u Identity Digit
simplifiedDigit = Digit <$> numParse <*> optionMaybe simplifiedSubDigit <*> optionMaybe version <*> optionMaybe letterParse

-- {simplified_subdigit} -> [.]{num}
--  TODO: have this fail? if there's nothing after the dot
simplifiedSubDigit :: ParsecT T.Text u Identity SubDigit
simplifiedSubDigit = char '.' >> (DotSubDigit <$> (Just <$> numParse) <*> (pure T.empty))

-- {subdigit} ->
--     [.] -> ( {num}{text} | {text} )
--  TODO: First case its probably 10.9a, second case .Foobar (thus a label)
--  TODO: Maybe have a textParse that will parse space or not
--  TODO: Fix this so it will check that its not a version first
--  TODO: have this fail? if there's nothing after the dot
dotSubDigit :: ParsecT T.Text u Identity SubDigit
dotSubDigit = char '.' >> (DotSubDigit <$> optionMaybe numParse <*> option T.empty textExceptVersionParse)




-- {version} -> v{num}
version :: ParsecT T.Text u Identity Integer
version = char 'v' *> numParse

--  {num} -> [0-9]+
numParse :: ParsecT T.Text u Identity Integer
numParse = liftA read (many1 digit)

-- {text} -> [A-z][A-z0-9 ]*{eof}
-- TODO: maybe make this optional to make parsing easier
-- TODO: maybe expand the acceptable characters
textParse :: ParsecT T.Text u Identity T.Text
textParse = letter <++> many anyChar <* eof

-- {textExceptVersion} -> [A-z][A-z0-9 ]*
-- TODO: maybe expand the acceptable characters
textExceptVersionParse :: ParsecT T.Text u Identity T.Text
textExceptVersionParse = do
    text <- optionMaybe $ try $ manyTillPutBack anyChar (try version)

    case text of
        -- TODO: Need to make error less confusing (expecting "v") for empty string??
        Nothing -> textParse
        Just t  -> return $ T.pack t

-- {letter} -> [A-z0-9]*
letterParse :: ParsecT T.Text u Identity T.Text
letterParse = letter <++> many alphaNum <* eof

-- This will attempt to match ending but then put it back into the stream
manyTillPutBack :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTillPutBack p end = scan
   where
    scan =  do{ lookAhead end; return [] }
        <|>
            do{ x <- p; xs <- scan; return (x:xs) }



-- Format:
-- Chp 00
-- Chp TBD
-- Chp.00
-- Chp.TBD
-- Chp 00: title
-- Chp.00: title
-- Chp.TBD: title
--
-- Chp 00.1
-- Chp.00.1
-- Chp 00.1: title
-- Chp.00.1: title
--
-- Chp Extra
-- Chp.Extra
-- Chp Extra: title
-- Chp.Extra: title
--
-- Chp Extra Content
-- Chp.Extra Content
-- Chp Extra Content: title
-- Chp.Extra Content: title
--
-- Chp 00v2
-- Chp.00v2
-- Chp 00v2: title
-- Chp.00v2: title
--
-- Chp 00.1v2
-- Chp.00.1v2
-- Chp 00.1v2: title
-- Chp.00.1v2: title
--
-- Chp 00a
-- Chp.00a
-- Chp 00a: title
-- Chp.00a: title
--
-- Chp 00va
-- Chp.00va
-- Chp 00va: title
-- Chp.00va: title
--
-- Chp 00 foobar
-- Chp.00 foobar
-- Chp 00.foobar
-- Chp.00.foobar
-- Chp 00 foobar: title
-- Chp.00 foobar: title
-- Chp 00.foobar: title
-- Chp.00.foobar: title
--
-- Chp 1,2
-- Chp.1,2
-- Chp 1,2: title
-- Chp.1,2: title
--
-- Chp 1-2
-- Chp.1-2
-- Chp 1-2: title
-- Chp.1-2: title
--
-- Chp 1-2,4
-- Chp.1-2,4
-- Chp 1-2,4: title
-- Chp.1-2,4: title
--
-- MISC
--  - Vol x Chp y: title
--  - Chp y: title
--  - Chp y Read Online
--  - Chp.Foo Bar Read Online
--  - Chp.0: [Oneshot]
--  - Chp.0: [Complete]


--runTests :: IO ()
--runTests = do
--    putStrLn "volParse"
--    print =<< runTestTT (buildParseTests volParse volParseData)
--
--buildParseTests parser testData = TestList $ map (\(src, dst) -> TestLabel (src ++ " -> " ++ dst) (parserTest parser src dst)) testData
--parserTest parser src dst = TestCase (assertEqual "" (Just [dst]) (either (const Nothing) Just (parse parser "(stdin)" src)))

volParseData :: [(String, String)]
volParseData =
    [ ("", "")
    , ("Volume 02", "")
    , ("Vol 12", "")
    , ("Vol TBD", "")
    , ("Vol.43", "")
    , ("Vol.01", "")
    , ("Vol.01v2", "")
    , ("Vol.01.3", "")
    , ("Vol.01.herp", "")
    , ("Vol.01.herp omake", "")
    , ("Vol.01 herp", "")
    , ("V.01", "")
    , ("V1", "")
    , ("V 1", "")
    , ("Vol 2-7", "")
    , ("Vol 2,5,7", "")
    , ("Vol 2-5,7", "")
    , ("Vol 1,3-5,7", "")
    , ("Vol 1,3-5,7-9", "")
    , ("Vol 4a", "")
    , ("Vol 4.3", "")
    , ("Vol 4v2", "")
    ]





fixChp :: String -> (Integer, Maybe T.Text)
fixChp = extract . cleanChapter

extract :: String -> (Integer, Maybe T.Text)
extract ('C':'h':'a':'p':'t':'e':'r':' ':a:b:xs) = (read [a, b], let txs = T.strip $ T.dropWhile (== ':') $ T.pack xs in if T.null txs then Nothing else Just txs)

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
extractV ('V':'o':'l':'u':'m':'e':' ':a:b:xs) = (read [a, b], let txs = T.strip $ T.dropWhile (== ':') $ T.pack xs in if T.null txs then Nothing else Just txs)

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



-- Process a sequence of english number into an integer value
-- If invalid/incomplete/what so not it will return a 0
wordToNumber :: String -> Integer
wordToNumber = DL.foldl' compute 0 . map T.unpack . T.words . T.toLower . T.pack
    where
        compute :: Integer -> String -> Integer
        compute prior word =
            let v = DL.lookup word numberWordTable
            in case v of
                Just x  -> x + prior
                Nothing ->
                    let w = DL.lookup word multiplerWordTable
                    in case w of
                        Just y  -> y * prior
                        Nothing -> prior

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
