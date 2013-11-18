{-# LANGUAGE FlexibleContexts #-}
module Sites.Util
    ( volChpParse
    , fixChp
    , fixVol
    , wordToNumber
    ) where

import qualified Data.List as DL
import qualified Data.Text as T

-- Parsec
import Text.Parsec
import Text.Parsec.Text
import Data.Functor.Identity (Identity)
import Control.Applicative ((*>), (<*), (<*>), (<$>), (<$), pure, liftA)
import Data.Monoid (mconcat)

-- Testing
import Test.HUnit

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
volChpParse :: String -> String -> String -> ComicTag
volChpParse = undefined


-- First pass
--  - Trim string
--  - Remove " Read Online"
--  - TODO: Check if its one that should be discarded? ([Oneshot], [Complete]) ?
firstPass :: T.Text -> T.Text
firstPass t =
    let clean = T.strip t
    in case T.stripSuffix (T.pack " Read Online") clean of
        Nothing -> clean
        Just t' -> t'


-- Second pass
--  - Break up in 3 part vol, chp, and optional title
--  - TODO: Reconstruct the title if it parses multiples
--
-- {Vol}[ .]{...} {Chp}[ .]{...} [ :]{Chp Title}{eof}
secondPass :: ParsecT T.Text u Identity [(Keyword, T.Text)]
secondPass = many ((,) <$> parseKeyword <*> parseContent) <* eof

data Keyword = Vol | Chp | Title deriving (Eq, Show)

parseKeyword :: ParsecT T.Text u Identity Keyword
parseKeyword =
    (Vol <$ string "Vol") <|>
    (Chp <$ string "Chp") <|>
    (Title <$ string ":") <?>
    "keyword (Vol, Chp, Title)"

parseContent :: ParsecT T.Text u Identity T.Text
parseContent = (T.pack <$> many space)
          <++> ((T.empty <$ lookAhead (try parseKeyword))
           <|> option T.empty (T.pack <$> many1 (noneOf " \t:") <++> parseContent))


-- Third pass
--  - Parse each segment (vol, chp) in isolation
thirdPass :: [(Keyword, T.Text)] -> [T.Text]
thirdPass = mconcat . map pass
    where
        pass :: (Keyword, T.Text) -> [T.Text]
        pass (Title, t) = [t]
        pass (_, t) = [t]

volParse :: ParsecT String u Identity [String]
volParse = undefined
--volParse = do
--    -- {Vol}
--    label <- choice
--        -- TODO: can probably make more efficient by building up from V,Vol,Volume
--        [ try $ string "Volume"
--        , string "Vol"
--        ]
--
--    -- [ .]
--    skipMany $ oneOf " ."
--
--    -- ( {digits} | {text} )
--    ident <- choice
--        [ try digitsParse
--        , (:[]) `fmap` textParse
--        ]
--
--    return $ label: ident


data Digits = RangeDigit Digit Digit
            | StandAlone Digit
            deriving (Show)

-- {digits} -> ( {simplified_digit}-{simplified_digit}(,{digits})? | {simplified_digit}(,{digits})? | {single_digit} ){eof}
digitsParse :: ParsecT T.Text u Identity [Digits]
digitsParse = do
    -- Try for simplified digits first
    digits <- choice
        [ try (do
            digit1 <- simplifiedDigit
            char '-'
            digit2 <- simplifiedDigit

            keepGoing <- optionMaybe (char ',' >> digitsParse)
            case keepGoing of
                Nothing -> return [RangeDigit digit1 digit2]
                Just k  -> return $ RangeDigit digit1 digit2 : k
          )
        , try (do
            digit <- simplifiedDigit

            keepGoing <- optionMaybe (char ',' >> digitsParse)
            case keepGoing of
                Nothing -> return [StandAlone digit]
                Just k  -> return $ StandAlone digit : k
          )
        , (do
            digit <- singleDigit
            return [StandAlone digit]
          )
        ]

    -- TODO: this fails for - "1.5v3-5.1v1,10.0v6-22v1,29.Eternity,30"
    -- [RangeDigit (Digit 1 (Just (DotSubDigit (Just 5) "")) (Just 3)) (Digit 5 (Just (DotSubDigit (Just 1) "")) (Just 1)),RangeDigit (Digit 10 (Just (DotSubDigit (Just 0) "")) (Just 6)) (Digit 22 Nothing (Just 1)),StandAlone (Digit 29 (Just (DotSubDigit Nothing "Eternity,30")) Nothing)]
    --
    return digits


data Digit = Digit Integer (Maybe SubDigit) (Maybe Integer)
           deriving (Show)


--  {single_digit} -> {num} ( {subdigit:[.]}{version} | {subdigit:[.]} | {text} )?
singleDigit :: ParsecT T.Text u Identity Digit
singleDigit = do
    digit <- numParse
--  TODO: Fix this so it will check that its not a version first
    subdigit <- optionMaybe dotSubDigit
    version <- optionMaybe version

    -- TODO: extend this to deal with {text} case
    return $ Digit digit subdigit version

-- {simplified_digit} -> {num} ( {simplified_subdigit}?{version}? )
simplifiedDigit :: ParsecT T.Text u Identity Digit
simplifiedDigit = Digit <$> numParse <*> optionMaybe simplifiedSubDigit <*> optionMaybe version


-- Sub Digits
data SubDigit = DotSubDigit (Maybe Integer) T.Text
              deriving (Show)

-- {simplified_subdigit} -> [.]{num}
simplifiedSubDigit :: ParsecT T.Text u Identity SubDigit
simplifiedSubDigit = char '.' >> (DotSubDigit <$> (Just <$> numParse) <*> (pure T.empty))

-- {subdigit} ->
--     [.] -> ( {num}{text} | {text} )
--  TODO: First case its probably 10.9a, second case .Foobar (thus a label)
--  TODO: Maybe have a textParse that will parse space or not
--  TODO: Fix this so it will check that its not a version first
dotSubDigit :: ParsecT T.Text u Identity SubDigit
dotSubDigit = char '.' >> (DotSubDigit <$> optionMaybe numParse <*> option T.empty textParse)


-- {version} -> v{num}
version :: ParsecT T.Text u Identity Integer
version = char 'v' *> numParse

--  {num} -> [0-9]+
numParse :: ParsecT T.Text u Identity Integer
numParse = liftA read (many1 digit)

-- {text} -> [A-z][A-z0-9 ]*{eof}
-- TODO: maybe make this optional to make parsing easier
textParse :: ParsecT T.Text u Identity T.Text
textParse = letter <++> many anyChar <* eof




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


runTests :: IO ()
runTests = do
    putStrLn "volParse"
    print =<< runTestTT (buildParseTests volParse volParseData)

buildParseTests parser testData = TestList $ map (\(src, dst) -> TestLabel (src ++ " -> " ++ dst) (parserTest parser src dst)) testData
parserTest parser src dst = TestCase (assertEqual "" (Just [dst]) (either (const Nothing) Just (parse parser "(stdin)" src)))

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



---- Filesystem format - SiteName/StoryName/Volume/Chapter/Page.*
--data ComicTag = ComicTag
--    { ctSiteName :: T.Text
--    , ctStoryName :: Maybe T.Text
--
--    , ctVolume :: Maybe UnitTag
--    , ctChapter :: Maybe UnitTag
--
--    , ctFileName :: Maybe T.Text -- TODO: need to find a way to make this mandatory...
----    , ctPage :: Maybe UnitTag --  TODO: Implement this, for now we just use file name
--    }
--    deriving (Show)
--
--data UnitTag = UnitTag
--    { utNumber :: Integer
--    , utTitle :: Maybe T.Text
--    }
--    deriving (Show)
--
--data UnitTagType = UnitTagVolume | UnitTagChapter




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


--packTextBenchmark = do
--    let target2 = "this is an pretty long string full of stuff and you know"
--    let target3 = T.pack "this is an pretty long string full of stuff and you know"
--
--    let dest = T.pack "this is an pretty long string full of stuff and you know"
--
--    let myConfig = defaultConfig { cfgSamples = ljust 10000 }
--
--    defaultMainWith myConfig (return ())
--        [ bgroup "T.Text concat"
--            [ bench "T.pack"      $ nf (T.append dest . T.pack) target2
--            , bench "mconcat"     $ nf (T.append dest . (mconcat . map T.singleton)) target2
--            , bench "id"          $ nf (T.append dest) target3
--            ]
--        ]
