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
import Text.Parsec.String (Parser)
import Data.Functor.Identity (Identity)

-- Testing
import Test.HUnit

-- Local Imports
import Types

-- Site, Story, "Vol/Chp/etc" parse
volChpParse :: String -> String -> String -> ComicTag
volChpParse = undefined

volParse :: ParsecT String u Identity [String]
volParse = do
    label <- choice -- TODO: can probably make more efficient by building up from V,Vol,Volume
        [ try $ string "Volume"
        , try $ string "Vol"
        , string "V"
        ]

    -- Eat a space/dot
    skipMany $ oneOf " ."

    -- Identifiers (tbd)
    ident <- try $ string "TBD"

    -- Version on vol

    -- Subvol (x.5, Xa, X.foo)

    -- Volume span/collections (2-3, 4,5,3)

    rest <- many anyChar

    return [label, ident, rest]


numParse :: ParsecT String u Identity [String]
numParse = do
    v <- (many1 (choice [digit, char '-'])) `sepBy` (char ',')


    rest <- many anyChar
    return $ v ++ [rest]


testNum = mapM_ (parseTest numParse)
    [ "1"
    , "01"
    , "01v2"
    , "01.3"
    , "01.herp"
    , "1,2,3"
    , "1-3"
    , "1,3-5"
    , "1-3,5"
    , "1a"
    ]


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





-- Format:
-- Vol 12 Ch 079: Name V
-- Vol TBD Ch 353: Foobar stuff
-- Ch 087: weird
-- Ch 000
-- Ch 032.5
-- Ch 032.1
-- Ch 087v2: foobar
-- Ch.23: foo
-- Vol.43 Ch.3234: Barz
-- Vol.32 Ch.123 Read Online
-- Vol.4 Chp.Extra: Foobar
-- Vol.4 Chp.Extra 2: Foobar
-- Ch.0: [Oneshot]
-- Ch.0: [Complete]
-- Vol.1 Ch.3 Extra: bar
-- Vol.03 Ch.13a
-- Vol.03 Ch.13cd
-- Vol.4 Chp.23-27: Foobar
-- Vol.2 Chp.Fanbook Omake Read Online



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
