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
    , "01.herp derp"
    , "01 herp derp"
    , "1,2,3"
    , "1-3"
    , "1,3-5"
    , "1-3,5"
    , "1a"
    ]



-- First pass
--  - Trim string
--  - Remove " Read Online"
--
-- Second pass
--  - Break up in 2 part vol/chp and optional title
--  - Trim the optional title
--  - Check if its one that should be discarded? ([Oneshot], [Complete]) ?
--
-- Third pass
--  - Attempt to break up Vol/Chp?


--
-- Formal Grammar Definition for Vol/Chp segments
--
-- IFF - If and only If
--
-- {Vol}
-- [ .]
--
-- ( {text} | {digits} )
--
--  {text} -> ( TBD | [A-z0-9 ]+ ){eos} - Basically anything except {eos}
--
--  {digits} -> ( {single_digit} | ( {single_digit},{digits} | {single_digit}-{single_digit} ( {eos} | ,{digits} ) ) )
--      TODO: not sure if ^ is the best rule
--      TODO: extend digits rules to include 1-2 and 1,2 approaches
--      TODO: do not allow text (only allow version) in the multi-digit & spread-digit
--
--  {single_digit} -> [0-9]+ ( {version}?{sub_digit:[ ]}? | {sub_digit}? )
--
--      {sub_digit}
--      [.] -> ( [0-9]+ | [A-z0-9 ]+{eos} ) - TODO: this means its probably the chapter label such as "Chp 09.Extra 2"
--
--      [ ] -> ( [A-z0-9 ]+{eol} ) - IFF there is no {next_section}
--          (IE. if its "Chp 0 Foobar", "Foobar" is the title. Otherwise if "Chp 0 Foobar: Bar" the "Foobar" is a chapter label
--
--      {version}
--      [v] -> v[0-9]+ - If and only if followed by [0-9]+ otherwise it is {text/label of some sort}
--
--  {eol} -> End of Line
--
--  {eos} -> ( maybe [ ] | : {title} | {Chp..} | {eol}
--
--  {next_section} -> ( : {title} | {Chp..} )
--

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
