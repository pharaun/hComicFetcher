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
import Text.Parsec.String (Parser)
import Data.Functor.Identity (Identity)

-- Testing
import Test.HUnit

-- Local Imports
import Types

-- Site, Story, "Vol/Chp/etc" parse
volChpParse :: String -> String -> String -> ComicTag
volChpParse = undefined


--
-- Formal Grammar Definition for Vol/Chp segments
--
-- {Vol}
-- [ .]
--
-- ( {digits} | {text} )
--
--  {text} -> [A-z0-9 ]*{eof}
--
--  TODO: does not take care of "Vol 30a"
--  {digits} -> ( {single_digit} | ( {simplified_digit},{digits} | {simplified_digit}-{simplified_digit} ( {eos} | ,{digits} ) ) )
--
--      {simplified_digit} -> [0-9]+ ( {version} | {simplified_subdigit}{version}? )?
--
--          {simplified_subdigit} -> [.][0-9]+
--
--  {single_digit} -> [0-9]+ ( {version} | {version}{subdigit:[ ]} | {subdigit:[.]} | {subdigit:[.]}{version} )?
--
--      {subdigit} ->
--
--          TODO: does not take care of "Vol 30.9a"
--          [.] -> ( [0-9]+ | [A-z][A-z0-9 ]+{eos} ) - TODO: this is probably a chapter label "Chp 09.Extra 2"
--
--          [ ] -> [A-z][A-z0-9 ]+ ( {eof} -> "Title" | {eos} -> "Chapter label" )
--
--  {version} -> v[0-9]+
--
--  {eof} -> End of Line
--
--  {eos} -> ( : {title} | {Chp..} | {eof} )
--
volParse :: ParsecT String u Identity [String]
volParse = do
    -- {Vol}
    label <- choice
        -- TODO: can probably make more efficient by building up from V,Vol,Volume
        [ try $ string "Volume"
        , string "Vol"
        ]

    -- [ .]
    skipMany $ oneOf " ."

    -- ( {digits} | {text} )
    ident <- choice
        [ try digitsParse
        , textParse
        ]

    return [label, ident]


-- {text} -> [A-z0-9 ]*{eof}
textParse :: ParsecT String u Identity String
textParse = do
    rest <- many anyChar
    eof
    return rest


-- {digits} -> ( {single_digit} | ( {simplified_digit},{digits} | {simplified_digit}-{simplified_digit} ( {eos} | ,{digits} ) ) )
digitsParse :: ParsecT String u Identity String
digitsParse = undefined

-- {simplified_digit} -> [0-9]+ ( {version} | {simplified_subdigit}{version}? )?
simplifiedDigit :: ParsecT String u Identity [String]
simplifiedDigit = do
    digits <- many1 digit
    subdigits <- option "" simplifiedSubDigit -- TODO: convert to an option Maybe
    version <- option "" version -- TODO: also convert to an option maybe

    return [digits, subdigits, version]

-- {simplified_subdigit} -> [.][0-9]+
simplifiedSubDigit :: ParsecT String u Identity String
simplifiedSubDigit = do
    char '.'
    return =<< many1 digit

-- {single_digit} -> [0-9]+ ( {version} | {version}{subdigit:[ ]} | {subdigit:[.]} | {subdigit:[.]}{version} )?
singleDigit :: ParsecT String u Identity String
singleDigit = undefined

-- {subdigit} ->
--     [.] -> ( [0-9]+ | [A-z][A-z0-9 ]+{eos} ) - TODO: this is probably a chapter label "Chp 09.Extra 2"
dotSubDigit :: ParsecT String u Identity String
dotSubDigit = do
    char '.'

    subDigit <- choice
        [ many1 digit
        , (do
            firstChar <- letter
            rest <- consumeRest -- TODO: fix this, if it fails it fails, no trying {eof}

            return $ firstChar : rest
          )
        ]

    return subDigit

-- {subdigit} ->
--     [ ] -> [A-z][A-z0-9 ]+ ( {eof} -> "Title" | {eos} -> "Chapter label" )
spaceSubDigit :: ParsecT String u Identity String
spaceSubDigit = do
    char ' '

    subDigit <- choice
        [ (do
            firstChar <- letter
            rest <- consumeRest -- TODO: fix this, if it fails it fails, no trying {eof}

            return $ firstChar : rest
          )
        , (do
            firstChar <- letter
            rest <- many1 anyChar
            eof

            return $ firstChar : rest
           )
        ]
    return subDigit

-- {version} -> v[0-9]+
version :: ParsecT String u Identity String
version = do
    char 'v'
    return =<< many1 digit

-- {eos} -> ( : {title} | {Chp..} | {eof} )
eos :: ParsecT String u Identity String
eos = do
    eos <- choice
        [ (oneOf ":" >> spaces >> many1 anyChar)
        , (do
            -- TODO: extend this to attempt an valid Chp parse?
            chp  <- string "Chp"
            rest <- many1 anyChar
            return $ chp ++ rest
          )
        , string "" -- Hack to make it accept an empty string and match on eof
        ]
    eof
    return eos

-- TODO: this part is the most questionable part followed by {eos}
-- Make sure we are not followed by {eos} and keep consuming
--  [A-z][A-z0-9 ]+{eos}
--  [A-z][A-z0-9 ]+ ( {eof} -> "Title" | {eos} -> "Chapter label" )
consumeRest :: ParsecT String u Identity String
consumeRest = do
    -- Custom {eos} rules
    rest <- many $ noneOf ":Cc" -- TODO: make it general for volume, etc

    -- Verify that its not followed by : or Chp, if its not, keep consuming
    notFollowedBy eos <|> eof

    -- Consume next char then resume parsing
    nextChar <- optionMaybe anyChar

    case nextChar of
        Nothing -> return rest
        Just c  -> do
            -- Resume parsing
            nextRest <- option "" consumeRest

            return $ rest ++ [c] ++ nextRest





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


----  {text} -> [A-z][A-z0-9 ]+ {eos} - Basically anything except {eos}
--textParse :: ParsecT String u Identity String
--textParse = do
--    firstLetter <- letter
--    rest <- option "" consumeRest
--
--    return $ firstLetter : rest
--
--   where
--    -- Make sure we are not followed by {eos} and keep consuming
--    consumeRest = do
--        -- Custom {eos} rules
--        rest <- many1 $ noneOf ":Cc"
--
--        -- Verify that its not followed by : or Chp, if its not, keep consuming
--        notFollowedBy eos <|> eof
--
--        -- Consume next char then resume parsing
--        nextChar <- optionMaybe anyChar
--
--        case nextChar of
--            Nothing -> return rest
--            Just c  -> do
--                -- Resume parsing
--                nextRest <- option "" consumeRest
--
--                return $ rest ++ [c] ++ nextRest
