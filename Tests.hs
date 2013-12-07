import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Data.Text as T
import qualified Data.List as DL
import Data.String (fromString)
import Text.Parsec (parse)
import Text.Parsec.Text

import Test.QuickCheck
import Test.QuickCheck.Modifiers

-- Local types for quickchecking
import Types

-- Parser to test
import Sites.Util
import Fetcher


-- Quickcheck instances
alphaFreqList =
    [ (26, choose ('a', 'z'))
    , (26, choose ('A', 'Z'))
    ]
digitFreqList = [ (10, choose ('0', '9')) ]

letter = frequency alphaFreqList
letterOrDigit = frequency $ alphaFreqList ++ digitFreqList

letterThenLetterOrDigit n
    | n <= 0    = elements [""]
    | n == 1    = (:[]) <$> letter
    | otherwise = do
        l <- letter
        r <- replicateM (n - 1) letterOrDigit
        return $ removeVersion $ l:r

instance Arbitrary T.Text where
    arbitrary = fromString <$> (arbitrary :: Gen String)

instance Arbitrary SubDigit where
    arbitrary = do
        Positive pos <- arbitrary
        posMaybe     <- elements [ Just pos, Nothing ]

        idx    <- choose (0, 10)
        ending <- T.pack <$> letterThenLetterOrDigit idx

        return $ DotSubDigit posMaybe ending

instance Arbitrary Digit where
    arbitrary = do
        Positive pos <- arbitrary

        sub <- arbitrary
        subMaybe <- elements [ Just sub, Nothing ]

        Positive ver <- arbitrary
        verMaybe     <- elements [ Just ver, Nothing ]

        idx          <- choose (1, 10)
        ending       <- T.pack <$> letterThenLetterOrDigit idx
        endingMaybe  <- elements [ Just ending, Nothing ]

        return $ Digit pos subMaybe verMaybe endingMaybe


instance Arbitrary Digits where
    arbitrary = do
        first <- arbitrary

        second <- arbitrary
        secondMaybe <- elements [ Just second, Nothing ]

        return $ case secondMaybe of
            Nothing -> StandAlone first
            Just  _ -> RangeDigit first second


-- Quickcheck main/options
main =
    mapM_ (quickCheckWith options)
        [ property $ checkSimplifiedSubDigit
        , property $ checkDotSubDigit
        , property $ checkSingleDigit
        , property $ checkSimplifiedDigit
        , property $ checkSingleDigits
        , property $ checkListDigits
        ]
    where
        options = stdArgs { maxSuccess = 1000 }



-- Checks
checkSimplifiedSubDigit ast =
    case (parse simplifiedSubDigit "" (formatSubDigit $ empty $ ast)) of
        Left _  ->  if (isEmpty $ empty ast)
                    then True
                    else False
        Right a -> (empty ast) == a

checkDotSubDigit ast =
    case (parse dotSubDigit "" (formatSubDigit ast)) of
        Left _  ->  if isEmpty ast
                    then True
                    else False
        Right a -> ast == a

checkSingleDigit ast =
    case (parse singleDigit "" (formatDigit $ emptyD ast)) of
        Left _  ->  if (isEmptyD $ emptyD ast)
                    then True
                    else False
        Right a -> (emptyD ast) == a

checkSimplifiedDigit ast =
    case (parse simplifiedDigit "" (formatDigit $ emptySD ast)) of
        Left _  ->  if (isEmptyD $ emptySD ast)
                    then True
                    else False
        Right a -> (emptySD ast) == a

checkSingleDigits ast =
    case (parse digitsParse "" (formatDigits $ cleanRangeDigit ast)) of
        Left _  ->  False
        Right a -> [cleanRangeDigit ast] == a


-- TODO: Need to treat the last element specially but for now this will do
checkListDigits (NonEmpty ast) =
    case (parse digitsParse "" (formatListDigits $ map strictCleanRangeDigit ast)) of
        Left _  ->  False
        Right a -> (map strictCleanRangeDigit ast) == a




-- Util
empty :: SubDigit -> SubDigit
empty (DotSubDigit a _) = DotSubDigit a T.empty

isEmpty :: SubDigit -> Bool
isEmpty (DotSubDigit Nothing a) = T.null a
isEmpty _ = False


emptyD :: Digit -> Digit
emptyD (Digit a Nothing c _) = Digit a Nothing c Nothing
emptyD (Digit a (Just (DotSubDigit Nothing _)) c _) = Digit a Nothing c Nothing
emptyD (Digit a (Just b@(DotSubDigit (Just _) _)) c _) = Digit a (Just $ empty b) c Nothing

emptySD :: Digit -> Digit
emptySD (Digit a (Just (DotSubDigit Nothing _)) c d) = Digit a Nothing c d
emptySD (Digit a (Just b@(DotSubDigit (Just _) _)) c d) = Digit a (Just $ empty b) c d
emptySD (Digit a Nothing c d) = Digit a Nothing c d


isEmptyD :: Digit -> Bool
isEmptyD (Digit _ _ _ Nothing) = True
isEmptyD _ = False

-- Cheap lazy way of replacing all v[0-9]
removeVersion :: String -> String
removeVersion s = T.unpack $ DL.foldl cleaner (T.pack s) cleanerList
    where
        cleaner :: T.Text -> T.Text -> T.Text
        cleaner a b = T.replace b (T.pack "a9") b

        cleanerList :: [T.Text]
        cleanerList = map T.pack $ DL.concatMap (\a -> DL.transpose [DL.replicate 10 a, "0123456789"]) "Vv"


cleanRangeDigit :: Digits -> Digits
cleanRangeDigit (StandAlone a)   = StandAlone $ secondDigit a
cleanRangeDigit (RangeDigit a b) = RangeDigit (firstDigit a) (secondDigit b)

firstDigit :: Digit -> Digit
firstDigit (Digit a Nothing b _) = Digit a Nothing b Nothing
firstDigit (Digit a (Just (DotSubDigit Nothing _)) b _) = Digit a Nothing b Nothing
firstDigit (Digit a (Just (DotSubDigit (Just c) _)) b _) = Digit a (Just $ DotSubDigit (Just c) T.empty) b Nothing

secondDigit :: Digit -> Digit
secondDigit (Digit a Nothing b c) = Digit a Nothing b c
secondDigit (Digit a (Just (DotSubDigit Nothing _)) b c) = Digit a Nothing b c
secondDigit (Digit a (Just (DotSubDigit (Just d) _)) b c) = Digit a (Just $ DotSubDigit (Just d) T.empty) b c

strictCleanRangeDigit :: Digits -> Digits
strictCleanRangeDigit (StandAlone a)   = StandAlone $ firstDigit a
strictCleanRangeDigit (RangeDigit a b) = RangeDigit (firstDigit a) (firstDigit b)


---- Int: 29 subdigit: .9 Int: v1 Text: a -> 29.9v1a
--data Digit = Digit Integer (Maybe SubDigit) (Maybe Integer) (Maybe T.Text)
--           deriving (Show, Eq)
--
---- Sub Digits
--data SubDigit = DotSubDigit (Maybe Integer) T.Text
--              deriving (Show, Eq)
