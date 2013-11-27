import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Data.Text as T
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
        return $ l:r

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

        idx          <- choose (0, 10)
        ending       <- T.pack <$> letterThenLetterOrDigit idx
        endingMaybe  <- elements [ Just ending, Nothing ]

        return $ Digit pos subMaybe verMaybe endingMaybe



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

isEmptyD :: Digit -> Bool
isEmptyD (Digit _ _ _ Nothing) = True
isEmptyD _ = False
