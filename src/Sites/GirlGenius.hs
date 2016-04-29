module Sites.GirlGenius
    ( girlGenius
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.List (isInfixOf, isPrefixOf)
import qualified Data.List as DL

import Text.XML.HXT.Core

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as UL
import qualified Data.ByteString.UTF8 as US

-- Local imports
import Types
import Sites.Util

--
-- Girl Genius
--
girlGenius = Comic
    { comicName = "Girl Genius"
    , seedPage = "http://www.girlgeniusonline.com/comic.php?date=20021104"
    , seedType = undefined
    , seedCache = Always
    , pageParse = toPipeline girlGeniusPageParse
    , cookies = []
    }

girlGeniusPageParse :: ReplyType t -> IO [FetchType t]
girlGeniusPageParse (WebpageReply html _) = do
    let doc = readString [withParseHTML yes, withWarnings no] $ UL.toString html
    next <- runX $ doc //> nextPage
    img <- runX $ doc //> comic
    vol <- concat `fmap` runX (whichVol doc)

    -- Do we have any comic we want to store to disk?
    putStrLn "Fetched Urls:"
    mapM_ putStrLn img
    mapM_ putStrLn next

    return $ map (\a -> Webpage a Always undefined) next ++ map (\a -> Image a $ comicFileName vol a) img

   where
    nextPage = hasName "td" >>> hasAttrValue "valign" (== "top") //> (hasName "a" </ (hasName "img" >>> hasAttrValue "alt" (== "The Next Comic"))) >>> getAttrValue "href"
    comic =
        hasName "td"
        >>> hasAttrValue "valign" (== "middle")
        //> hasName "img"
        >>> hasAttr "src"
        >>> getAttrValue "src"
        >>. arr (filter (isPrefixOf "http"))

    comicFileName vol url = ComicTag (T.pack "girl_genius") Nothing (Just $ UnitTag [StandAlone $ Digit (girlGeniusVol vol) Nothing Nothing Nothing] Nothing) Nothing (Just $ last $ decodePathSegments $ US.fromString url)

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
        >>. filter (\(a, b) -> a || ((("VOLUME" `isInfixOf` b) || ("Volume" `isInfixOf` b)) && not (("Final Page" `isPrefixOf` b) || ("Wallpaper" `isInfixOf` b))))
        ) >. takeWhile (not . fst)
        >>> unlistA
        >>> arr snd
        -- ) >>> arr ((SL.split . SL.keepDelimsL . SL.whenElt) (isPrefixOf "Chapter")) -- TODO: this is for splitting things up

    girlGeniusVol :: String -> Integer
    girlGeniusVol a = wordToNumber (DL.reverse $ DL.head $ DL.words $ DL.drop 3 $ DL.reverse a)
