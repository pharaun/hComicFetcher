import Data.List
import Control.Arrow.ArrowTree
import Text.XML.HXT.Core



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

    3. Requirements?
        a. Identify what qualifies as a Volume, Chapter, Page
        b. Identify how to process/fetch "next" page, chapter, volume
        c. Identify how to download and store each to disk in a scheme that makes sense
        d. How to specifiy what to be fetched (Url of the said series?) (Site?)
            a. Typeclass
                - nextPage
                - comic
                - volume, chapter
-}


main = do
    html <- readFile "test.html"
    let doc = readString [withParseHTML yes, withWarnings no] html
    let nextPage = doc //> hasName "a" >>> hasAttrValue "class" (isInfixOf "navi-next") >>> hasAttr "href"

    let comic = doc //> hasAttrValue "id" (== "comic") >>> hasName "div" //> hasName "img" >>> hasAttr "src"

    -- Image
    img <- runX $ comic >>> getAttrValue "src"
    mapM_ putStrLn img

    -- Navi
    next <- runX $ nextPage >>> getAttrValue "href"
    mapM_ putStrLn next
