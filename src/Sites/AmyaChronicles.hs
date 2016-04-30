{-# LANGUAGE OverloadedStrings #-}
module Sites.AmyaChronicles
    ( amyaChronicles
    ) where

import Network.HTTP.Types.URI (decodePathSegments)

import Data.Maybe (maybeToList, listToMaybe)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.UTF8 as US

-- Taggy
import Control.Lens (only,(^?), folded, _Just, (^..), (^.))
import Text.Taggy.Lens hiding (name)

-- Parser
import Text.Parsec
import Data.Functor.Identity (Identity)
import Control.Applicative (liftA)

-- Local imports
import Types
import Sites.Util (toPipeline)

--
-- Amya Chronicles - Testing Taggy parsing
--
amyaChronicles = Comic
    { comicName = "Amya Chronicles"
    , seedPage = "http://www.amyachronicles.com/archives/comic/09292009"
    , seedCache = Always
    , pageParse = toPipeline amyaChroniclesPageParse
    , cookies = []
    }

amyaChroniclesPageParse :: ReplyType -> IO [FetchType]
amyaChroniclesPageParse (WebpageReply pg) = do
    let text = foldl (\c (a, b) -> TL.replace a b c) (TLE.decodeUtf8With lenientDecode pg)
            [ (">>", ">&gt;")
            , ("<<", "^lt;<")
            , ("< ", "&lt; ")
            -- , (" >", " &gt;") - Breaks the image match
            ]

    -- Next Page Link
    -- TODO: make it so it can match on only 'comic-nav-next'
    -- TODO: make it so that it only matches "class" attr
    let next = text ^? html . allAttributed (folded . only "comic-nav-base comic-nav-next") . attr "href" . _Just

    -- Page Name
    let name = text ^. html . allAttributed (folded . only "post-title") . contents
    print name

    -- Search for double page image link
    -- TODO: make it actually verify that its a link to
    --      "THIS IS A DOUBLE PAGE SPREAD, CLICK HERE FOR FULL IMAGE!"
    let img = case (listToMaybe $ filter (T.isSuffixOf "jpg") (text ^.. html . allNamed (only "strong") . elements . attr "href" . _Just)) of
            Nothing -> text ^. html . allAttributed (folded . only "comic") . allNamed (only "img") . attr "src" . _Just
            Just x  -> x

    -- Parse the title and create the tag.
    case parseTitle name img of
        Left _  -> return $ map (\url -> Webpage (T.unpack url) Always) $ maybeToList next
        Right x -> return $ [Image (T.unpack img) x] ++ (map (\url -> Webpage (T.unpack url) Always) $ maybeToList next)

    -- Fetching next page


parseTitle name url = runParser (titles url) () "" name

titles :: T.Text -> ParsecT T.Text u Identity ComicTag
titles url = choice
    [ try (do
        chp <- numParse
        _ <- char '.'
        _ <- numParse -- pg
        eof
        return $ mainStory chp url)

    , try (do
        chp <- numParse
        _ <- char '.'
        _ <- numParse -- pg
        _ <- space
        _ <- char '–'
        _ <- space
        _ <- numParse -- chp'
        _ <- char '.'
        _ <- numParse -- pg'
        eof
        return $ mainStory chp url)

    , try (do
        name <- wordParse
        _ <- space
        _ <- numParse
        eof
        return $ shortStory name url)

    , (return $ artStory url)
    ]

numParse :: ParsecT T.Text u Identity Integer
numParse = liftA read (many1 digit)

wordParse :: ParsecT T.Text u Identity T.Text
wordParse = liftA T.pack (many1 letter)


-- Comic Tag
mainStory chp url = ComicTag "amya_chronicles" Nothing Nothing (Just $ UnitTag [StandAlone $ Digit chp Nothing Nothing Nothing] Nothing) (Just $ last $ decodePathSegments $ US.fromString $ T.unpack url)

shortStory storyName url = ComicTag "amya_chronicles" (Just storyName) Nothing Nothing (Just $ last $ decodePathSegments $ US.fromString $ T.unpack url)

artStory url = ComicTag "amya_chronicles" (Just "artwork") Nothing Nothing (Just $ last $ decodePathSegments $ US.fromString $ T.unpack url)
